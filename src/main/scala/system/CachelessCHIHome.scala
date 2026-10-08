package system

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xscache.chi._
import xscache.openLLC.NoSnpPortIO

/** Finite transaction resources, not a line cache or a residency directory.
  * Ordinary and release slots are physically disjoint: reads cannot consume
  * the resources needed to return victims and complete their own CompAck.
  */
case class CachelessCHIHomeParams(ordinaryEntries: Int, releaseEntries: Int,
                                  downstreamEntries: Int = 64, linkCredits: Int = 15) {
  require(ordinaryEntries > 0 && releaseEntries > 0 && downstreamEntries > 0)
  require(linkCredits > 0 && linkCredits <= 15)
  val entries: Int = ordinaryEntries + releaseEntries
}

/** One RN-F port aggregates all banks of one inclusive L2.  There are no
  * other coherent requesters or DMA masters in this topology.  Consequently
  * local permission/CMO completions require no snoop and coherent reads can
  * grant UC.  Dirty contents always travel from the L2 to the real SN/NCB.
  *
  * The SN must use EWA=0 and no early write completion: DBIDResp grants data,
  * Comp arrives only after AXI B.  This module preserves that distinction.
  * An address chain serializes accepted transactions on the same 64 B line,
  * including the RN CompAck lifetime.  Only transaction metadata is retained;
  * data flows through finite channel queues and is never a resident line.
  */
class CachelessCHIHomeCore(val capacity: CachelessCHIHomeParams)(implicit val p: Parameters)
    extends Module with HasCHIOpcodes {
  private val n = capacity.entries
  private val idBits = log2Ceil(n)
  require(idBits <= TXNID_WIDTH)
  require(DATA_WIDTH == 256, "this L2/NCB integration has two 256-bit beats per line")
  val io = IO(new Bundle {
    val rn = Flipped(new DecoupledPortIO)
    val sn = new DecoupledNoSnpPortIO
    val nodeID = Input(UInt(NODEID_WIDTH.W))
    val snNodeID = Input(UInt(NODEID_WIDTH.W))
    val active = Output(UInt(log2Ceil(n + 1).W))
    val ordinaryActive = Output(UInt(log2Ceil(capacity.ordinaryEntries + 1).W))
    val releaseActive = Output(UInt(log2Ceil(capacity.releaseEntries + 1).W))
    val blockedAdmissions = Output(UInt(32.W))
    val dependentTransactions = Output(UInt(log2Ceil(n + 1).W))
  })

  def isRead(op: UInt): Bool = op === ReadShared || op === ReadClean || op === ReadOnce ||
    op === ReadNoSnp || op === ReadUnique || op === ReadNotSharedDirty
  def isCopyback(op: UInt): Bool = op === WriteBackFull || op === WriteBackPtl ||
    op === WriteCleanFull || op === WriteEvictFull
  def isWrite(op: UInt): Bool = isCopyback(op) || op === WriteNoSnpFull || op === WriteNoSnpPtl ||
    op === WriteUniqueFull || op === WriteUniquePtl
  def isEvict(op: UInt): Bool = op === Evict || afterIssueEbOrElse(op === WriteEvictOrEvict, false.B)
  def isLocal(op: UInt): Bool = isEvict(op) || op === MakeUnique || op === CleanUnique ||
    op === CleanShared || op === CleanInvalid || op === MakeInvalid
  def isRelease(op: UInt): Bool = isCopyback(op) || isEvict(op)
  def line(addr: UInt): UInt = addr(ADDR_WIDTH - 1, 6)
  def expectedBeats(req: CHIREQ): UInt = Mux(req.size === 6.U, 3.U(2.W),
    Mux(req.addr(5), 2.U(2.W), 1.U(2.W)))

  val valid = RegInit(VecInit(Seq.fill(n)(false.B)))
  val req = Reg(Vec(n, new CHIREQ))
  val waitValid = RegInit(VecInit(Seq.fill(n)(false.B)))
  val waitID = Reg(Vec(n, UInt(idBits.W)))
  val hasFollower = RegInit(VecInit(Seq.fill(n)(false.B)))
  val started = RegInit(VecInit(Seq.fill(n)(false.B)))
  val snDone = RegInit(VecInit(Seq.fill(n)(false.B)))
  val dbidValid = RegInit(VecInit(Seq.fill(n)(false.B)))
  val snDBID = Reg(Vec(n, UInt(DBID_WIDTH.W)))
  val snSrcID = Reg(Vec(n, UInt(NODEID_WIDTH.W)))
  val rnDBIDSent = RegInit(VecInit(Seq.fill(n)(false.B)))
  val rnCompSent = RegInit(VecInit(Seq.fill(n)(false.B)))
  val receiptSent = RegInit(VecInit(Seq.fill(n)(false.B)))
  val acked = RegInit(VecInit(Seq.fill(n)(false.B)))
  val readBeats = RegInit(VecInit(Seq.fill(n)(0.U(2.W))))
  val writeBeats = RegInit(VecInit(Seq.fill(n)(0.U(2.W))))
  val error = RegInit(VecInit(Seq.fill(n)(0.U(RESPERR_WIDTH.W))))
  val retire = Wire(Vec(n, Bool()))

  // A single accepted Device/ordered operation is a global ordering barrier.
  // Snapshot only the older live slot bits; younger victim releases may still
  // enter and progress, so the barrier cannot consume their reserved resources.
  // This is transient ordering metadata, not an address residency directory.
  val barrierValid = RegInit(false.B)
  val barrierID = Reg(UInt(idBits.W))
  val barrierOlder = RegInit(0.U(n.W))
  val admissionOrdered = io.rn.tx.req.bits.memAttr.device ||
    io.rn.tx.req.bits.order >= OrderEncodings.RequestOrder
  val barrierPending = Wire(Vec(n, Bool()))
  for (i <- 0 until n) barrierPending(i) := barrierValid && barrierID === i.U && barrierOlder.orR
  when(barrierValid) {
    barrierOlder := barrierOlder & ~retire.asUInt
    when(retire(barrierID)) { barrierValid := false.B }
  }

  val admissionRelease = isRelease(io.rn.tx.req.bits.opcode)
  val free = VecInit((0 until n).map(i => !valid(i) &&
    (if (i < capacity.ordinaryEntries) !admissionRelease else admissionRelease)))
  val allocID = PriorityEncoder(free)
  val tails = VecInit((0 until n).map(i => valid(i) && !hasFollower(i) &&
    line(req(i).addr) === line(io.rn.tx.req.bits.addr)))
  val predecessor = PriorityEncoder(tails)
  val supported = isRead(io.rn.tx.req.bits.opcode) || isWrite(io.rn.tx.req.bits.opcode) ||
    isLocal(io.rn.tx.req.bits.opcode)
  io.rn.tx.req.ready := free.asUInt.orR && supported &&
    (!barrierValid || (admissionRelease && !admissionOrdered))
  when(io.rn.tx.req.valid) {
    assert(supported, "unsupported CHI request in the single-requester cacheless topology")
    assert(io.rn.tx.req.bits.size <= 6.U, "request exceeds the 64-byte L2 line")
    assert(PopCount(tails) <= 1.U, "same-address chain must have exactly one tail")
  }
  when(io.rn.tx.req.fire) {
    valid(allocID) := true.B
    req(allocID) := io.rn.tx.req.bits
    waitValid(allocID) := tails.asUInt.orR && !retire(predecessor)
    waitID(allocID) := predecessor
    hasFollower(allocID) := false.B
    when(tails.asUInt.orR) { hasFollower(predecessor) := true.B }
    started(allocID) := false.B
    snDone(allocID) := false.B
    dbidValid(allocID) := false.B
    rnDBIDSent(allocID) := false.B
    rnCompSent(allocID) := false.B
    receiptSent(allocID) := false.B
    acked(allocID) := false.B
    readBeats(allocID) := 0.U
    writeBeats(allocID) := 0.U
    error(allocID) := 0.U
    when(admissionOrdered) {
      barrierValid := true.B
      barrierID := allocID
      barrierOlder := valid.asUInt & ~retire.asUInt
    }
  }

  val downstreamActive = PopCount((0 until n).map(i => valid(i) && started(i) && !snDone(i)))
  val reqArb = Module(new RRArbiter(new CHIREQ, n))
  for (i <- 0 until n) {
    reqArb.io.in(i).valid := valid(i) && !waitValid(i) && !barrierPending(i) && !started(i) &&
      (isRead(req(i).opcode) || isWrite(req(i).opcode)) &&
      downstreamActive < capacity.downstreamEntries.U
    reqArb.io.in(i).bits := req(i)
    reqArb.io.in(i).bits.srcID := io.nodeID
    reqArb.io.in(i).bits.tgtID := io.snNodeID
    reqArb.io.in(i).bits.txnID := i.U
    reqArb.io.in(i).bits.returnNID := io.nodeID
    reqArb.io.in(i).bits.returnTxnID := i.U
    reqArb.io.in(i).bits.opcode := Mux(isRead(req(i).opcode), ReadNoSnp, WriteNoSnpPtl)
    // OpenNCB accepts only first requests: AllowRetry=1 and PCrdType=0.
    // Its transaction L-credits bound admission; it never emits RetryAck/PCrdGrant.
    reqArb.io.in(i).bits.allowRetry := true.B
    reqArb.io.in(i).bits.expCompAck := false.B
    reqArb.io.in(i).bits.order := OrderEncodings.None
    reqArb.io.in(i).bits.pCrdType := 0.U
    reqArb.io.in(i).bits.snpAttr := false.B
    reqArb.io.in(i).bits.snoopMe := false.B
    reqArb.io.in(i).bits.memAttr := MemAttr(false.B, false.B, req(i).memAttr.device, false.B)
    when(reqArb.io.in(i).fire) { started(i) := true.B }
  }
  io.sn.tx.req <> Queue(reqArb.io.out, 2, pipe = true)

  val rspArb = Module(new RRArbiter(new CHIRSP, n))
  for (i <- 0 until n) {
    val grantDBID = isWrite(req(i).opcode) && dbidValid(i) && !rnDBIDSent(i)
    val localComp = isLocal(req(i).opcode) && !rnCompSent(i)
    val writeComp = isWrite(req(i).opcode) && snDone(i) && rnDBIDSent(i) && !rnCompSent(i)
    val readReceipt = isRead(req(i).opcode) && started(i) && req(i).order =/= OrderEncodings.None && !receiptSent(i)
    rspArb.io.in(i).valid := valid(i) && !waitValid(i) && !barrierPending(i) &&
      (grantDBID || localComp || writeComp || readReceipt)
    rspArb.io.in(i).bits := 0.U.asTypeOf(new CHIRSP)
    rspArb.io.in(i).bits.qos := req(i).qos
    rspArb.io.in(i).bits.srcID := io.nodeID
    rspArb.io.in(i).bits.tgtID := req(i).srcID
    rspArb.io.in(i).bits.txnID := req(i).txnID
    rspArb.io.in(i).bits.dbID := i.U
    rspArb.io.in(i).bits.opcode := Mux(grantDBID, DBIDResp, Mux(readReceipt, ReadReceipt, Comp))
    rspArb.io.in(i).bits.resp := Mux(req(i).opcode === MakeUnique || req(i).opcode === CleanUnique,
      CHICohStates.UC, CHICohStates.I)
    rspArb.io.in(i).bits.respErr := error(i)
    rspArb.io.in(i).bits.traceTag := req(i).traceTag
    when(rspArb.io.in(i).fire) {
      when(grantDBID) { rnDBIDSent(i) := true.B }
        .elsewhen(readReceipt) { receiptSent(i) := true.B }
        .otherwise { rnCompSent(i) := true.B }
    }
  }
  io.rn.rx.rsp <> Queue(rspArb.io.out, 2, pipe = true)
  io.rn.rx.snp.valid := false.B
  io.rn.rx.snp.bits := 0.U.asTypeOf(new CHISNP)

  // SN data is delivered to the original full RN TxnID (which includes bank bits).
  val readID = io.sn.rx.dat.bits.txnID(idBits - 1, 0)
  val readInRange = io.sn.rx.dat.bits.txnID < n.U
  val readMatch = readInRange && valid(readID) && started(readID) && isRead(req(readID).opcode)
  val rnData = Wire(Decoupled(new CHIDAT))
  rnData.valid := io.sn.rx.dat.valid && readMatch
  rnData.bits := io.sn.rx.dat.bits
  rnData.bits.srcID := io.nodeID
  rnData.bits.tgtID := req(readID).srcID
  rnData.bits.txnID := req(readID).txnID
  rnData.bits.homeNID := io.nodeID
  rnData.bits.dbID := readID
  rnData.bits.resp := Mux(req(readID).opcode === ReadNoSnp, CHICohStates.I, CHICohStates.UC)
  io.rn.rx.dat <> Queue(rnData, 2, pipe = true)
  io.sn.rx.dat.ready := rnData.ready && readMatch
  when(io.sn.rx.dat.valid) {
    assert(readMatch, "SN CompData has no active read transaction")
    assert(io.sn.rx.dat.bits.opcode === CompData, "NCB must return combined CompData")
    assert(io.sn.rx.dat.bits.srcID === io.snNodeID && io.sn.rx.dat.bits.tgtID === io.nodeID,
      "SN CompData node ID mismatch")
    assert(!io.sn.rx.dat.bits.dataID(0), "256-bit CHI uses DataID 0 or 2")
  }
  when(io.sn.rx.dat.fire) {
    val beat = (1.U(2.W) << io.sn.rx.dat.bits.dataID(1))(1, 0)
    assert((readBeats(readID) & beat) === 0.U, "duplicate read beat")
    assert((expectedBeats(req(readID)) & beat) =/= 0.U, "unexpected read beat")
    readBeats(readID) := readBeats(readID) | beat
    when((readBeats(readID) | beat) === expectedBeats(req(readID))) { snDone(readID) := true.B }
  }

  // An upstream DBID is a home slot; the downstream DBID is allocated by NCB.
  val writeID = io.rn.tx.dat.bits.txnID(idBits - 1, 0)
  val writeMatch = io.rn.tx.dat.bits.txnID < n.U && valid(writeID) &&
    isWrite(req(writeID).opcode) && dbidValid(writeID) && rnDBIDSent(writeID)
  val snData = Wire(Decoupled(new CHIDAT))
  snData.valid := io.rn.tx.dat.valid && writeMatch
  snData.bits := io.rn.tx.dat.bits
  snData.bits.srcID := io.nodeID
  snData.bits.tgtID := snSrcID(writeID)
  snData.bits.txnID := snDBID(writeID)
  snData.bits.opcode := NonCopyBackWrData
  snData.bits.resp := 0.U
  snData.bits.respErr := 0.U
  snData.bits.dbID := 0.U
  snData.bits.homeNID := 0.U
  io.sn.tx.dat <> Queue(snData, 2, pipe = true)
  io.rn.tx.dat.ready := snData.ready && writeMatch
  when(io.rn.tx.dat.valid) {
    assert(writeMatch, "RN write data does not match an issued home DBID")
    assert(io.rn.tx.dat.bits.srcID === req(writeID).srcID && io.rn.tx.dat.bits.tgtID === io.nodeID,
      "RN write data node ID mismatch")
    assert(io.rn.tx.dat.bits.opcode === Mux(isCopyback(req(writeID).opcode), CopyBackWrData, NonCopyBackWrData))
    assert(!io.rn.tx.dat.bits.dataID(0), "256-bit CHI uses DataID 0 or 2")
  }
  when(io.rn.tx.dat.fire) {
    val beat = (1.U(2.W) << io.rn.tx.dat.bits.dataID(1))(1, 0)
    assert((writeBeats(writeID) & beat) === 0.U, "duplicate write beat")
    assert((expectedBeats(req(writeID)) & beat) =/= 0.U, "unexpected write beat")
    writeBeats(writeID) := writeBeats(writeID) | beat
  }

  val snRspID = io.sn.rx.rsp.bits.txnID(idBits - 1, 0)
  val snRspMatch = io.sn.rx.rsp.bits.txnID < n.U && valid(snRspID) && started(snRspID) &&
    isWrite(req(snRspID).opcode)
  io.sn.rx.rsp.ready := true.B
  when(io.sn.rx.rsp.fire) {
    assert(snRspMatch, "SN response has no active write transaction")
    assert(io.sn.rx.rsp.bits.srcID === io.snNodeID && io.sn.rx.rsp.bits.tgtID === io.nodeID,
      "SN response node ID mismatch")
    assert(io.sn.rx.rsp.bits.opcode === DBIDResp || io.sn.rx.rsp.bits.opcode === Comp,
      "cacheless SN must separate DBID credit and endpoint Comp")
    when(io.sn.rx.rsp.bits.opcode === DBIDResp) {
      assert(!dbidValid(snRspID), "duplicate downstream DBID")
      dbidValid(snRspID) := true.B
      snDBID(snRspID) := io.sn.rx.rsp.bits.dbID
      snSrcID(snRspID) := io.sn.rx.rsp.bits.srcID
    }.otherwise {
      assert(writeBeats(snRspID) === expectedBeats(req(snRspID)), "write Comp before all data beats")
      assert(!snDone(snRspID), "duplicate downstream Comp")
      snDone(snRspID) := true.B
      error(snRspID) := io.sn.rx.rsp.bits.respErr
    }
  }

  val ackID = io.rn.tx.rsp.bits.txnID(idBits - 1, 0)
  io.rn.tx.rsp.ready := true.B
  when(io.rn.tx.rsp.fire) {
    assert(io.rn.tx.rsp.bits.opcode === CompAck, "unexpected RN response without snoops/retries")
    assert(io.rn.tx.rsp.bits.txnID < n.U && valid(ackID) && req(ackID).expCompAck,
      "RN CompAck has no matching expected transaction")
    assert(io.rn.tx.rsp.bits.srcID === req(ackID).srcID, "CompAck source ID mismatch")
    assert(io.rn.tx.rsp.bits.tgtID === io.nodeID, "CompAck target ID mismatch")
    assert(!acked(ackID), "duplicate RN CompAck")
    acked(ackID) := true.B
  }

  for (i <- 0 until n) {
    val readDone = isRead(req(i).opcode) && snDone(i) &&
      (req(i).order === OrderEncodings.None || receiptSent(i))
    val writeDone = isWrite(req(i).opcode) && snDone(i) && rnCompSent(i) &&
      writeBeats(i) === expectedBeats(req(i))
    val localDone = isLocal(req(i).opcode) && rnCompSent(i)
    retire(i) := valid(i) && (readDone || writeDone || localDone) &&
      (!req(i).expCompAck || acked(i))
    when(retire(i)) { valid(i) := false.B }
    when(valid(i) && waitValid(i) && retire(waitID(i))) { waitValid(i) := false.B }
  }
  val blocked = RegInit(0.U(32.W))
  when(io.rn.tx.req.valid && !io.rn.tx.req.ready) { blocked := blocked + 1.U }
  io.blockedAdmissions := blocked
  io.active := PopCount(valid)
  io.ordinaryActive := PopCount(valid.take(capacity.ordinaryEntries))
  io.releaseActive := PopCount(valid.drop(capacity.ordinaryEntries))
  io.dependentTransactions := PopCount((0 until n).map(i => valid(i) && (waitValid(i) || barrierPending(i))))
  assert(downstreamActive <= capacity.downstreamEntries.U)
}

/** Existing credit-based CHI links, with bounded credit buffers on each RX. */
class CachelessCHIHome(val capacity: CachelessCHIHomeParams)(implicit val p: Parameters)
    extends Module with HasCHIMsgParameters {
  val io = IO(new Bundle {
    val rn = Flipped(new PortIO)
    val sn = new NoSnpPortIO
    val nodeID = Input(UInt(NODEID_WIDTH.W))
    val snNodeID = Input(UInt(NODEID_WIDTH.W))
  })
  val core = Module(new CachelessCHIHomeCore(capacity))
  core.io.nodeID := io.nodeID
  core.io.snNodeID := io.snNodeID
  def state(req: Bool, ack: Bool): LinkState = LinkState(RegNext(MuxLookup(Cat(req, ack), LinkStates.STOP)(Seq(
    "b10".U -> LinkStates.ACTIVATE, "b11".U -> LinkStates.RUN, "b01".U -> LinkStates.DEACTIVATE)), LinkStates.STOP))
  val rnRXState = state(io.rn.tx.linkactivereq, io.rn.tx.linkactiveack)
  val rnTXState = state(io.rn.rx.linkactivereq, io.rn.rx.linkactiveack)
  val snRXState = state(io.sn.rx.linkactivereq, io.sn.rx.linkactiveack)
  val snTXState = state(io.sn.tx.linkactivereq, io.sn.tx.linkactiveack)
  val rnReqReclaim, rnRspReclaim, rnDatReclaim, snRspReclaim, snDatReclaim = Wire(Bool())
  LCredit2Decoupled(io.rn.tx.req, core.io.rn.tx.req, rnRXState, rnReqReclaim, Some("home_rn_req"), capacity.linkCredits)
  LCredit2Decoupled(io.rn.tx.rsp, core.io.rn.tx.rsp, rnRXState, rnRspReclaim, Some("home_rn_rsp"), capacity.linkCredits)
  LCredit2Decoupled(io.rn.tx.dat, core.io.rn.tx.dat, rnRXState, rnDatReclaim, Some("home_rn_dat"), capacity.linkCredits)
  Decoupled2LCredit(core.io.rn.rx.rsp, io.rn.rx.rsp, rnTXState, Some("home_rn_rsp"))
  Decoupled2LCredit(core.io.rn.rx.dat, io.rn.rx.dat, rnTXState, Some("home_rn_dat"))
  Decoupled2LCredit(core.io.rn.rx.snp, io.rn.rx.snp, rnTXState, Some("home_rn_snp"))
  Decoupled2LCredit(core.io.sn.tx.req, io.sn.tx.req, snTXState, Some("home_sn_req"))
  Decoupled2LCredit(core.io.sn.tx.dat, io.sn.tx.dat, snTXState, Some("home_sn_dat"))
  LCredit2Decoupled(io.sn.rx.rsp, core.io.sn.rx.rsp, snRXState, snRspReclaim, Some("home_sn_rsp"), capacity.linkCredits)
  LCredit2Decoupled(io.sn.rx.dat, core.io.sn.rx.dat, snRXState, snDatReclaim, Some("home_sn_dat"), capacity.linkCredits)
  io.rn.rxsactive := true.B
  io.rn.syscoack := RegNext(io.rn.syscoreq, false.B)
  io.rn.rx.linkactivereq := !reset.asBool
  io.rn.tx.linkactiveack := RegNext(io.rn.tx.linkactivereq, false.B) || !(rnReqReclaim && rnRspReclaim && rnDatReclaim)
  io.sn.txsactive := true.B
  io.sn.tx.linkactivereq := !reset.asBool
  io.sn.rx.linkactiveack := RegNext(io.sn.rx.linkactivereq, false.B) || !(snRspReclaim && snDatReclaim)
}
