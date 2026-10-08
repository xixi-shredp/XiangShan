package system

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.UnlocatableSourceInfo
import freechips.rocketchip.diplomacy.{AddressSet, IdRange, RegionType, TransferSizes}
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink._
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xscache.chi._
import xscache.common.BankBitsKey
import xscache.coupledL2.{EdgeInKey, EdgeOutKey, L2Param, L2ParamKey, MSHR, MetaData, SeparateWriteCompletion}

/** Drive the actual synthesizable protocol core, including real channel queues.
  * The SN test driver models distinct data-credit and endpoint-completion events;
  * the SoC continues to instantiate its existing NCB/AXI/DRAM implementation.
  */
class CachelessCHIHomeTest extends AnyFlatSpec with ChiselSim with HasCHIOpcodes {
  implicit lazy val p: Parameters = Parameters.empty.alterPartial {
    case CHIIssue => Issue.Eb
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false)
  }
  private val capacity = CachelessCHIHomeParams(4, 2, 2, 4)
  private val home = 2
  private val sn = 7
  private val rn = 1

  private def mshrParams(split: Boolean): Parameters = {
    val client = TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
      name = "dcache", sourceId = IdRange(0, 16), supportsProbe = TransferSizes(64))))
    val manager = TLSlavePortParameters.v1(Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(0, (BigInt(1) << 48) - 1)), regionType = RegionType.TRACKED,
      supportsAcquireT = TransferSizes(64), supportsAcquireB = TransferSizes(64),
      supportsGet = TransferSizes(1, 64), supportsPutFull = TransferSizes(1, 64),
      supportsPutPartial = TransferSizes(1, 64))), beatBytes = 32, endSinkId = 16)
    val base = p.alterPartial {
      case L2ParamKey => L2Param(supportSeparateWriteCompletion = split, enablePerf = false,
        enableRollingDB = false, elaboratedTopDown = false)
      case BankBitsKey => 0
      case MaxHartIdBits => 1
      case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
      case LogUtilsOptionsKey => LogUtilsOptions(false, false, false)
    }
    base.alterPartial {
      case EdgeInKey => new TLEdgeIn(client, manager, base, UnlocatableSourceInfo)
      case EdgeOutKey => new TLEdgeOut(client, manager, base, UnlocatableSourceInfo)
    }
  }

  private def zero(data: Data): Unit = data match {
    case b: Bool => b.poke(false.B)
    case u: UInt => u.poke(0.U)
    case r: Record => r.elements.values.foreach(zero)
    case v: Vec[_] => v.foreach(zero)
    case other => throw new IllegalArgumentException(s"unhandled test input $other")
  }
  private case class Request(id: Int, address: BigInt, opcode: BigInt)
  private case class Response(id: Int, dbid: Int, opcode: BigInt, state: BigInt)
  private case class Beat(id: Int, dbid: Int, dataID: Int, data: BigInt, be: BigInt, state: BigInt)

  private class Driver(val m: CachelessCHIHomeCore) {
    zero(m.io.rn.tx.req.bits); zero(m.io.rn.tx.rsp.bits); zero(m.io.rn.tx.dat.bits)
    zero(m.io.sn.rx.rsp.bits); zero(m.io.sn.rx.dat.bits)
    m.io.nodeID.poke(home.U); m.io.snNodeID.poke(sn.U)
    m.io.rn.tx.req.valid.poke(false.B); m.io.rn.tx.rsp.valid.poke(false.B)
    m.io.rn.tx.dat.valid.poke(false.B); m.io.sn.rx.rsp.valid.poke(false.B)
    m.io.sn.rx.dat.valid.poke(false.B)
    m.io.rn.rx.rsp.ready.poke(false.B); m.io.rn.rx.dat.ready.poke(false.B)
    m.io.rn.rx.snp.ready.poke(true.B); m.io.sn.tx.req.ready.poke(false.B)
    m.io.sn.tx.dat.ready.poke(false.B)
    m.clock.step(2)

    def waitFor(cond: => Boolean): Unit = {
      var cycles = 0
      while (!cond && cycles < 200) { m.clock.step(); cycles += 1 }
      assert(cond, "protocol did not progress within 200 test cycles")
    }
    def admit(op: UInt, address: BigInt, txn: Int, ack: Boolean = false,
              order: UInt = OrderEncodings.None, size: Int = 6, device: Boolean = false): Unit = {
      zero(m.io.rn.tx.req.bits)
      m.io.rn.tx.req.bits.opcode.poke(op); m.io.rn.tx.req.bits.addr.poke(address.U)
      m.io.rn.tx.req.bits.txnID.poke(txn.U); m.io.rn.tx.req.bits.srcID.poke(rn.U)
      m.io.rn.tx.req.bits.tgtID.poke(home.U); m.io.rn.tx.req.bits.size.poke(size.U)
      m.io.rn.tx.req.bits.expCompAck.poke(ack.B); m.io.rn.tx.req.bits.order.poke(order)
      m.io.rn.tx.req.bits.memAttr.device.poke(device.B)
      m.io.rn.tx.req.valid.poke(true.B)
      waitFor(m.io.rn.tx.req.ready.peek().litToBoolean)
      m.clock.step(); m.io.rn.tx.req.valid.poke(false.B)
    }
    def popReq(): Request = {
      m.io.sn.tx.req.ready.poke(true.B)
      waitFor(m.io.sn.tx.req.valid.peek().litToBoolean)
      val b = m.io.sn.tx.req.bits
      b.srcID.expect(home.U); b.tgtID.expect(sn.U)
      b.memAttr.ewa.expect(false.B); b.order.expect(OrderEncodings.None)
      b.allowRetry.expect(true.B); b.pCrdType.expect(0.U); b.expCompAck.expect(false.B)
      val r = Request(b.txnID.peek().litValue.toInt, b.addr.peek().litValue, b.opcode.peek().litValue)
      m.clock.step(); m.io.sn.tx.req.ready.poke(false.B); r
    }
    def snRsp(id: Int, op: UInt, dbid: Int): Unit = {
      zero(m.io.sn.rx.rsp.bits)
      m.io.sn.rx.rsp.bits.txnID.poke(id.U); m.io.sn.rx.rsp.bits.dbID.poke(dbid.U)
      m.io.sn.rx.rsp.bits.srcID.poke(sn.U); m.io.sn.rx.rsp.bits.tgtID.poke(home.U)
      m.io.sn.rx.rsp.bits.opcode.poke(op); m.io.sn.rx.rsp.valid.poke(true.B)
      m.io.sn.rx.rsp.ready.expect(true.B)
      m.clock.step(); m.io.sn.rx.rsp.valid.poke(false.B)
    }
    def popRsp(): Response = {
      m.io.rn.rx.rsp.ready.poke(true.B)
      waitFor(m.io.rn.rx.rsp.valid.peek().litToBoolean)
      val b = m.io.rn.rx.rsp.bits
      b.srcID.expect(home.U); b.tgtID.expect(rn.U)
      val r = Response(b.txnID.peek().litValue.toInt, b.dbID.peek().litValue.toInt,
        b.opcode.peek().litValue, b.resp.peek().litValue)
      m.clock.step(); m.io.rn.rx.rsp.ready.poke(false.B); r
    }
    def readBeat(id: Int, dataID: Int, value: BigInt): Unit = {
      zero(m.io.sn.rx.dat.bits)
      m.io.sn.rx.dat.bits.txnID.poke(id.U); m.io.sn.rx.dat.bits.srcID.poke(sn.U)
      m.io.sn.rx.dat.bits.tgtID.poke(home.U); m.io.sn.rx.dat.bits.opcode.poke(CompData)
      m.io.sn.rx.dat.bits.dataID.poke(dataID.U); m.io.sn.rx.dat.bits.data.poke(value.U)
      m.io.sn.rx.dat.bits.be.poke(BigInt("ffffffff", 16).U)
      m.io.sn.rx.dat.valid.poke(true.B)
      waitFor(m.io.sn.rx.dat.ready.peek().litToBoolean)
      m.clock.step(); m.io.sn.rx.dat.valid.poke(false.B)
    }
    def writeBeat(id: Int, dataID: Int, value: BigInt, copyback: Boolean = true,
                  be: BigInt = BigInt("ffffffff", 16)): Unit = {
      zero(m.io.rn.tx.dat.bits)
      m.io.rn.tx.dat.bits.txnID.poke(id.U); m.io.rn.tx.dat.bits.srcID.poke(rn.U)
      m.io.rn.tx.dat.bits.tgtID.poke(home.U)
      m.io.rn.tx.dat.bits.opcode.poke(if (copyback) CopyBackWrData else NonCopyBackWrData)
      m.io.rn.tx.dat.bits.dataID.poke(dataID.U); m.io.rn.tx.dat.bits.data.poke(value.U)
      m.io.rn.tx.dat.bits.be.poke(be.U); m.io.rn.tx.dat.valid.poke(true.B)
      waitFor(m.io.rn.tx.dat.ready.peek().litToBoolean)
      m.clock.step(); m.io.rn.tx.dat.valid.poke(false.B)
    }
    def popData(fromRN: Boolean): Beat = {
      val port = if (fromRN) m.io.rn.rx.dat else m.io.sn.tx.dat
      port.ready.poke(true.B); waitFor(port.valid.peek().litToBoolean)
      val b = port.bits
      b.srcID.expect(home.U); b.tgtID.expect((if (fromRN) rn else sn).U)
      b.opcode.expect(if (fromRN) CompData else NonCopyBackWrData)
      val beat = Beat(b.txnID.peek().litValue.toInt, b.dbID.peek().litValue.toInt,
        b.dataID.peek().litValue.toInt, b.data.peek().litValue, b.be.peek().litValue, b.resp.peek().litValue)
      m.clock.step(); port.ready.poke(false.B); beat
    }
    def ack(id: Int): Unit = {
      zero(m.io.rn.tx.rsp.bits)
      m.io.rn.tx.rsp.bits.opcode.poke(CompAck); m.io.rn.tx.rsp.bits.txnID.poke(id.U)
      m.io.rn.tx.rsp.bits.srcID.poke(rn.U); m.io.rn.tx.rsp.bits.tgtID.poke(home.U)
      m.io.rn.tx.rsp.valid.poke(true.B); m.clock.step(); m.io.rn.tx.rsp.valid.poke(false.B)
    }
    def empty(): Unit = { m.clock.step(3); m.io.active.expect(0.U) }
  }

  behavior of "CachelessCHIHome"
  it should "preserve all bank TxnID bits, read data and CompAck lifetime under backpressure" in {
    simulate(new CachelessCHIHomeCore(capacity)) { m =>
      val d = new Driver(m)
      for (bank <- 0 until 4) {
        // Issue E.b TxnID[11] is MMIO, [10:9] select the four L2 banks.
        val txn = (bank << 9) | 13
        d.admit(ReadUnique, 0x80000000L + bank * 64, txn, ack = true)
        val request = d.popReq()
        assert(request.opcode == ReadNoSnp.litValue)
        d.readBeat(request.id, 2, bank + 0x2200); d.readBeat(request.id, 0, bank + 0x1100)
        m.clock.step(7); m.io.active.expect(1.U)
        val second = d.popData(fromRN = true); val first = d.popData(fromRN = true)
        assert(second.id == txn && second.dataID == 2 && second.data == bank + 0x2200)
        assert(first.id == txn && first.dataID == 0 && first.data == bank + 0x1100)
        assert(first.state == CHICohStates.UC.litValue)
        d.ack(first.dbid); d.empty()
      }
    }
  }

  it should "grant copyback credit before data but withhold Comp and a same-line read until endpoint completion" in {
    simulate(new CachelessCHIHomeCore(capacity)) { m =>
      val d = new Driver(m)
      d.admit(WriteBackFull, 0x80001000L, 71, ack = true)
      d.admit(ReadNotSharedDirty, 0x80001000L, 142, ack = true)
      val write = d.popReq(); assert(write.opcode == WriteNoSnpPtl.litValue)
      d.snRsp(write.id, DBIDResp, 23)
      val grant = d.popRsp(); assert(grant.opcode == DBIDResp.litValue && grant.id == 71)
      d.writeBeat(grant.dbid, 0, 0x1234); d.writeBeat(grant.dbid, 2, 0x5678)
      val a = d.popData(fromRN = false); val b = d.popData(fromRN = false)
      assert(a.id == 23 && b.id == 23 && a.data == 0x1234 && b.data == 0x5678)
      m.clock.step(66)
      m.io.rn.rx.rsp.valid.expect(false.B); m.io.sn.tx.req.valid.expect(false.B)
      d.snRsp(write.id, Comp, 23)
      val completed = d.popRsp(); assert(completed.opcode == Comp.litValue && completed.dbid == grant.dbid)
      m.io.sn.tx.req.valid.expect(false.B)
      d.ack(completed.dbid)
      val read = d.popReq(); assert(read.address == 0x80001000L)
      d.readBeat(read.id, 0, 0x1234); d.readBeat(read.id, 2, 0x5678)
      val data0 = d.popData(fromRN = true); val data1 = d.popData(fromRN = true)
      assert(data0.id == 142 && data1.id == 142)
      d.ack(data0.dbid); d.empty()
    }
  }

  it should "complete permissions and clean maintenance locally without issuing a false memory read" in {
    simulate(new CachelessCHIHomeCore(capacity)) { m =>
      val d = new Driver(m)
      for ((op, state) <- Seq(MakeUnique -> CHICohStates.UC, CleanUnique -> CHICohStates.UC,
        CleanShared -> CHICohStates.I, CleanInvalid -> CHICohStates.I, MakeInvalid -> CHICohStates.I,
        Evict -> CHICohStates.I, WriteEvictOrEvict -> CHICohStates.I)) {
        d.admit(op, 0x80002000L, 193, ack = true)
        val r = d.popRsp(); assert(r.opcode == Comp.litValue && r.state == state.litValue && r.id == 193)
        m.io.sn.tx.req.valid.expect(false.B); m.io.rn.rx.dat.valid.expect(false.B)
        d.ack(r.dbid); d.empty()
      }
    }
  }

  it should "reserve release resources when ordinary slots fill and bound downstream transactions" in {
    simulate(new CachelessCHIHomeCore(capacity)) { m =>
      val d = new Driver(m)
      for (i <- 0 until 4) d.admit(ReadUnique, 0x80003000L + i * 64, i)
      m.io.ordinaryActive.expect(4.U)
      m.io.rn.tx.req.valid.poke(true.B); m.io.rn.tx.req.ready.expect(false.B); m.clock.step(3)
      m.io.rn.tx.req.valid.poke(false.B)
      d.admit(WriteBackFull, 0x80004000L, 64)
      d.admit(Evict, 0x80005000L, 128)
      m.io.releaseActive.expect(2.U); m.io.active.expect(6.U)
      assert(m.io.blockedAdmissions.peek().litValue >= 3)
      // Only two SN transactions may start; local eviction can still progress.
      val evict = d.popRsp(); assert(evict.id == 128 && evict.opcode == Comp.litValue)
      m.clock.step(3); m.io.releaseActive.expect(1.U)
      m.io.rn.tx.req.bits.opcode.poke(WriteBackFull); m.io.rn.tx.req.ready.expect(true.B)
    }
  }

  it should "preserve partial uncached writes and upper-beat reads, including ordered read receipts" in {
    simulate(new CachelessCHIHomeCore(capacity)) { m =>
      val d = new Driver(m)
      d.admit(WriteNoSnpPtl, 0x80006024L, 0x805, size = 2, device = true)
      val w = d.popReq(); d.snRsp(w.id, DBIDResp, 29)
      val grant = d.popRsp(); d.writeBeat(grant.dbid, 2, 0xfeed, copyback = false, be = BigInt("f0", 16))
      val beat = d.popData(fromRN = false)
      assert(beat.id == 29 && beat.dataID == 2 && beat.be == 0xf0 && beat.data == 0xfeed)
      d.snRsp(w.id, Comp, 29); assert(d.popRsp().id == 0x805); d.empty()
      d.admit(ReadNoSnp, 0x80006024L, 0x806, order = OrderEncodings.EndpointOrder, size = 2, device = true)
      val r = d.popReq(); assert(d.popRsp().opcode == ReadReceipt.litValue)
      d.readBeat(r.id, 2, 0xfeed)
      val data = d.popData(fromRN = true)
      assert(data.id == 0x806 && data.state == CHICohStates.I.litValue && data.dataID == 2)
      d.empty()
    }
  }

  it should "order Device requests behind older endpoint writes while permitting reserved victim releases" in {
    simulate(new CachelessCHIHomeCore(capacity)) { m =>
      val d = new Driver(m)
      d.admit(WriteBackFull, 0x80007000L, 31)
      val w = d.popReq(); d.snRsp(w.id, DBIDResp, 30)
      val grant = d.popRsp()
      d.admit(ReadNoSnp, 0x80008000L, 95, order = OrderEncodings.EndpointOrder, size = 3, device = true)
      m.clock.step(5); m.io.sn.tx.req.valid.expect(false.B)
      m.io.rn.tx.req.bits.opcode.poke(ReadUnique); m.io.rn.tx.req.bits.memAttr.device.poke(false.B)
      m.io.rn.tx.req.bits.order.poke(OrderEncodings.None)
      m.io.rn.tx.req.ready.expect(false.B) // younger ordinary request waits behind Device barrier
      d.admit(Evict, 0x80009000L, 159)
      assert(d.popRsp().id == 159) // release resources remain independently available
      d.writeBeat(grant.dbid, 0, 10); d.writeBeat(grant.dbid, 2, 20)
      d.popData(fromRN = false); d.popData(fromRN = false)
      m.clock.step(5); m.io.sn.tx.req.valid.expect(false.B)
      d.snRsp(w.id, Comp, 30); assert(d.popRsp().id == 31)
      val r = d.popReq(); assert(r.address == 0x80008000L)
      assert(d.popRsp().opcode == ReadReceipt.litValue)
      d.readBeat(r.id, 0, 0x123); assert(d.popData(fromRN = true).id == 95)
      d.empty()
    }
  }

  it should "distinguish concurrent acquire and write Comp by HN token while keeping the old config default" in {
    assert(!L2Param().supportSeparateWriteCompletion)
    simulate(new SeparateWriteCompletion) { m =>
      m.io.allocate.poke(false.B); m.io.response.valid.poke(false.B); zero(m.io.response.bits)
      m.clock.step()
      def response(op: UInt, token: Int, source: Int = home): Unit = {
        m.io.response.bits.opcode.poke(op); m.io.response.bits.srcID.poke(source.U)
        m.io.response.bits.dbID.poke(token.U); m.io.response.valid.poke(true.B)
      }
      response(DBIDResp, 5); m.io.isWriteComp.expect(false.B); m.clock.step()
      m.io.pending.expect(true.B)
      response(Comp, 1); m.io.isWriteComp.expect(false.B); m.clock.step(); m.io.pending.expect(true.B)
      response(Comp, 5, source = 3); m.io.isWriteComp.expect(false.B); m.clock.step()
      response(Comp, 5); m.io.isWriteComp.expect(true.B); m.clock.step(); m.io.pending.expect(false.B)
      m.io.response.valid.poke(false.B); m.io.allocate.poke(true.B); m.clock.step()
      m.io.pending.expect(false.B)
    }
  }

  it should "keep channel payloads and response IDs stable with randomized backpressure and reordered independent reads" in {
    simulate(new CachelessCHIHomeCore(capacity)) { m =>
      val d = new Driver(m)
      val random = new scala.util.Random(0x434849)
      for (round <- 0 until 24) {
        val addresses = Seq(BigInt(0x80010000L + round * 128), BigInt(0x80010040L + round * 128))
        val txns = Seq(((round % 4) << 9) | 17, (((round + 1) % 4) << 9) | 18)
        for (i <- 0 until 2) d.admit(ReadUnique, addresses(i), txns(i), ack = true)
        val requests = (0 until 2).map { _ =>
          d.waitFor(m.io.sn.tx.req.valid.peek().litToBoolean)
          val before = m.io.sn.tx.req.bits.txnID.peek().litValue
          val address = m.io.sn.tx.req.bits.addr.peek().litValue
          m.clock.step(random.nextInt(10) + 1)
          m.io.sn.tx.req.valid.expect(true.B)
          m.io.sn.tx.req.bits.txnID.expect(before.U); m.io.sn.tx.req.bits.addr.expect(address.U)
          d.popReq()
        }
        for (request <- random.shuffle(requests)) {
          val i = addresses.indexOf(request.address)
          val values = Seq(BigInt(round * 100 + i * 10 + 1), BigInt(round * 100 + i * 10 + 2))
          val order = random.shuffle(Seq(0, 2))
          for (dataID <- order) d.readBeat(request.id, dataID, values(dataID / 2))
          m.clock.step(random.nextInt(10) + 1)
          val beats = Seq(d.popData(fromRN = true), d.popData(fromRN = true))
          assert(beats.map(_.id) == Seq(txns(i), txns(i)))
          assert(beats.map(_.dataID) == order)
          assert(beats.forall(b => b.data == values(b.dataID / 2)))
          m.clock.step(random.nextInt(6) + 1); d.ack(beats.head.dbid)
        }
        d.empty()
      }
    }
  }

  it should "activate the real bounded credit links without an LLC parameter dependency" in {
    simulate(new CachelessCHIHome(capacity)) { m =>
      m.io.nodeID.poke(home.U); m.io.snNodeID.poke(sn.U)
      m.io.rn.tx.linkactivereq.poke(true.B); m.io.rn.rx.linkactiveack.poke(true.B)
      m.io.rn.txsactive.poke(true.B); m.io.rn.syscoreq.poke(true.B)
      m.io.sn.rx.linkactivereq.poke(true.B); m.io.sn.tx.linkactiveack.poke(true.B)
      m.io.sn.rxsactive.poke(true.B)
      for (port <- Seq(m.io.rn.tx.req, m.io.rn.tx.rsp, m.io.rn.tx.dat, m.io.sn.rx.rsp, m.io.sn.rx.dat)) {
        port.flitv.poke(false.B); port.flitpend.poke(false.B); port.flit.poke(0.U)
      }
      for (port <- Seq(m.io.rn.rx.rsp, m.io.rn.rx.dat, m.io.rn.rx.snp, m.io.sn.tx.req, m.io.sn.tx.dat))
        port.lcrdv.poke(false.B)
      var credits = 0
      for (_ <- 0 until 20) {
        if (m.io.rn.tx.req.lcrdv.peek().litToBoolean) credits += 1
        m.clock.step()
      }
      assert(credits == capacity.linkCredits)
      m.io.rn.tx.linkactiveack.expect(true.B); m.io.rn.rx.linkactivereq.expect(true.B)
      m.io.rn.syscoack.expect(true.B); m.io.sn.rx.linkactiveack.expect(true.B)
      m.io.sn.tx.linkactivereq.expect(true.B); m.io.sn.tx.req.flitv.expect(false.B)
    }
  }

  it should "keep the real L2 CMO blocked until delayed write Comp and retain default composite completion" in {
    for (split <- Seq(false, true)) simulate(new MSHR()(mshrParams(split))) { m =>
      m.io.id.poke(3.U)
      zero(m.io.alloc.bits); zero(m.io.nestedwb); zero(m.io.aMergeTask.bits)
      zero(m.io.replResp.bits); zero(m.io.resps.rxrsp.bits); zero(m.io.resps.rxdat.bits)
      zero(m.io.resps.sinkC.bits)
      m.io.alloc.valid.poke(false.B); m.io.aMergeTask.valid.poke(false.B)
      m.io.replResp.valid.poke(false.B); m.io.resps.rxrsp.valid.poke(false.B)
      m.io.resps.rxdat.valid.poke(false.B); m.io.resps.sinkC.valid.poke(false.B)
      m.io.tasks.txreq.ready.poke(false.B); m.io.tasks.txrsp.ready.poke(false.B)
      m.io.tasks.mainpipe.ready.poke(false.B); m.io.tasks.source_b.ready.poke(false.B)
      m.io.pCrd.grant.poke(false.B)
      m.clock.step(2)
      val state = m.io.alloc.bits.state
      state.elements.values.foreach(_.asInstanceOf[Bool].poke(true.B))
      state.s_acquire.poke(false.B); state.s_release.poke(false.B)
      state.s_cmoresp.poke(false.B); state.s_cbwrdata.get.poke(false.B)
      state.w_grant.poke(false.B); state.w_grantfirst.poke(false.B)
      state.w_grantlast.poke(false.B); state.w_releaseack.poke(false.B)
      m.io.alloc.bits.task.channel.poke(1.U)
      m.io.alloc.bits.task.opcode.poke(TLMessages.CBOClean)
      m.io.alloc.bits.dirResult.hit.poke(true.B)
      m.io.alloc.bits.dirResult.meta.state.poke(MetaData.TIP)
      m.io.alloc.bits.dirResult.meta.dirty.poke(true.B)
      m.io.alloc.valid.poke(true.B); m.clock.step(); m.io.alloc.valid.poke(false.B)
      m.io.tasks.mainpipe.valid.expect(true.B)
      m.io.tasks.mainpipe.bits.chiOpcode.get.expect(WriteCleanFull)
      m.io.tasks.mainpipe.ready.poke(true.B); m.clock.step(); m.io.tasks.mainpipe.ready.poke(false.B)
      def respond(op: UInt, token: Int): Unit = {
        m.io.resps.rxrsp.bits.chiOpcode.get.poke(op)
        m.io.resps.rxrsp.bits.srcID.get.poke(home.U)
        m.io.resps.rxrsp.bits.dbID.get.poke(token.U)
        m.io.resps.rxrsp.bits.resp.get.poke(CHICohStates.I)
        m.io.resps.rxrsp.valid.poke(true.B); m.clock.step(); m.io.resps.rxrsp.valid.poke(false.B)
      }
      respond(if (split) DBIDResp else CompDBIDResp, 5)
      m.io.msInfo.bits.w_releaseack.expect((!split).B)
      m.io.tasks.mainpipe.valid.expect(true.B)
      m.io.tasks.mainpipe.bits.chiOpcode.get.expect(CopyBackWrData)
      m.io.tasks.mainpipe.ready.poke(true.B); m.clock.step(); m.io.tasks.mainpipe.ready.poke(false.B)
      if (split) {
        m.clock.step(66)
        m.io.msInfo.bits.w_releaseack.expect(false.B)
        m.io.tasks.txreq.valid.expect(false.B)
        respond(Comp, 1) // acquire/another token cannot acknowledge the victim write
        m.io.msInfo.bits.w_releaseack.expect(false.B)
        respond(Comp, 5)
        m.io.msInfo.bits.w_releaseack.expect(true.B)
        m.io.tasks.txrsp.valid.expect(false.B) // WriteClean has no CompAck
      }
      m.io.tasks.txreq.valid.expect(true.B) // only now may the CMO proceed
    }
  }
}
