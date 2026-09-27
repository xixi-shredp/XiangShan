// Copyright (c) 2026
// Synthesizable H2P Hint v3 predictor. The simulation image loader drives io.load.
package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.GTimer
import utility.XSPerfAccumulate
import xiangshan.frontend.bpu.{BpuModule, BpuRedirect, Prediction}
import xiangshan.frontend.ftq.FtqPtr

class HintSnapshot(implicit p: Parameters) extends Bundle {
  val ithHead = UInt(7.W)
  val callHead = UInt(7.W)
  val retHead = UInt(7.W)
  val ithWrites = UInt(8.W)
  val callWrites = UInt(8.W)
  val retWrites = UInt(8.W)
  val callLow12 = UInt(12.W)
  val retLow12 = UInt(12.W)
  val lhrHead = Vec(16, UInt(4.W))
}

class HintLoadCmd extends Bundle {
  // 0: clear; 1: CAM; 2: Context; 3: Slot.
  val kind = UInt(2.W)
  val index = UInt(12.W)
  val data = UInt(64.W)
}

class HintResolve(implicit p: Parameters) extends Bundle {
  val ftqPtr = new FtqPtr
  val cfiPc = UInt(64.W)
  val target = UInt(64.W)
  val eligible = Bool()
  val baselinePc = UInt(64.W)
  val baselineTarget = UInt(64.W)
  val baselineValid = Bool()
  val blockStart = UInt(64.W)
}

class HintProfileEvent extends Bundle {
  val pc = UInt(64.W)
  val actual = UInt(64.W)
  val baselinePc = UInt(64.W)
  val baselineTarget = UInt(64.W)
  val baselineValid = Bool()
  val blockStart = UInt(64.W)
  val queryCycle = UInt(64.W)
  val ith = UInt(28.W)
  val lhr = UInt(28.W)
  val call = UInt(28.W)
  val ret = UInt(28.W)
  val eligible = Bool()
  val observed = Bool()
}

class HintReplayEvent extends Bundle {
  val pc = UInt(64.W)
  val actual = UInt(64.W)
  val baselineTarget = UInt(64.W)
  val finalTarget = UInt(64.W)
  val usedHint = Bool()
  val contextHit = Bool()
  val confAtPrediction = UInt(2.W)
}

class HintProfileRecord(implicit p: Parameters) extends Bundle {
  val ptr = new FtqPtr
  val resolved = Bool()
  val event = new HintProfileEvent
  // Observer-only restore window. It prevents baseline profiling from needing
  // the physical Hint admission stalls after repeated speculative writes.
  val ithLow64 = UInt(64.W)
  val callLow64 = UInt(64.W)
  val retLow64 = UInt(64.W)
  val lhrLow8 = Vec(16, UInt(8.W))
}

class HintStats extends Bundle {
  val camMiss = UInt(32.W)
  val contextMiss = UInt(32.W)
  val confZero = UInt(32.W)
  val overrides = UInt(32.W)
  val metaStall = UInt(32.W)
  val lhrStall = UInt(32.W)
  val confUp = UInt(32.W)
  val confDown = UInt(32.W)
  val confDropped = UInt(32.W)
}

class HintLookup extends Bundle {
  val candidate = Bool()
  val pc = UInt(64.W)
  val camIndex = UInt(4.W)
  val contextHit = Bool()
  val usable = Bool()
  val set = UInt(11.W)
  val way = UInt(1.W)
  val target = UInt(64.W)
}

class HintRecord(implicit p: Parameters) extends Bundle {
  val valid = Bool()
  val ptr = new FtqPtr
  val camIndex = UInt(4.W)
  val confirmed = Bool()
  val ready = Bool()
  val committed = Bool()
  val enqueued = Bool()
  val measure = Bool()
  val correct = Bool()
  val contextHit = Bool()
  val setway = UInt(12.W)
  val shadowTarget = UInt(64.W)
  val baselineTarget = UInt(64.W)
  val actualTarget = UInt(64.W)
  val actualValid = Bool()
  val usedHint = Bool()
  val confAtPrediction = UInt(2.W)
}

class HintConfReq(implicit p: Parameters) extends Bundle {
  val ptr = new FtqPtr
  val setway = UInt(12.W)
  val oldConf = UInt(2.W)
  val newConf = UInt(2.W)
  val applied = Bool()
  val committed = Bool()
}

class Hint(implicit p: Parameters) extends BpuModule {
  class HintIO extends Bundle {
    val load = Flipped(Decoupled(new HintLoadCmd))
    // The simulation wrapper raises this only after clearing and loading an image.
    val enabled = Input(Bool())
    val profile = Input(Bool())
    val measureActive = Input(Bool())
    val s1StartPc = Input(UInt(64.W))
    val s1CfiPc = Input(UInt(64.W))
    val s1Prediction = Input(new Prediction)
    val s1Ptr = Input(new FtqPtr)
    val s1Fire = Input(Bool())
    val s2Fire = Input(Bool())
    val s3Fire = Input(Bool())
    val s3Ptr = Input(new FtqPtr)
    val s3CfiPc = Input(UInt(64.W))
    val s3Prediction = Input(new Prediction)
    val s3Override = Input(Bool())
    val s3UsedHint = Input(Bool())
    val s3BaselineTarget = Input(UInt(64.W))
    val redirect = Input(Valid(new BpuRedirect))
    val commit = Input(Valid(new FtqPtr))
    val resolve = Input(Vec(backendParams.BrhCnt + 1, Valid(new HintResolve)))
    val canAccept = Output(Bool())
    val s3Lookup = Output(new HintLookup)
    val s3Snapshot = Output(new HintSnapshot)
    val confDropped = Output(UInt(32.W))
    val profileEvent = Output(Valid(new HintProfileEvent))
    val replayEvent = Output(Valid(new HintReplayEvent))
    val stats = Output(new HintStats)
  }
  val io = IO(new HintIO)

  private val cam = RegInit(VecInit(Seq.fill(16)(0.U(52.W))))
  private val slots = RegInit(VecInit(Seq.fill(32)(0.U(47.W))))
  // A single Context access (two ways of one set) is admitted each cycle.
  private val context = Reg(Vec(4096, UInt(25.W)))
  private val clearing = RegInit(true.B)
  private val clearIndex = RegInit(0.U(12.W))

  private val ith = RegInit(VecInit(Seq.fill(128)(0.U(2.W))))
  private val calls = RegInit(VecInit(Seq.fill(128)(0.U(2.W))))
  private val rets = RegInit(VecInit(Seq.fill(128)(0.U(2.W))))
  private val lhr = RegInit(VecInit(Seq.fill(16)(VecInit(Seq.fill(8)(0.U(2.W))))))
  private val ithHead = RegInit(0.U(7.W))
  private val callHead = RegInit(0.U(7.W))
  private val retHead = RegInit(0.U(7.W))
  private val ithWrites = RegInit(0.U(8.W))
  private val callWrites = RegInit(0.U(8.W))
  private val retWrites = RegInit(0.U(8.W))
  private val lhrHead = RegInit(VecInit(Seq.fill(16)(0.U(4.W))))

  private val records = RegInit(VecInit(Seq.fill(16)(0.U.asTypeOf(new HintRecord))))
  private val confLog = Reg(Vec(32, new HintConfReq))
  private val confHead = RegInit(0.U(6.W))
  private val confWrite = RegInit(0.U(6.W))
  private val confTail = RegInit(0.U(6.W))
  private val rollback = RegInit(false.B)
  private val rollbackBoundary = Reg(new FtqPtr)
  private val confDropped = RegInit(0.U(32.W))
  io.confDropped := confDropped
  private val camMisses = RegInit(0.U(32.W))
  private val contextMisses = RegInit(0.U(32.W))
  private val confZeroFallbacks = RegInit(0.U(32.W))
  private val overrideCount = RegInit(0.U(32.W))
  private val metaStalls = RegInit(0.U(32.W))
  private val lhrStalls = RegInit(0.U(32.W))
  private val confUpCount = RegInit(0.U(32.W))
  private val confDownCount = RegInit(0.U(32.W))
  io.stats.camMiss := camMisses
  io.stats.contextMiss := contextMisses
  io.stats.confZero := confZeroFallbacks
  io.stats.overrides := overrideCount
  io.stats.metaStall := metaStalls
  io.stats.lhrStall := lhrStalls
  io.stats.confUp := confUpCount
  io.stats.confDown := confDownCount
  io.stats.confDropped := confDropped

  private def ringLow(cells: Vec[UInt], head: UInt, samples: Int, indexBits: Int): UInt =
    Cat((0 until samples).reverse.map(i => cells((head - (i + 1).U)(indexBits - 1, 0))))

  private def fold28(x: UInt): UInt = {
    val v = x.pad(84)
    (v ^ (v >> 28) ^ (v >> 56))(27, 0)
  }
  private def rol28(x: UInt, n: Int): UInt = Cat(x(27 - n, 0), x(27, 28 - n))
  private def fold14(pc: UInt): UInt = {
    val v = pc(47, 1).pad(56)
    v(13, 0) ^ v(27, 14) ^ v(41, 28) ^ v(55, 42)
  }
  private def canonical(word: UInt): UInt = Cat(Fill(16, word(46)), word, 0.U(1.W))

  val snapshot = Wire(new HintSnapshot)
  snapshot.ithHead := ithHead
  snapshot.callHead := callHead
  snapshot.retHead := retHead
  snapshot.ithWrites := ithWrites
  snapshot.callWrites := callWrites
  snapshot.retWrites := retWrites
  snapshot.callLow12 := ringLow(calls, callHead, 6, 7)
  snapshot.retLow12 := ringLow(rets, retHead, 6, 7)
  snapshot.lhrHead := lhrHead
  val s2Snapshot = RegEnable(snapshot, io.s1Fire)
  val s3Snapshot = RegEnable(s2Snapshot, io.s2Fire)
  io.s3Snapshot := s3Snapshot

  // CAM image rows are sorted by ascending PC, so the first window hit is the earliest PC.
  val endWide = Cat(0.U(1.W), io.s1StartPc) + 64.U(65.W)
  val windowEnd = Cat(endWide(63, 5), 0.U(5.W))
  val camHit = VecInit((0 until 16).map { i =>
    val pc = canonical(cam(i)(46, 0))
    cam(i)(51) && !endWide(64) && pc >= io.s1StartPc && pc < windowEnd
  })
  val candidate = (io.enabled || io.profile) && camHit.asUInt.orR
  when(io.enabled && io.s1Fire && !camHit.asUInt.orR) {
    camMisses := camMisses + 1.U
  }
  val camIndex = PriorityEncoder(camHit)
  val camWord = cam(camIndex)
  val candidatePc = canonical(camWord(46, 0))
  val candidateMask = camWord(50, 47)

  val free = VecInit(records.map(r => !r.valid))
  val lhrInflight = PopCount(records.map(r => r.valid && r.camIndex === camIndex))
  when(io.enabled && candidate && !free.asUInt.orR) { metaStalls := metaStalls + 1.U }
  when(io.enabled && candidate && lhrInflight >= 4.U) { lhrStalls := lhrStalls + 1.U }
  io.canAccept := !io.enabled ||
    (!clearing && !rollback && (!candidate || (free.asUInt.orR && (!candidateMask(1) || lhrInflight < 4.U))))

  if (sys.env.get("H2P_HINT_DIAG").contains("1")) {
    val lhrFull = io.enabled && candidate && lhrInflight >= 4.U
    val metaFull = io.enabled && candidate && !free.asUInt.orR
    val gateCount = RegInit(0.U(32.W))
    XSPerfAccumulate("h2p_diag_lhr_candidate_full", lhrFull)
    XSPerfAccumulate("h2p_diag_meta_candidate_full", metaFull)
    when(lhrFull || metaFull) {
      gateCount := gateCount + 1.U
      when(gateCount < 64.U || gateCount(9, 0) === 0.U) {
        printf(p"H2P_DIAG_HINT_GATE cycle=${GTimer()} gate_count=${gateCount} " +
          p"ftq=${io.s1Ptr.value} start_pc=${Hexadecimal(io.s1StartPc)} " +
          p"candidate_pc=${Hexadecimal(candidatePc)} cam_index=${camIndex} lhr_inflight=${lhrInflight} " +
          p"free=${free.asUInt.orR} meta_full=${metaFull} lhr_full=${lhrFull} " +
          p"clearing=${clearing} rollback=${rollback} measure=${io.measureActive} can_accept=${io.canAccept}\n")
      }
    }
  }
  val lookupFire = io.s1Fire && candidate

  val ithLow64 = ringLow(ith, ithHead, 32, 7)
  val callLow64 = ringLow(calls, callHead, 32, 7)
  val retLow64 = ringLow(rets, retHead, 32, 7)
  val lhrLow8 = ringLow(lhr(camIndex), lhrHead(camIndex), 4, 3)
  val ithComponent = fold28(ithLow64)
  val lhrComponent = fold28(lhrLow8)
  val callComponent = fold28(callLow64)
  val retComponent = fold28(retLow64)
  val key = fold28(camWord(46, 0)) ^
    Mux(candidateMask(0), rol28(ithComponent, 2), 0.U(28.W)) ^
    Mux(candidateMask(1), rol28(lhrComponent, 3), 0.U(28.W)) ^
    Mux(candidateMask(2), rol28(callComponent, 4), 0.U(28.W)) ^
    Mux(candidateMask(3), rol28(retComponent, 5), 0.U(28.W))
  val set = key(10, 0)
  val tag = key(27, 11)
  val readContext = lookupFire
  val s2Row0 = RegEnable(context(Cat(set, 0.U(1.W))), lookupFire)
  val s2Row1 = RegEnable(context(Cat(set, 1.U(1.W))), lookupFire)
  val s2Candidate = RegEnable(candidate, io.s1Fire)
  val s2Pc = RegEnable(candidatePc, io.s1Fire)
  val s2CamIndex = RegEnable(camIndex, io.s1Fire)
  val s2Set = RegEnable(set, io.s1Fire)
  val s2Tag = RegEnable(tag, io.s1Fire)
  val s3Row0 = RegEnable(s2Row0, io.s2Fire)
  val s3Row1 = RegEnable(s2Row1, io.s2Fire)
  val s3Candidate = RegEnable(s2Candidate, io.s2Fire)
  val s3Pc = RegEnable(s2Pc, io.s2Fire)
  val s3CamIndex = RegEnable(s2CamIndex, io.s2Fire)
  val s3Set = RegEnable(s2Set, io.s2Fire)
  val s3Tag = RegEnable(s2Tag, io.s2Fire)
  val way0Hit = s3Row0(24) && s3Row0(16, 0) === s3Tag
  val way1Hit = s3Row1(24) && s3Row1(16, 0) === s3Tag
  val contextHit = io.enabled && s3Candidate && (way0Hit || way1Hit)
  val selectedWay = !way0Hit && way1Hit
  val selectedRow = Mux(selectedWay, s3Row1, s3Row0)
  val selectedSetway = Cat(s3Set, selectedWay)

  // Forward every unapplied or applied, uncommitted confidence update in FTQ age order.
  private def forwardedConf(setway: UInt, raw: UInt): UInt = {
    val values = Wire(Vec(33, UInt(2.W)))
    values(0) := raw
    for (i <- 0 until 32) {
      val req = confLog((confHead + i.U)(4, 0))
      values(i + 1) := Mux(i.U < (confTail - confHead) && req.setway === setway,
        req.newConf, values(i))
    }
    values(32)
  }
  val selectedConf = forwardedConf(selectedSetway, selectedRow(23, 22))
  when(io.enabled && io.s3Fire && s3Candidate && !contextHit) {
    contextMisses := contextMisses + 1.U
  }
  when(io.enabled && io.s3Fire && contextHit && selectedConf === 0.U) {
    confZeroFallbacks := confZeroFallbacks + 1.U
  }
  io.s3Lookup.candidate := s3Candidate
  io.s3Lookup.pc := s3Pc
  io.s3Lookup.camIndex := s3CamIndex
  io.s3Lookup.contextHit := contextHit
  io.s3Lookup.usable := contextHit && selectedConf =/= 0.U
  io.s3Lookup.set := s3Set
  io.s3Lookup.way := selectedWay
  io.s3Lookup.target := canonical(slots(selectedRow(21, 17)))

  val s1HintEvent = candidate && io.s1Prediction.taken &&
    io.s1Prediction.attribute.needIttage && io.s1CfiPc === candidatePc
  val s3HintEvent = s3Candidate && io.s3Prediction.taken &&
    io.s3Prediction.attribute.needIttage && io.s3CfiPc === s3Pc
  when(io.enabled && io.s3Fire && io.s3UsedHint && io.s3Override) {
    overrideCount := overrideCount + 1.U
  }

  // The observer retains one committed branch event per FTQ entry. It has no
  // path into io.canAccept or the prediction mux and is inert outside profile.
  val profileCycle = RegInit(0.U(64.W))
  profileCycle := profileCycle + 1.U
  val profileRows = RegInit(VecInit(Seq.fill(64)(0.U.asTypeOf(new HintProfileRecord))))
  when(io.profile && io.s1Fire) {
    val row = profileRows(io.s1Ptr.value)
    row := 0.U.asTypeOf(new HintProfileRecord)
    row.ptr := io.s1Ptr
    row.ithLow64 := ithLow64
    row.callLow64 := callLow64
    row.retLow64 := retLow64
    for (i <- 0 until 16) {
      row.lhrLow8(i) := ringLow(lhr(i), lhrHead(i), 4, 3)
    }
    when(candidate) {
      row.event.observed := true.B
      row.event.pc := candidatePc
      row.event.queryCycle := profileCycle
      row.event.ith := ithComponent
      row.event.lhr := lhrComponent
      row.event.call := callComponent
      row.event.ret := retComponent
    }
  }
  when(io.profile) {
    for (resolve <- io.resolve) {
      val row = profileRows(resolve.bits.ftqPtr.value)
      when(resolve.valid && resolve.bits.eligible &&
        row.ptr === resolve.bits.ftqPtr && !row.resolved) {
        row.resolved := true.B
        row.event.pc := resolve.bits.cfiPc
        row.event.actual := resolve.bits.target
        row.event.baselinePc := resolve.bits.baselinePc
        row.event.baselineTarget := resolve.bits.baselineTarget
        row.event.baselineValid := resolve.bits.baselineValid
        row.event.blockStart := resolve.bits.blockStart
        row.event.eligible := row.event.observed &&
          row.event.pc === resolve.bits.cfiPc && resolve.bits.baselineValid
      }
    }
  }
  val commitRow = profileRows(io.commit.bits.value)
  io.profileEvent.valid := io.profile && io.commit.valid && commitRow.resolved &&
    commitRow.ptr === io.commit.bits
  io.profileEvent.bits := commitRow.event
  when(io.profile && io.commit.valid) {
    profileRows(io.commit.bits.value).resolved := false.B
  }
  when(io.profile && io.s3Override) {
    for (i <- 0 until 64) {
      when(profileRows(i).ptr > io.s3Ptr) { profileRows(i).resolved := false.B }
    }
  }
  when(io.profile && io.redirect.valid) {
    for (i <- 0 until 64) {
      when(profileRows(i).ptr >= io.redirect.bits.newFtqIdx) {
        profileRows(i).resolved := false.B
      }
    }
  }

  // Snapshot restoration rewinds every global path, even when this FTQ entry is not an H2P branch.
  private def restoreAndApply(meta: HintSnapshot, pc: UInt, target: UInt,
                              taken: Bool, indirect: Bool, push: Bool, pop: Bool,
                              h2p: Bool, h2pIndex: UInt): Unit = {
    when(!io.profile) {
      assert((ithWrites - meta.ithWrites) <= 96.U)
      assert((callWrites - meta.callWrites) <= 96.U)
      assert((retWrites - meta.retWrites) <= 96.U)
    }
    ithHead := meta.ithHead + (taken && indirect).asUInt
    callHead := meta.callHead + (taken && push).asUInt
    retHead := meta.retHead + (taken && pop).asUInt
    ithWrites := meta.ithWrites + (taken && indirect).asUInt
    callWrites := meta.callWrites + (taken && push).asUInt
    retWrites := meta.retWrites + (taken && pop).asUInt
    for (i <- 0 until 16) lhrHead(i) := meta.lhrHead(i)
    for (i <- 0 until 6) {
      calls((meta.callHead - (i + 1).U)(6, 0)) := meta.callLow12(2 * i + 1, 2 * i)
      rets((meta.retHead - (i + 1).U)(6, 0)) := meta.retLow12(2 * i + 1, 2 * i)
    }
    when(taken && indirect) { ith(meta.ithHead) := target(2, 1) }
    when(taken && push) {
      val newLow = Cat(meta.callLow12, 0.U(2.W)) ^ fold14(pc)
      for (i <- 0 until 7) calls((meta.callHead - i.U)(6, 0)) := newLow(2 * i + 1, 2 * i)
    }
    when(taken && pop) {
      val newLow = Cat(meta.retLow12, 0.U(2.W)) ^ fold14(pc)
      for (i <- 0 until 7) rets((meta.retHead - i.U)(6, 0)) := newLow(2 * i + 1, 2 * i)
    }
    when(taken && h2p) {
      lhr(h2pIndex)(meta.lhrHead(h2pIndex)(2, 0)) := target(2, 1)
      lhrHead(h2pIndex) := meta.lhrHead(h2pIndex) + 1.U
    }
  }

  private def restoreProfileWindow(meta: HintSnapshot, row: HintProfileRecord): Unit = {
    for (i <- 0 until 32) {
      ith((meta.ithHead - (i + 1).U)(6, 0)) := row.ithLow64(2 * i + 1, 2 * i)
      calls((meta.callHead - (i + 1).U)(6, 0)) := row.callLow64(2 * i + 1, 2 * i)
      rets((meta.retHead - (i + 1).U)(6, 0)) := row.retLow64(2 * i + 1, 2 * i)
    }
    for (j <- 0 until 16; i <- 0 until 4) {
      lhr(j)((meta.lhrHead(j) - (i + 1).U)(2, 0)) := row.lhrLow8(j)(2 * i + 1, 2 * i)
    }
  }

  when((io.enabled || io.profile) && io.s1Fire && !io.redirect.valid && !io.s3Override) {
    val pred = io.s1Prediction
    when(pred.taken && pred.attribute.needIttage) {
      ith(ithHead) := pred.target.toUInt(2, 1)
      ithHead := ithHead + 1.U
      ithWrites := ithWrites + 1.U
    }
    when(pred.taken && pred.attribute.hasPush) {
      val newLow = Cat(snapshot.callLow12, 0.U(2.W)) ^ fold14(io.s1CfiPc)
      for (i <- 0 until 7) calls((callHead - i.U)(6, 0)) := newLow(2 * i + 1, 2 * i)
      callHead := callHead + 1.U
      callWrites := callWrites + 1.U
    }
    when(pred.taken && pred.attribute.hasPop) {
      val newLow = Cat(snapshot.retLow12, 0.U(2.W)) ^ fold14(io.s1CfiPc)
      for (i <- 0 until 7) rets((retHead - i.U)(6, 0)) := newLow(2 * i + 1, 2 * i)
      retHead := retHead + 1.U
      retWrites := retWrites + 1.U
    }
    when(s1HintEvent) {
      lhr(camIndex)(lhrHead(camIndex)(2, 0)) := pred.target.toUInt(2, 1)
      lhrHead(camIndex) := lhrHead(camIndex) + 1.U
    }
  }
  when((io.enabled || io.profile) && io.s3Override && !io.redirect.valid) {
    val pred = io.s3Prediction
    when(io.profile) { restoreProfileWindow(s3Snapshot, profileRows(io.s3Ptr.value)) }
    restoreAndApply(s3Snapshot, io.s3CfiPc, pred.target.toUInt,
      pred.taken, pred.attribute.needIttage, pred.attribute.hasPush,
      pred.attribute.hasPop, s3HintEvent, s3CamIndex)
  }
  when((io.enabled || io.profile) && io.redirect.valid) {
    val redirect = io.redirect.bits
    val reapply = redirect.reapply
    val exactHit = VecInit((0 until 16).map(i => cam(i)(51) &&
      canonical(cam(i)(46, 0)) === redirect.cfiPc.toUInt)).asUInt
    val profilePtr = Mux(reapply, (redirect.newFtqIdx.value - 1.U)(5, 0),
      redirect.newFtqIdx.value)
    when(io.profile) {
      restoreProfileWindow(redirect.meta.hint, profileRows(profilePtr))
    }
    restoreAndApply(redirect.meta.hint, redirect.cfiPc.toUInt, redirect.target.toUInt,
      reapply && redirect.taken, redirect.attribute.needIttage,
      redirect.attribute.hasPush, redirect.attribute.hasPop,
      exactHit.orR && reapply && redirect.taken && redirect.attribute.needIttage,
      PriorityEncoder(exactHit))
  }

  // S1 reserves a record before the history write; S3 confirms the actual CFI.
  val freeIndex = PriorityEncoder(free)
  when(io.enabled && io.s1Fire && candidate && !io.redirect.valid && !io.s3Override) {
    assert(io.canAccept)
    records(freeIndex) := 0.U.asTypeOf(new HintRecord)
    records(freeIndex).valid := true.B
    records(freeIndex).ptr := io.s1Ptr
    records(freeIndex).camIndex := camIndex
  }
  when(io.s3Fire) {
    for (i <- 0 until 16) {
      when(records(i).valid && records(i).ptr === io.s3Ptr) {
        when(s3HintEvent) {
          records(i).confirmed := true.B
          records(i).contextHit := contextHit
          records(i).setway := selectedSetway
          records(i).shadowTarget := io.s3Lookup.target
          records(i).baselineTarget := io.s3BaselineTarget
          records(i).usedHint := io.s3UsedHint
          records(i).confAtPrediction := selectedConf
        }.otherwise {
          records(i).valid := false.B
        }
      }
    }
  }
  for (r <- io.resolve) {
    for (i <- 0 until 16) {
      when(r.valid && records(i).valid && records(i).confirmed &&
        records(i).ptr === r.bits.ftqPtr &&
        canonical(cam(records(i).camIndex)(46, 0)) === r.bits.cfiPc) {
        records(i).ready := true.B
        records(i).measure := io.measureActive && r.bits.eligible
        records(i).correct := records(i).shadowTarget === r.bits.target
        records(i).actualTarget := r.bits.target
        records(i).actualValid := true.B
      }
    }
  }
  val replayHits = VecInit(records.map(r => r.valid && r.confirmed &&
    r.actualValid && r.ptr === io.commit.bits))
  val replayRecord = records(PriorityEncoder(replayHits))
  io.replayEvent.valid := io.enabled && io.commit.valid && replayHits.asUInt.orR
  io.replayEvent.bits.pc := canonical(cam(replayRecord.camIndex)(46, 0))
  io.replayEvent.bits.actual := replayRecord.actualTarget
  io.replayEvent.bits.baselineTarget := replayRecord.baselineTarget
  io.replayEvent.bits.finalTarget := Mux(replayRecord.usedHint,
    replayRecord.shadowTarget, replayRecord.baselineTarget)
  io.replayEvent.bits.usedHint := replayRecord.usedHint
  io.replayEvent.bits.contextHit := replayRecord.contextHit
  io.replayEvent.bits.confAtPrediction := replayRecord.confAtPrediction
  when(io.commit.valid) {
    for (i <- 0 until 16) {
      when(records(i).valid && records(i).ptr === io.commit.bits) {
        records(i).committed := true.B
        // A predicted candidate may never execute because an earlier CFI in
        // the same FTQ entry wins. Commit still releases its metadata, but
        // must not turn the speculative candidate into a scored branch.
        val candidatePc = canonical(cam(records(i).camIndex)(46, 0))
        val resolvesCandidateNow = io.resolve.map(r =>
          r.valid && r.bits.ftqPtr === records(i).ptr && r.bits.cfiPc === candidatePc
        ).reduce(_ || _)
        when(!records(i).ready && !resolvesCandidateNow) {
          records(i).ready := true.B
          records(i).measure := false.B
        }
      }
    }
    for (i <- 0 until 32) {
      val idx = (confHead + i.U)(4, 0)
      when(i.U < (confTail - confHead) && confLog(idx).ptr === io.commit.bits) {
        confLog(idx).committed := true.B
      }
    }
  }
  for (i <- 0 until 16) {
    when(records(i).valid && records(i).committed && records(i).enqueued) {
      records(i).valid := false.B
    }
  }
  when(io.s3Override) {
    for (i <- 0 until 16) {
      when(records(i).valid && records(i).ptr > io.s3Ptr) { records(i).valid := false.B }
    }
  }
  when(io.redirect.valid) {
    for (i <- 0 until 16) {
      when(records(i).valid && records(i).ptr >= io.redirect.bits.newFtqIdx) {
        records(i).valid := false.B
      }
    }
  }

  val oldest = VecInit((0 until 16).map { i =>
    records(i).valid && records(i).confirmed && !records(i).enqueued &&
      !(0 until 16).filter(_ != i).map(j =>
        records(j).valid && !records(j).enqueued && records(j).ptr < records(i).ptr
      ).reduce(_ || _)
  })
  val oldestIndex = PriorityEncoder(oldest)
  val oldestRecord = records(oldestIndex)
  val drain = oldest.asUInt.orR && oldestRecord.ready && !rollback && !io.redirect.valid
  val confFull = (confTail - confHead) === 32.U
  val priorConf = forwardedConf(oldestRecord.setway, context(oldestRecord.setway)(23, 22))
  val nextConf = Mux(oldestRecord.correct, Mux(priorConf === 3.U, 3.U, priorConf + 1.U),
    Mux(priorConf === 0.U, 0.U, priorConf - 1.U))
  when(drain) {
    records(oldestIndex).enqueued := true.B
    when(oldestRecord.contextHit && oldestRecord.measure) {
      when(confFull) {
        confDropped := confDropped + 1.U
      }.otherwise {
        confLog(confTail(4, 0)).ptr := oldestRecord.ptr
        confLog(confTail(4, 0)).setway := oldestRecord.setway
        confLog(confTail(4, 0)).oldConf := priorConf
        confLog(confTail(4, 0)).newConf := nextConf
        confLog(confTail(4, 0)).applied := false.B
        confLog(confTail(4, 0)).committed := oldestRecord.committed ||
          (io.commit.valid && io.commit.bits === oldestRecord.ptr)
        confTail := confTail + 1.U
      }
    }
  }

  // A redirect reverses applied younger writes from newest to oldest. Lookups stall
  // until the physical single-port table and forwarded state agree again.
  when(io.enabled && io.redirect.valid) {
    rollback := true.B
    rollbackBoundary := io.redirect.bits.newFtqIdx
  }
  val lastIndex = (confTail - 1.U)(4, 0)
  val last = confLog(lastIndex)
  val undo = rollback && confTail =/= confHead && last.ptr >= rollbackBoundary
  val applyUpdate = !clearing && !rollback && !io.redirect.valid &&
    !readContext && !(io.load.valid && io.load.bits.kind === 2.U) &&
    confWrite =/= confTail
  val retire = confHead =/= confTail && confLog(confHead(4, 0)).applied &&
    confLog(confHead(4, 0)).committed && !rollback
  when(rollback) {
    when(undo) {
      when(last.applied) {
        val row = context(last.setway)
        context(last.setway) := Cat(row(24), last.oldConf, row(21, 0))
      }
      confTail := confTail - 1.U
      when(confWrite === confTail) { confWrite := confWrite - 1.U }
    }.otherwise {
      rollback := false.B
    }
  }.elsewhen(applyUpdate) {
    val req = confLog(confWrite(4, 0))
    val row = context(req.setway)
    context(req.setway) := Cat(row(24), req.newConf, row(21, 0))
    confLog(confWrite(4, 0)).applied := true.B
    confWrite := confWrite + 1.U
  }
  when(retire) { confHead := confHead + 1.U }
  when(retire) {
    val oldValue = confLog(confHead(4, 0)).oldConf
    val newValue = confLog(confHead(4, 0)).newConf
    when(newValue > oldValue) { confUpCount := confUpCount + 1.U }
    when(newValue < oldValue) { confDownCount := confDownCount + 1.U }
  }

  io.load.ready := !clearing && !rollback && (!readContext || io.load.bits.kind =/= 2.U)
  when(clearing) {
    context(clearIndex) := 0.U
    clearIndex := clearIndex + 1.U
    when(clearIndex === 4095.U) { clearing := false.B }
  }
  when(io.load.fire) {
    switch(io.load.bits.kind) {
      is(0.U) {
        clearing := true.B
        clearIndex := 0.U
        cam.foreach(_ := 0.U)
        slots.foreach(_ := 0.U)
        ith.foreach(_ := 0.U)
        calls.foreach(_ := 0.U)
        rets.foreach(_ := 0.U)
        lhr.foreach(_.foreach(_ := 0.U))
        ithHead := 0.U
        callHead := 0.U
        retHead := 0.U
        ithWrites := 0.U
        callWrites := 0.U
        retWrites := 0.U
        lhrHead.foreach(_ := 0.U)
        records.foreach(_.valid := false.B)
        confHead := 0.U
        confWrite := 0.U
        confTail := 0.U
        confDropped := 0.U
      }
      is(1.U) { when(io.load.bits.index < 16.U) { cam(io.load.bits.index(3, 0)) := io.load.bits.data(51, 0) } }
      is(2.U) { context(io.load.bits.index) := io.load.bits.data(24, 0) }
      is(3.U) { when(io.load.bits.index < 32.U) { slots(io.load.bits.index(4, 0)) := io.load.bits.data(46, 0) } }
    }
  }
}
