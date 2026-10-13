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
  val lhrLow8 = Vec(16, UInt(8.W))
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
  val offset = UInt(6.W)
  val mispredict = Bool()
  val sourceBackend = Bool()
}

class HintProfileTraceEvent extends Bundle {
  val eventKind = UInt(3.W)
  val cycle = UInt(64.W)
  val queryCycle = UInt(64.W)
  val ftqTag = UInt(7.W)
  val offset = UInt(6.W)
  val pc = UInt(64.W)
  val target = UInt(64.W)
  val mispredict = Bool()
  val survived = Bool()
  val observed = Bool()
  val resolved = Bool()
  val sourceBackend = Bool()
  val eligible = Bool()
}

class HintProfileEvent extends Bundle {
  val ftqTag = UInt(7.W)
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
  val ftqTag = UInt(7.W)
  val queryCycle = UInt(64.W)
  val ith = UInt(28.W)
  val lhr = UInt(28.W)
  val call = UInt(28.W)
  val ret = UInt(28.W)
  val mask = UInt(4.W)
  val contextHash = UInt(28.W)
  val pc = UInt(64.W)
  val actual = UInt(64.W)
  val baselineTarget = UInt(64.W)
  val finalTarget = UInt(64.W)
  val usedHint = Bool()
  val contextHit = Bool()
  val confAtPrediction = UInt(2.W)
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
  val lookupPortFallback = UInt(32.W)
  val metadataFallback = UInt(32.W)
  val rollbackFallback = UInt(32.W)
  val clearingFallback = UInt(32.W)
  val s3Confirmed = UInt(32.W)
  val recordAllocate = UInt(32.W)
  val lhrWrites = UInt(32.W)
  val confFullWait = UInt(32.W)
  val confApply = UInt(32.W)
  val confUndo = UInt(32.W)
  val confAccepted = UInt(32.W)
  val confCanceled = UInt(32.W)
  val confRetired = UInt(32.W)
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
  val queryCycle = UInt(64.W)
  val components = Vec(4, UInt(28.W))
  val mask = UInt(4.W)
  val contextHash = UInt(28.W)
  val camIndex = UInt(4.W)
  val confirmed = Bool()
  val ready = Bool()
  val committed = Bool()
  val commitMeasure = Bool()
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

class HintConfEvent extends Bundle {
  val setway = UInt(12.W)
  val correct = Bool()
}

class Hint(implicit p: Parameters) extends BpuModule {
  class HintIO extends Bundle {
    val load = Flipped(Decoupled(new HintLoadCmd))
    // Offline readiness only. The DPI loader latches completion; transient
    // execution/rollback pressure must never toggle its running mode.
    val tableReady = Output(Bool())
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
    val debugLhrFull = Option.when(!env.FPGAPlatform)(Output(Bool()))
    val debugMetaFull = Option.when(!env.FPGAPlatform)(Output(Bool()))
    val debugSelectedConf = Option.when(!env.FPGAPlatform)(Output(UInt(2.W)))
    val debugMetaHead = Option.when(!env.FPGAPlatform)(Output(UInt(5.W)))
    val debugMetaDrain = Option.when(!env.FPGAPlatform)(Output(UInt(5.W)))
    val debugMetaTail = Option.when(!env.FPGAPlatform)(Output(UInt(5.W)))
    val debugConfHead = Option.when(!env.FPGAPlatform)(Output(UInt(5.W)))
    val debugConfWrite = Option.when(!env.FPGAPlatform)(Output(UInt(5.W)))
    val debugConfTail = Option.when(!env.FPGAPlatform)(Output(UInt(5.W)))
    val debugRollback = Option.when(!env.FPGAPlatform)(Output(Bool()))
    val s3Lookup = Output(new HintLookup)
    val s3Snapshot = Output(new HintSnapshot)
    val confDropped = Output(UInt(32.W))
    val traceCommit = Input(Valid(new HintTraceCommit))
    val profileEvent = Output(Valid(new HintTraceFoldEvent))
    val profileTrace = Option.when(!env.FPGAPlatform)(Output(Vec(backendParams.BrhCnt + 5, Valid(new HintProfileTraceEvent))))
    val replayEvent = Output(Valid(new HintReplayEvent))
    val stats = Output(new HintStats)
  }
  val io = IO(new HintIO)

  private val cam = RegInit(VecInit(Seq.fill(16)(0.U(52.W))))
  private val contextTable = Module(new HintContextTable)
  private val slotTable = Module(new HintSlotTable)
  private val clearing = RegInit(true.B)
  // Decode Clear acceptance without feeding io.load.ready back through the
  // leaf clear input. Other load kinds use leaf ready, which depends on clear.
  private val clearFire = WireDefault(false.B)
  contextTable.io.clear := clearFire
  slotTable.io.clear := clearFire

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
  private val metaHead = RegInit(0.U(5.W))
  private val metaTail = RegInit(0.U(5.W))
  // Commit-qualified events only. The active consumer remains the FIFO head,
  // so the total physical event capacity is exactly sixteen.
  private val confEvents = Reg(Vec(16, new HintConfEvent))
  private val confHead = RegInit(0.U(5.W))
  private val confTail = RegInit(0.U(5.W))
  private val confOccupancy = (confTail - confHead)(4, 0)
  private val trainIdle :: trainReadResponse :: trainWrite :: Nil = Enum(3)
  private val trainState = RegInit(trainIdle)
  private val trainSetway = Reg(UInt(12.W))
  private val trainCorrect = Reg(Bool())
  private val trainRawConf = Reg(UInt(2.W))
  private val trainNextConf = Reg(UInt(2.W))
  if (!env.FPGAPlatform) {
    val pendingObserver = Module(new xiangshan.frontend.ftq.HintPendingDpi)
    pendingObserver.clock := clock
    pendingObserver.pending := confOccupancy
    pendingObserver.outstanding := confOccupancy
    pendingObserver.meta := PopCount(records.map(r => r.valid && r.ready && r.committed &&
      r.measure && r.commitMeasure && r.contextHit))
  }
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
  io.debugMetaHead.foreach(_ := metaHead)
  io.debugMetaDrain.foreach(_ := metaHead)
  io.debugMetaTail.foreach(_ := metaTail)
  io.debugConfHead.foreach(_ := confHead)
  io.debugConfWrite.foreach(_ := confHead)
  io.debugConfTail.foreach(_ := confTail)
  io.debugRollback.foreach(_ := false.B)

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
  for (i <- 0 until 16) snapshot.lhrLow8(i) := ringLow(lhr(i), lhrHead(i), 4, 3)
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

  // Ordinary S1 is independent of Hint capacity and table maintenance.
  val metaFull = (metaTail - metaHead) === 16.U
  val confFull = confOccupancy === 16.U
  val tableBusy = clearing || io.redirect.valid || io.load.valid || metaFull
  io.canAccept := true.B
  io.debugLhrFull.foreach(_ := false.B)
  io.debugMetaFull.foreach(_ := metaFull)
  contextTable.io.read.valid := io.s1Fire && candidate && !tableBusy
  val lookupFire = contextTable.io.read.fire

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
  contextTable.io.read.bits := set
  val s2ReadValid = RegEnable(lookupFire, false.B, io.s1Fire)
  val s3ReadValid = RegInit(false.B)
  val s3SlotValid = RegInit(false.B)
  when(io.s2Fire && !io.s1Fire) { s2ReadValid := false.B }
  when(io.redirect.valid || io.s3Override || clearFire) { s2ReadValid := false.B }
  when(io.s3Fire && !io.s2Fire) { s3ReadValid := false.B; s3SlotValid := false.B }
  when(io.redirect.valid || clearFire) { s3ReadValid := false.B; s3SlotValid := false.B }
  val s2Candidate = RegEnable(candidate, io.s1Fire)
  val s2Pc = RegEnable(candidatePc, io.s1Fire)
  val s2CamIndex = RegEnable(camIndex, io.s1Fire)
  val s2Set = RegEnable(set, io.s1Fire)
  val s2Tag = RegEnable(tag, io.s1Fire)
  // The leaf holds only prediction responses. Training reads/writes cannot
  // overwrite a stalled S2's raw SRAM confidence.
  val s2Rows = contextTable.io.rows
  when(contextTable.io.respValid && s2ReadValid && !io.redirect.valid) {
    assert(contextTable.io.readSet === s2Set)
  }
  val s2Components = RegEnable(VecInit(Seq(ithComponent, lhrComponent, callComponent, retComponent)), io.s1Fire)
  val s3Components = RegEnable(s2Components, io.s2Fire)
  val s2Mask = RegEnable(candidateMask, io.s1Fire)
  val s3Mask = RegEnable(s2Mask, io.s2Fire)
  val s2Key = RegEnable(key, io.s1Fire)
  val s3Key = RegEnable(s2Key, io.s2Fire)
  val queryClock = RegInit(0.U(64.W))
  queryClock := queryClock + 1.U
  val s2QueryCycle = RegEnable(queryClock, io.s1Fire)
  val s3QueryCycle = RegEnable(s2QueryCycle, io.s2Fire)
  val s3Candidate = RegEnable(s2Candidate, io.s2Fire)
  val s3Pc = RegEnable(s2Pc, io.s2Fire)
  val s3CamIndex = RegEnable(s2CamIndex, io.s2Fire)
  val s3Set = RegEnable(s2Set, io.s2Fire)
  val s2Way0Hit = s2Rows(0)(24) && s2Rows(0)(16, 0) === s2Tag
  val s2Way1Hit = s2Rows(1)(24) && s2Rows(1)(16, 0) === s2Tag
  val s2SelectedWay = !s2Way0Hit && s2Way1Hit
  val s2SelectedRow = Mux(s2SelectedWay, s2Rows(1), s2Rows(0))
  slotTable.io.read.valid := io.s2Fire && s2ReadValid && s2Candidate &&
    (s2Way0Hit || s2Way1Hit) && !clearing && !clearFire && !io.redirect.valid && !io.s3Override
  slotTable.io.read.bits := s2SelectedRow(21, 17)
  when(io.s2Fire) {
    s3ReadValid := s2ReadValid && !io.redirect.valid && !io.s3Override && !clearFire
    s3SlotValid := slotTable.io.read.fire
  }
  val selectedWay = RegEnable(s2SelectedWay, io.s2Fire)
  val selectedSlot = RegEnable(s2SelectedRow(21, 17), io.s2Fire)
  val selectedRawConf = RegEnable(s2SelectedRow(23, 22), io.s2Fire)
  val selectedSetway = Cat(s3Set, selectedWay)
  when(slotTable.io.respValid && s3SlotValid && !io.redirect.valid) {
    assert(slotTable.io.readSlot === selectedSlot)
  }
  val contextHit = io.enabled && s3ReadValid && s3SlotValid && s3Candidate
  val targetHold = RegEnable(slotTable.io.target, slotTable.io.respValid)
  val selectedTarget = Mux(slotTable.io.respValid, slotTable.io.target, targetHold)

  // No log, associative bypass, or write broadcast changes a prediction's
  // confidence. This value is exactly the raw SRAM value from its S1 query.
  val selectedConf = selectedRawConf
  io.debugSelectedConf.foreach(_ := selectedConf)
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
  io.s3Lookup.usable := contextHit && selectedConf =/= 0.U && !metaFull &&
    !clearing && !io.redirect.valid && io.s3Prediction.taken &&
    io.s3Prediction.attribute.needIttage && !io.s3Prediction.attribute.isReturn &&
    io.s3CfiPc === s3Pc
  io.s3Lookup.set := s3Set
  io.s3Lookup.way := selectedWay
  io.s3Lookup.target := canonical(selectedTarget)

  val s1Exact = VecInit((0 until 16).map(i => cam(i)(51) && canonical(cam(i)(46, 0)) === io.s1CfiPc))
  val s1ExactIndex = PriorityEncoder(s1Exact)
  val s1HintEvent = s1Exact.asUInt.orR && io.s1Prediction.taken && io.s1Prediction.attribute.needIttage
  val s3HintEvent = s3Candidate && io.s3Prediction.taken &&
    io.s3Prediction.attribute.needIttage && !io.s3Prediction.attribute.isReturn && io.s3CfiPc === s3Pc
  val s3Exact = VecInit((0 until 16).map(i => cam(i)(51) && canonical(cam(i)(46, 0)) === io.s3CfiPc))
  when(io.enabled && io.s3Fire && io.s3UsedHint && io.s3Override) {
    overrideCount := overrideCount + 1.U
  }

  // The v4 trace observes actual pipeline data without feeding any predictor control.
  // S1 owns the pre-update components; S3 uses the saved S1 query cycle.
  if (!env.FPGAPlatform) {
    val traceStartS2 = RegEnable(io.s1StartPc, io.s1Fire)
    val traceStartS3 = RegEnable(traceStartS2, io.s2Fire)
    for (stage <- 0 until 2) {
      val observer = Module(new HintQueryV4Dpi)
      observer.io.clock := clock
      observer.io.stage := stage.U
      if (stage == 0) {
        observer.io.valid := !reset.asBool && io.profile && io.s1Fire
        observer.io.ftqTag := io.s1Ptr.asUInt
        observer.io.queryCycle := queryClock
        observer.io.startPc := io.s1StartPc
        observer.io.candidatePc := candidatePc
        observer.io.pcId := camIndex
        observer.io.candidate := candidate
        observer.io.lookupFire := lookupFire
        observer.io.s3Eligible := false.B
        observer.io.admission := !tableBusy
        observer.io.ith := ithComponent
        observer.io.lhr := lhrComponent
        observer.io.call := callComponent
        observer.io.ret := retComponent
      } else {
        observer.io.valid := !reset.asBool && io.profile && io.s3Fire
        observer.io.ftqTag := io.s3Ptr.asUInt
        observer.io.queryCycle := s3QueryCycle
        observer.io.startPc := traceStartS3
        observer.io.candidatePc := s3Pc
        observer.io.pcId := s3CamIndex
        observer.io.candidate := s3Candidate
        observer.io.lookupFire := s3ReadValid
        observer.io.s3Eligible := s3HintEvent
        observer.io.admission := !metaFull && !clearing && !io.redirect.valid
        observer.io.ith := s3Components(0)
        observer.io.lhr := s3Components(1)
        observer.io.call := s3Components(2)
        observer.io.ret := s3Components(3)
      }
    }
  }

  val backendBoundary = Mux(io.redirect.bits.reapply,
    io.redirect.bits.newFtqIdx - 1.U, io.redirect.bits.newFtqIdx)
  def pastCutoff(pcWord: UInt, cutoffWord: UInt, inclusive: Bool): Bool =
    pcWord > cutoffWord || (inclusive && pcWord === cutoffWord)
  def backendKills(ptr: FtqPtr, pcWord: UInt): Bool =
    ptr > backendBoundary || (ptr === backendBoundary &&
      (!io.redirect.bits.reapply || pastCutoff(pcWord, io.redirect.bits.cfiPc.toUInt(47, 1),
         io.redirect.bits.flushItself)))
  def s3Kills(ptr: FtqPtr, pcWord: UInt): Bool =
    ptr > io.s3Ptr || (ptr === io.s3Ptr && io.s3Prediction.taken && pcWord > io.s3CfiPc(47, 1))
  def killedNow(ptr: FtqPtr, pcWord: UInt): Bool =
    Mux(io.redirect.valid, backendKills(ptr, pcWord), io.s3Override && s3Kills(ptr, pcWord))
  // FTQ owns the existing snapshot. Reconstruct committed profiling components
  // directly from its heads and tiny restoration windows, without profileRows.
  val trace = io.traceCommit.bits
  val traceActualHits = VecInit((0 until 16).map(i => cam(i)(51) &&
    canonical(cam(i)(46, 0)) === trace.actualPc))
  val traceActualIndex = PriorityEncoder(traceActualHits)
  val traceEndWide = Cat(0.U(1.W), trace.queryStartPc) + 64.U(65.W)
  val traceWindowEnd = Cat(traceEndWide(63, 5), 0.U(5.W))
  val traceWindowHits = VecInit((0 until 16).map { i =>
    val pc = canonical(cam(i)(46, 0))
    cam(i)(51) && !traceEndWide(64) && pc >= trace.queryStartPc && pc < traceWindowEnd
  })
  val traceQueryValid = trace.queryValid && traceActualHits.asUInt.orR &&
    traceWindowHits.asUInt.orR && PriorityEncoder(traceWindowHits) === traceActualIndex
  def restoredLow64(cells: Vec[UInt], head: UInt, low12: UInt): UInt =
    Cat((0 until 32).reverse.map(i =>
      if (i < 6) low12(2 * i + 1, 2 * i)
      else cells((head - (i + 1).U)(6, 0))))
  io.profileEvent.valid := io.profile && io.traceCommit.valid && trace.eligible && traceActualHits.asUInt.orR
  io.profileEvent.bits.actualPc := trace.actualPc
  io.profileEvent.bits.actualTarget := trace.actualTarget
  io.profileEvent.bits.baselineMiss := trace.baselineMiss
  io.profileEvent.bits.queryStartPc := trace.queryStartPc
  io.profileEvent.bits.ftqTag := trace.ftqTag
  io.profileEvent.bits.pcId := traceActualIndex
  io.profileEvent.bits.queryValid := traceQueryValid
  io.profileEvent.bits.eligible := trace.eligible
  io.profileEvent.bits.queryCycle.zip(trace.queryCycle).foreach { case (dst, src) => dst := src }
  io.profileEvent.bits.ith := fold28(ringLow(ith, trace.snapshot.ithHead, 32, 7))
  io.profileEvent.bits.lhr := fold28(trace.snapshot.lhrLow8(traceActualIndex))
  io.profileEvent.bits.call := fold28(restoredLow64(calls, trace.snapshot.callHead, trace.snapshot.callLow12))
  io.profileEvent.bits.ret := fold28(restoredLow64(rets, trace.snapshot.retHead, trace.snapshot.retLow12))
  when(io.profile && io.traceCommit.valid) {
    assert((ithWrites - trace.snapshot.ithWrites) <= 96.U)
    assert((callWrites - trace.snapshot.callWrites) <= 96.U)
    assert((retWrites - trace.snapshot.retWrites) <= 96.U)
  }
  // The former row-dependent diagnostics are replaced by the FTQ trace module.
  io.profileTrace.foreach(events => events := 0.U.asTypeOf(events))

  // Snapshot restoration rewinds every global path, even when this FTQ entry is not an H2P branch.
  private def restoreAndApply(meta: HintSnapshot, pc: UInt, target: UInt,
                              taken: Bool, indirect: Bool, push: Bool, pop: Bool,
                              h2p: Bool, h2pIndex: UInt): Unit = {
    assert((ithWrites - meta.ithWrites) <= 96.U)
    assert((callWrites - meta.callWrites) <= 96.U)
    assert((retWrites - meta.retWrites) <= 96.U)
    ithHead := meta.ithHead + (taken && indirect).asUInt
    callHead := meta.callHead + (taken && push).asUInt
    retHead := meta.retHead + (taken && pop).asUInt
    ithWrites := meta.ithWrites + (taken && indirect).asUInt
    callWrites := meta.callWrites + (taken && push).asUInt
    retWrites := meta.retWrites + (taken && pop).asUInt
    for (j <- 0 until 16) {
      lhrHead(j) := meta.lhrHead(j)
      for (i <- 0 until 4) lhr(j)((meta.lhrHead(j) - (i + 1).U)(2, 0)) := meta.lhrLow8(j)(2*i+1, 2*i)
    }
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
      lhr(s1ExactIndex)(lhrHead(s1ExactIndex)(2, 0)) := pred.target.toUInt(2, 1)
      lhrHead(s1ExactIndex) := lhrHead(s1ExactIndex) + 1.U
    }
  }
  when((io.enabled || io.profile) && io.s3Override && !io.redirect.valid) {
    val pred = io.s3Prediction
    restoreAndApply(s3Snapshot, io.s3CfiPc, pred.target.toUInt,
      pred.taken, pred.attribute.needIttage, pred.attribute.hasPush,
      pred.attribute.hasPop, s3Exact.asUInt.orR && pred.attribute.needIttage, PriorityEncoder(s3Exact))
  }
  when((io.enabled || io.profile) && io.redirect.valid) {
    val redirect = io.redirect.bits
    val reapply = redirect.reapply
    val applySurvivingCfi = reapply && !redirect.flushItself && redirect.taken
    val exactHit = VecInit((0 until 16).map(i => cam(i)(51) &&
      canonical(cam(i)(46, 0)) === redirect.cfiPc.toUInt)).asUInt
    restoreAndApply(redirect.meta.hint, redirect.cfiPc.toUInt, redirect.target.toUInt,
      applySurvivingCfi, redirect.attribute.needIttage,
      redirect.attribute.hasPush, redirect.attribute.hasPop,
      exactHit.orR && applySurvivingCfi && redirect.attribute.needIttage,
      PriorityEncoder(exactHit))
  }

  // The S3 actual CFI owns metadata; no speculative S1 reservation.
  val allocate = io.enabled && io.s3Fire && s3HintEvent && contextHit &&
    !metaFull && !clearing && !io.redirect.valid
  when(allocate) {
    assert(!records(metaTail(3, 0)).valid)
    val r = records(metaTail(3, 0))
    r := 0.U.asTypeOf(new HintRecord)
    r.valid := true.B
    r.ptr := io.s3Ptr
    r.queryCycle := s3QueryCycle
    r.components := s3Components
    r.mask := s3Mask
    r.contextHash := s3Key
    r.camIndex := s3CamIndex
    r.confirmed := true.B
    r.contextHit := contextHit
    r.setway := selectedSetway
    r.shadowTarget := io.s3Lookup.target
    r.baselineTarget := io.s3BaselineTarget
    r.usedHint := io.s3UsedHint
    r.confAtPrediction := selectedConf
    metaTail := metaTail + 1.U
  }
  for (r <- io.resolve) {
    for (i <- 0 until 16) {
      when(r.valid && records(i).valid && records(i).confirmed && !records(i).committed &&
        records(i).ptr === r.bits.ftqPtr &&
        r.bits.sourceBackend && canonical(cam(records(i).camIndex)(46, 0)) === r.bits.cfiPc &&
        !killedNow(records(i).ptr, cam(records(i).camIndex)(46, 0))) {
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
  io.replayEvent.valid := io.enabled && io.commit.valid && replayHits.asUInt.orR &&
    !killedNow(replayRecord.ptr, cam(replayRecord.camIndex)(46, 0))
  io.replayEvent.bits.ftqTag := Cat(replayRecord.ptr.flag, replayRecord.ptr.value)
  io.replayEvent.bits.queryCycle := replayRecord.queryCycle
  io.replayEvent.bits.ith := replayRecord.components(0)
  io.replayEvent.bits.lhr := replayRecord.components(1)
  io.replayEvent.bits.call := replayRecord.components(2)
  io.replayEvent.bits.ret := replayRecord.components(3)
  io.replayEvent.bits.mask := replayRecord.mask
  io.replayEvent.bits.contextHash := replayRecord.contextHash
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
      val record = records(i)
      val pc = canonical(cam(record.camIndex)(46, 0))
      val resolvesNow = io.resolve.map(r => r.valid && r.bits.sourceBackend &&
        r.bits.ftqPtr === record.ptr && r.bits.cfiPc === pc &&
        !killedNow(record.ptr, pc(47, 1))).reduce(_ || _)
      when(record.valid && record.ptr === io.commit.bits &&
        !killedNow(record.ptr, cam(record.camIndex)(46, 0))) {
        record.committed := true.B
        record.commitMeasure := io.measureActive
        // A candidate bypassed by an earlier CFI must release metadata without
        // becoming an actual branch sample or confidence event.
        when(!record.ready && !resolvesNow) {
          record.ready := true.B
          record.measure := false.B
        }
      }
    }
  }
  val flush = io.redirect.valid || io.s3Override
  val killed = VecInit((0 until 16).map { i =>
    records(i).valid && !records(i).committed &&
      killedNow(records(i).ptr, cam(records(i).camIndex)(46, 0))
  })
  val killCount = PopCount(killed)
  when(flush) {
    when(allocate) { assert(killCount === 0.U) }
    for (i <- 0 until 16) when(killed(i)) { records(i).valid := false.B }
    metaTail := (metaTail - killCount + allocate.asUInt)(4, 0)
  }
  val oldestRecord = records(metaHead(3, 0))
  val drain = metaHead =/= metaTail && oldestRecord.valid && oldestRecord.ready &&
    oldestRecord.committed && !flush
  val needsConf = oldestRecord.contextHit && oldestRecord.actualValid &&
    oldestRecord.measure && oldestRecord.commitMeasure
  val acceptConf = drain && needsConf && !confFull
  val dropConf = drain && needsConf && confFull
  when(drain) {
    records(metaHead(3, 0)).valid := false.B
    metaHead := metaHead + 1.U
    when(acceptConf) {
      confEvents(confTail(3, 0)).setway := oldestRecord.setway
      confEvents(confTail(3, 0)).correct := oldestRecord.correct
      confTail := confTail + 1.U
    }
    when(dropConf) { confDropped := confDropped + 1.U }
  }

  // Strict FIFO read-modify-write: the next head is not read until this head's
  // physical write fires. Repeated same-row events therefore never use stale
  // prediction confidence snapshots. Continuous prediction reads may starve
  // training, which drops only new training when full and never stalls S1.
  contextTable.io.trainRead.valid := trainState === trainIdle && confHead =/= confTail && !clearing
  contextTable.io.trainRead.bits := confEvents(confHead(3, 0)).setway
  when(contextTable.io.trainRead.fire) {
    trainSetway := confEvents(confHead(3, 0)).setway
    trainCorrect := confEvents(confHead(3, 0)).correct
    trainState := trainReadResponse
  }
  when(trainState === trainReadResponse && contextTable.io.trainRespValid) {
    val raw = contextTable.io.trainConf
    trainRawConf := raw
    trainNextConf := Mux(trainCorrect, Mux(raw === 3.U, 3.U, raw + 1.U),
      Mux(raw === 0.U, 0.U, raw - 1.U))
    trainState := trainWrite
  }
  contextTable.io.writeConf.valid := trainState === trainWrite && !clearing
  contextTable.io.writeConf.bits.index := trainSetway
  contextTable.io.writeConf.bits.conf := trainNextConf
  val applyUpdate = contextTable.io.writeConf.fire
  when(applyUpdate) {
    confHead := confHead + 1.U
    trainState := trainIdle
    when(trainNextConf > trainRawConf) { confUpCount := confUpCount + 1.U }
    when(trainNextConf < trainRawConf) { confDownCount := confDownCount + 1.U }
  }
  val trainingEmpty = confHead === confTail && trainState === trainIdle
  val loaderQuiescent = metaHead === metaTail && trainingEmpty &&
    !s2ReadValid && !s3ReadValid && !s3SlotValid
  assert((metaTail - metaHead)(4, 0) <= 16.U)
  assert(confOccupancy <= 16.U)
  when(trainState =/= trainIdle) { assert(confHead =/= confTail) }
  when(io.load.valid) { assert(!io.enabled && !io.profile) }
  when(io.load.fire) { assert(loaderQuiescent) }
  when(clearing) { assert(!io.enabled) }
  def count(event: Bool): UInt = {
    val r = RegInit(0.U(32.W))
    when(event) { r := r + 1.U }
    r
  }
  io.stats.lookupPortFallback := count(io.enabled && io.s1Fire && candidate && tableBusy && !clearing)
  io.stats.metadataFallback := count(io.enabled && io.s3Fire && s3HintEvent && contextHit && metaFull)
  io.stats.rollbackFallback := 0.U
  io.stats.clearingFallback := count(io.enabled && io.s1Fire && candidate && clearing)
  io.stats.s3Confirmed := count(io.enabled && io.s3Fire && s3HintEvent)
  io.stats.recordAllocate := count(allocate)
  val redirectH2p = VecInit((0 until 16).map(i => cam(i)(51) &&
    canonical(cam(i)(46, 0)) === io.redirect.bits.cfiPc.toUInt)).asUInt.orR
  val lhrAppend = (io.s1Fire && s1HintEvent && !flush) ||
    (io.s3Override && !io.redirect.valid && io.s3Prediction.taken &&
      io.s3Prediction.attribute.needIttage && s3Exact.asUInt.orR) ||
    (io.redirect.valid && io.redirect.bits.reapply && !io.redirect.bits.flushItself && io.redirect.bits.taken &&
      io.redirect.bits.attribute.needIttage && redirectH2p)
  io.stats.lhrWrites := count((io.enabled || io.profile) && lhrAppend)
  io.stats.confFullWait := 0.U // FIFO full drops a new event; it never waits.
  io.stats.confApply := count(applyUpdate)
  io.stats.confUndo := 0.U
  io.stats.confAccepted := count(acceptConf)
  io.stats.confCanceled := 0.U
  io.stats.confRetired := count(applyUpdate)

  contextTable.io.load.valid := io.load.valid && io.load.bits.kind === 2.U && !clearing && loaderQuiescent
  contextTable.io.load.bits.index := io.load.bits.index
  contextTable.io.load.bits.data := io.load.bits.data(24, 0)
  slotTable.io.load.valid := io.load.valid && io.load.bits.kind === 3.U && !clearing && loaderQuiescent
  slotTable.io.load.bits.index := io.load.bits.index(4, 0)
  slotTable.io.load.bits.data := io.load.bits.data(46, 0)
  val resetDone = contextTable.io.resetDone && slotTable.io.resetDone
  clearFire := io.load.valid && io.load.bits.kind === 0.U && !clearing && resetDone && loaderQuiescent
  when(io.load.valid && io.load.bits.kind === 0.U) { assert(clearFire === io.load.fire) }
  io.load.ready := !clearing && resetDone && loaderQuiescent && MuxLookup(io.load.bits.kind, true.B)(Seq(
    2.U -> contextTable.io.load.ready,
    3.U -> slotTable.io.load.ready))
  // The explicit busy register spans the Clear acceptance edge even though
  // resetDone still describes the previous image on that edge.
  when(clearing && resetDone && !clearFire) { clearing := false.B }
  io.tableReady := resetDone && !clearing && !io.load.valid &&
    loaderQuiescent
  when(io.load.fire) {
    switch(io.load.bits.kind) {
      is(0.U) {
        clearing := true.B
        cam.foreach(_ := 0.U)
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
        metaHead := 0.U
        metaTail := 0.U
        confHead := 0.U
        confTail := 0.U
        trainState := trainIdle
        confDropped := 0.U
      }
      is(1.U) { when(io.load.bits.index < 16.U) { cam(io.load.bits.index(3, 0)) := io.load.bits.data(51, 0) } }
      is(2.U) { assert(contextTable.io.load.fire) }
      is(3.U) { assert(io.load.bits.index < 32.U); assert(slotTable.io.load.fire) }
    }
  }
}
