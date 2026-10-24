// Copyright (c) 2026
// Synthesizable H2P Hint v3 predictor. The simulation image loader drives io.load.
package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.bpu.{BpuModule, BpuRedirect, Prediction, HalfAlignHelper}
import xiangshan.frontend.ftq.{FtqPtr, HintOwnership}

class HintSnapshot(implicit p: Parameters) extends Bundle {
  val ithHead = UInt(7.W)
  val callHead = UInt(7.W)
  val retHead = UInt(7.W)
  val callLow12 = UInt(12.W)
  val retLow12 = UInt(12.W)
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
  val baselineTarget = UInt(64.W)
  val baselineValid = Bool()
  val offset = UInt(6.W)
  val sourceBackend = Bool()
  val fresh = Bool() // existing FTQ actual-trace state supplies resolve deduplication
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
  val classify = Bool()
  val position = UInt(5.W)
  val isCall = Bool()
  val admitted = Bool()
  val pc = UInt(64.W)
  val camIndex = UInt(4.W)
  val contextHit = Bool()
  val usable = Bool()
  val set = UInt(11.W)
  val way = UInt(1.W)
  val target = UInt(64.W)
}

/** Exactly the architecture's 101-bit local recovery/training payload.
  * Heads are even bit coordinates (0..30), not sample indices.
  * Allocation identity and lifetime belong to the existing FTQ entry.
  */
class HintRecord(implicit p: Parameters) extends Bundle {
  val set = UInt(11.W)
  val way = UInt(1.W)
  val valid = Bool() // completed eligible Context query, independent of conf
  val conf = UInt(2.W)
  val lhrHead = Vec(16, UInt(5.W))
  val lhrTarget = UInt(2.W)
  val lhrSel = UInt(4.W)
}

/** Combinational view of FTQ-owned Hint association; never independently stored. */
class HintOwner(implicit p: Parameters) extends Bundle {
  val valid = Bool()
  val ptr = new FtqPtr
  val index = UInt(4.W)
}

/** Fourteen payload bits; FIFO head/tail occupancy supplies the valid bit. */
class HintConfEvent extends Bundle {
  val setway = UInt(12.W)
  val conf = UInt(2.W)
}

class HintPredictionObservation(implicit p: Parameters) extends Bundle {
  val original = new Prediction
  val camSelected = Bool()
  val ittageHit = Bool()
  val ittageTarget = UInt(64.W)
  val btbTargetValid = Bool()
  val btbTarget = UInt(64.W)
  val fallThroughTarget = UInt(64.W)
}

class Hint(implicit p: Parameters) extends BpuModule with HalfAlignHelper {
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
    val s3Observation = Option.when(!env.FPGAPlatform)(Input(new HintPredictionObservation))
    val redirect = Input(Valid(new BpuRedirect))
    val commit = Input(Valid(new FtqPtr))
    val owners = Input(Vec(FtqSize, new HintOwner))
    val allocation = Output(Valid(UInt(4.W)))
    val resolve = Input(Vec(backendParams.BrhCnt + 1, Valid(new HintResolve)))
    val canAccept = Output(Bool())
    val debugLhrFull = Option.when(!env.FPGAPlatform)(Output(Bool()))
    val debugMetaFull = Option.when(!env.FPGAPlatform)(Output(Bool()))
    val debugSelectedConf = Option.when(!env.FPGAPlatform)(Output(UInt(2.W)))
    val debugMetaHead = Option.when(!env.FPGAPlatform)(Output(UInt(5.W)))
    val debugMetaDrain = Option.when(!env.FPGAPlatform)(Output(UInt(5.W)))
    val debugMetaTail = Option.when(!env.FPGAPlatform)(Output(UInt(5.W)))
    val debugConfHead = Option.when(!env.FPGAPlatform)(Output(UInt(6.W)))
    val debugConfWrite = Option.when(!env.FPGAPlatform)(Output(UInt(6.W)))
    val debugConfTail = Option.when(!env.FPGAPlatform)(Output(UInt(6.W)))
    val debugRollback = Option.when(!env.FPGAPlatform)(Output(Bool()))
    val s3Lookup = Output(new HintLookup)
    val s3Snapshot = Output(new HintSnapshot)
    val confDropped = Output(UInt(32.W))
    val traceCommit = Input(Valid(new HintTraceCommit))
    val profileEvent = Output(Valid(new HintTraceFoldEvent))
    val stats = Output(new HintStats)
  }
  val io = IO(new HintIO)
  require(128 >= 64 + FtqSize, "Hint global ring must protect each live FTQ snapshot")
  require((new HintRecord).getWidth == 101, "Hint queue payload must remain exactly 101 bits")

  private val cam = RegInit(VecInit(Seq.fill(16)(0.U.asTypeOf(new HintCamEntry))))
  require((new HintCamEntry).getWidth == 58)
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
  private val lhr = RegInit(VecInit(Seq.fill(16)(VecInit(Seq.fill(16)(0.U(2.W))))))
  private val ithHead = RegInit(0.U(7.W))
  private val callHead = RegInit(0.U(7.W))
  private val retHead = RegInit(0.U(7.W))
  private val lhrHead = RegInit(VecInit(Seq.fill(16)(0.U(4.W))))

  private val records = RegInit(VecInit(Seq.fill(16)(0.U.asTypeOf(new HintRecord))))
  private val metaHead = RegInit(0.U(5.W))
  private val metaTail = RegInit(0.U(5.W))
  // Resolve produces approximate writes from the prediction-time snapshot.
  // Accepted requests survive subsequent wrong-path cancellation.
  private val confEvents = Reg(Vec(32, new HintConfEvent))
  private val confHead = RegInit(0.U(6.W))
  private val confTail = RegInit(0.U(6.W))
  private val confOccupancy = (confTail - confHead)(5, 0)
  if (!env.FPGAPlatform) {
    val pendingObserver = Module(new xiangshan.frontend.ftq.HintPendingDpi)
    pendingObserver.clock := clock
    pendingObserver.pending := confOccupancy
    pendingObserver.outstanding := confOccupancy
    pendingObserver.meta := 0.U // metadata releases on the existing FTQ commit edge
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

  private def fold28(x: UInt): UInt = HintHash.component(x)
  private def fold14(pc: UInt): UInt = {
    val v = pc(47, 1).pad(56)
    v(13, 0) ^ v(27, 14) ^ v(41, 28) ^ v(55, 42)
  }
  private def canonical(word: UInt): UInt = Cat(Fill(16, word(46)), word, 0.U(1.W))

  val snapshot = Wire(new HintSnapshot)
  snapshot.ithHead := ithHead
  snapshot.callHead := callHead
  snapshot.retHead := retHead
  snapshot.callLow12 := ringLow(calls, callHead, 6, 7)
  snapshot.retLow12 := ringLow(rets, retHead, 6, 7)
  val s2Snapshot = RegEnable(snapshot, io.s1Fire)
  val s3Snapshot = RegEnable(s2Snapshot, io.s2Fire)
  io.s3Snapshot := s3Snapshot

  // CAM keys are exact prediction-window starts, never branch PCs.
  val camHit = VecInit((0 until 16).map(i =>
    cam(i).valid && cam(i).pc === io.s1StartPc(47, 1)))
  val candidate = (io.enabled || io.profile) && camHit.asUInt.orR
  when(io.enabled && io.s1Fire && !camHit.asUInt.orR) {
    camMisses := camMisses + 1.U
  }
  val camIndex = PriorityEncoder(camHit)
  val camWord = cam(camIndex)
  val candidatePc = io.s1StartPc
  val candidateMask = camWord.mask

  // S1 allocates the complete local checkpoint directly in the sixteen-entry
  // queue. S2/S3 own no additional LHR-head arrays and cannot exceed capacity.
  val metaFull = (metaTail - metaHead) === 16.U
  val sameCamCount = PopCount(io.owners.map(o => o.valid && records(o.index).lhrSel === camIndex))
  val lhrFull = sameCamCount >= 8.U
  val admitted = candidate && !metaFull && !lhrFull && !clearing && !io.load.valid &&
    !io.redirect.valid && !io.s3Override
  val s2ReadValid = RegInit(false.B)
  val s3ReadValid = RegInit(false.B)
  val s3SlotValid = RegInit(false.B)
  val queryBusy = s2ReadValid || s3ReadValid
  val tableBusy = clearing || io.redirect.valid || io.load.valid || !admitted || queryBusy
  io.canAccept := true.B
  io.debugLhrFull.foreach(_ := lhrFull)
  io.debugMetaFull.foreach(_ := metaFull)
  when(io.enabled && io.s1Fire && candidate && metaFull) { metaStalls := metaStalls + 1.U }
  when(io.enabled && io.s1Fire && candidate && lhrFull) { lhrStalls := lhrStalls + 1.U }
  contextTable.io.read.valid := io.s1Fire && candidate && !tableBusy
  val lookupFire = contextTable.io.read.fire

  val ithLow128 = ringLow(ith, ithHead, 64, 7)
  val callLow128 = ringLow(calls, callHead, 64, 7)
  val retLow128 = ringLow(rets, retHead, 64, 7)
  val lhrLow16 = ringLow(lhr(camIndex), lhrHead(camIndex), 8, 4)
  val ithComponent = fold28(ithLow128)
  val lhrComponent = fold28(lhrLow16)
  val callComponent = fold28(callLow128)
  val retComponent = fold28(retLow128)
  val key = fold28(io.s1StartPc(47, 1)) ^
    Mux(candidateMask(0), ithComponent, 0.U(28.W)) ^
    Mux(candidateMask(1), callComponent, 0.U(28.W)) ^
    Mux(candidateMask(2), retComponent, 0.U(28.W)) ^
    Mux(candidateMask(3), lhrComponent, 0.U(28.W))
  val set = key(10, 0)
  val tag = key(27, 11)
  contextTable.io.read.bits := set
  when(io.s1Fire) { s2ReadValid := lookupFire }
  when(io.s2Fire && !io.s1Fire) { s2ReadValid := false.B }
  when(io.redirect.valid || io.s3Override || clearFire) { s2ReadValid := false.B }
  when(io.s3Fire && !io.s2Fire) { s3ReadValid := false.B; s3SlotValid := false.B }
  when(io.redirect.valid || clearFire) { s3ReadValid := false.B; s3SlotValid := false.B }
  val s2Candidate = RegEnable(candidate, io.s1Fire)
  val s2CamIndex = RegEnable(camIndex, io.s1Fire)
  val s2CamPosition = RegEnable(camWord.position, io.s1Fire)
  val s2CamIsCall = RegEnable(camWord.isCall, io.s1Fire)
  val s2Set = RegEnable(set, io.s1Fire)
  val s2Tag = RegEnable(tag, io.s1Fire)
  // The leaf holds only prediction responses. Training reads/writes cannot
  // overwrite a stalled S2's raw SRAM confidence.
  val s2Rows = contextTable.io.rows
  when(contextTable.io.respValid && s2ReadValid && !io.redirect.valid) {
    assert(contextTable.io.readSet === s2Set)
  }
  // Query identity is only a simulation observer, not part of recoverable state.
  val queryClock = Option.when(!env.FPGAPlatform) {
    val cycle = RegInit(0.U(64.W))
    cycle := cycle + 1.U
    cycle
  }
  val s3QueryCycle = Option.when(!env.FPGAPlatform) {
    val s2Cycle = RegEnable(queryClock.get, io.s1Fire)
    RegEnable(s2Cycle, io.s2Fire)
  }
  val s3Candidate = RegEnable(s2Candidate, io.s2Fire)
  val s3CamIndex = RegEnable(s2CamIndex, io.s2Fire)
  val s3CamPosition = RegEnable(s2CamPosition, io.s2Fire)
  val s3CamIsCall = RegEnable(s2CamIsCall, io.s2Fire)
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
  // S3 consumes its old transaction on the redirect edge; no younger S2
  // reservation or read response may survive the flush into the next cycle.
  when(io.s3Override) {
    s2ReadValid := false.B
    s3ReadValid := false.B
    s3SlotValid := false.B
  }
  val selectedWay = RegEnable(s2SelectedWay, io.s2Fire)
  val selectedSlot = RegEnable(s2SelectedRow(21, 17), io.s2Fire)
  val selectedRawConf = RegEnable(s2SelectedRow(23, 22), io.s2Fire)
  when(slotTable.io.respValid && s3SlotValid && !io.redirect.valid) {
    assert(slotTable.io.readSlot === selectedSlot)
  }
  val contextHit = s3ReadValid && s3SlotValid && s3Candidate
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
  val s3Association = io.owners(io.s3Ptr.value)
  val s3Owned = s3Association.valid && s3Association.ptr === io.s3Ptr
  val s3Owner = records(s3Association.index)
  io.s3Lookup.candidate := s3Candidate
  io.s3Lookup.classify := io.enabled && s3Candidate && !clearing && !io.redirect.valid
  io.s3Lookup.position := s3CamPosition
  io.s3Lookup.isCall := s3CamIsCall
  io.s3Lookup.admitted := s3Owned
  io.s3Lookup.pc := io.s3CfiPc
  io.s3Lookup.camIndex := s3CamIndex
  io.s3Lookup.contextHit := contextHit
  io.s3Lookup.usable := io.enabled && contextHit && selectedConf =/= 0.U && s3Owned &&
    !clearing && !io.redirect.valid
  io.s3Lookup.set := s3Set
  io.s3Lookup.way := selectedWay
  io.s3Lookup.target := canonical(selectedTarget)

  val s1HintEvent = admitted && io.s1Prediction.taken &&
    io.s1Prediction.attribute.needIttage && !io.s1Prediction.attribute.isReturn &&
    io.s1Prediction.cfiPosition === camWord.position
  val s3HintEvent = s3Candidate && s3Owned && io.s3Prediction.taken &&
    io.s3Prediction.attribute.needIttage && !io.s3Prediction.attribute.isReturn &&
    io.s3Prediction.cfiPosition === s3CamPosition
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
        observer.io.valid := !reset.asBool && (io.profile || io.enabled) && io.s1Fire
        observer.io.ftqTag := io.s1Ptr.asUInt
        observer.io.queryCycle := queryClock.get
        observer.io.startPc := io.s1StartPc
        observer.io.candidatePc := candidatePc
        observer.io.pcId := camIndex
        observer.io.candidate := candidate
        observer.io.lookupFire := lookupFire
        observer.io.s3Eligible := false.B
        observer.io.admission := admitted
        observer.io.ith := ithComponent
        observer.io.lhr := lhrComponent
        observer.io.call := callComponent
        observer.io.ret := retComponent
        observer.io.mask := candidateMask
        observer.io.contextHash := key
      } else {
        observer.io.valid := !reset.asBool && (io.profile || io.enabled) && io.s3Fire
        observer.io.ftqTag := io.s3Ptr.asUInt
        observer.io.queryCycle := s3QueryCycle.get
        observer.io.startPc := traceStartS3
        observer.io.candidatePc := traceStartS3
        observer.io.pcId := s3CamIndex
        observer.io.candidate := s3Candidate
        observer.io.lookupFire := s3ReadValid
        observer.io.s3Eligible := s3HintEvent
        observer.io.admission := s3Owned
        observer.io.ith := 0.U
        observer.io.lhr := 0.U
        observer.io.call := 0.U
        observer.io.ret := 0.U
        observer.io.mask := 0.U
        observer.io.contextHash := 0.U
      }
    }
  }

  val backendBoundary = Mux(io.redirect.bits.reapply,
    io.redirect.bits.newFtqIdx - 1.U, io.redirect.bits.newFtqIdx)
  def ownerKilled(owner: HintOwner): Bool = owner.valid &&
    HintOwnership.killed(owner.ptr, io.redirect.valid, io.redirect.bits.newFtqIdx,
      io.s3Override, io.s3Ptr)
  // Find the oldest owned FTQ window at/after the recovery boundary. This also
  // recovers every local head when the redirecting window has no Hint payload.
  def localHeadsBefore(ptr: FtqPtr): Vec[UInt] = {
    val candidates = (0 until FtqSize).map { distance =>
      val nextPtr = ptr + distance.U
      val owner = io.owners(nextPtr.value)
      (owner.valid && owner.ptr === nextPtr, owner.index)
    }
    val hits = VecInit(candidates.map(_._1))
    val index = Mux1H(PriorityEncoderOH(hits), candidates.map(_._2))
    VecInit((0 until 16).map(i => Mux(hits.asUInt.orR,
      records(index).lhrHead(i)(4, 1), lhrHead(i))))
  }
  // FTQ owns the existing snapshot. Reconstruct committed profiling components
  // directly from its heads and tiny restoration windows, without profileRows.
  val trace = io.traceCommit.bits
  val traceActualHits = VecInit((0 until 16).map(i => cam(i).valid &&
    cam(i).pc === trace.queryStartPc(47, 1)))
  val traceActualIndex = PriorityEncoder(traceActualHits)
  val traceAssociation = io.owners(trace.ftqTag(log2Ceil(FtqSize) - 1, 0))
  val traceOwner = records(traceAssociation.index)
  val traceOwned = traceAssociation.valid && traceAssociation.ptr.asUInt === trace.ftqTag &&
    traceOwner.lhrSel === traceActualIndex
  val traceQueryValid = trace.queryValid && traceActualHits.asUInt.orR && traceOwned
  def restoredLow128(cells: Vec[UInt], head: UInt, low12: UInt): UInt =
    Cat((0 until 64).reverse.map(i =>
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
  io.profileEvent.bits.ith := fold28(ringLow(ith, trace.snapshot.ithHead, 64, 7))
  io.profileEvent.bits.lhr := fold28(ringLow(lhr(traceActualIndex), traceOwner.lhrHead(traceActualIndex)(4, 1), 8, 4))
  io.profileEvent.bits.call := fold28(restoredLow128(calls, trace.snapshot.callHead, trace.snapshot.callLow12))
  io.profileEvent.bits.ret := fold28(restoredLow128(rets, trace.snapshot.retHead, trace.snapshot.retLow12))

  // Snapshot restoration rewinds every global path, even when this FTQ entry is not an H2P branch.
  private def restoreAndApply(meta: HintSnapshot, pc: UInt, target: UInt,
                              taken: Bool, indirect: Bool, push: Bool, pop: Bool,
                              h2p: Bool, h2pIndex: UInt, heads: Vec[UInt]): Unit = {
    ithHead := meta.ithHead + (taken && indirect).asUInt
    callHead := meta.callHead + (taken && push).asUInt
    retHead := meta.retHead + (taken && pop).asUInt
    for (j <- 0 until 16) {
      lhrHead(j) := heads(j)
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
      lhr(h2pIndex)(heads(h2pIndex)) := target(2, 1)
      lhrHead(h2pIndex) := heads(h2pIndex) + 1.U
    }
  }

  when((io.enabled || io.profile) && io.s1Fire && !io.redirect.valid && !io.s3Override) {
    val pred = io.s1Prediction
    when(pred.taken && pred.attribute.needIttage) {
      ith(ithHead) := pred.target.toUInt(2, 1)
      ithHead := ithHead + 1.U
    }
    when(pred.taken && pred.attribute.hasPush) {
      val newLow = Cat(snapshot.callLow12, 0.U(2.W)) ^ fold14(io.s1CfiPc)
      for (i <- 0 until 7) calls((callHead - i.U)(6, 0)) := newLow(2 * i + 1, 2 * i)
      callHead := callHead + 1.U
    }
    when(pred.taken && pred.attribute.hasPop) {
      val newLow = Cat(snapshot.retLow12, 0.U(2.W)) ^ fold14(io.s1CfiPc)
      for (i <- 0 until 7) rets((retHead - i.U)(6, 0)) := newLow(2 * i + 1, 2 * i)
      retHead := retHead + 1.U
    }
    when(s1HintEvent) {
      lhr(camIndex)(lhrHead(camIndex)) := pred.target.toUInt(2, 1)
      lhrHead(camIndex) := lhrHead(camIndex) + 1.U
    }
  }
  val restoredLhrAppend = WireDefault(false.B)
  when((io.enabled || io.profile) && io.s3Override && !io.redirect.valid) {
    val pred = io.s3Prediction
    val h2p = s3Owned && s3Candidate && pred.attribute.needIttage && !pred.attribute.isReturn &&
      pred.cfiPosition === s3CamPosition
    restoredLhrAppend := pred.taken && h2p
    restoreAndApply(s3Snapshot, io.s3CfiPc, pred.target.toUInt,
      pred.taken, pred.attribute.needIttage, pred.attribute.hasPush,
      pred.attribute.hasPop, h2p, s3Owner.lhrSel, localHeadsBefore(io.s3Ptr))
  }
  when((io.enabled || io.profile) && io.redirect.valid) {
    val redirect = io.redirect.bits
    val reapply = redirect.reapply
    val applySurvivingCfi = reapply && !redirect.flushItself && redirect.taken
    val exactHit = VecInit((0 until 16).map(i => cam(i).valid &&
      cam(i).pc === redirect.startPc.toUInt(47, 1) &&
      getCfiPcFromPosition(redirect.startPc, cam(i).position).toUInt === redirect.cfiPc.toUInt)).asUInt
    val redirectOwner = io.owners(backendBoundary.value)
    val h2p = exactHit.orR && redirectOwner.valid && redirectOwner.ptr === backendBoundary &&
      records(redirectOwner.index).lhrSel === PriorityEncoder(exactHit) &&
      applySurvivingCfi && redirect.attribute.needIttage && !redirect.attribute.isReturn
    restoredLhrAppend := applySurvivingCfi && h2p
    restoreAndApply(redirect.meta.hint, redirect.cfiPc.toUInt, redirect.target.toUInt,
      applySurvivingCfi, redirect.attribute.needIttage,
      redirect.attribute.hasPush, redirect.attribute.hasPop,
      h2p, PriorityEncoder(exactHit), localHeadsBefore(backendBoundary))
  }

  // The FTQ owns the five-bit association. This queue stores only the 101-bit
  // architecture payload, and releases on the FTQ's existing commit edge.
  val allocate = (io.enabled || io.profile) && io.s1Fire && admitted
  io.allocation.valid := allocate
  io.allocation.bits := metaTail(3, 0)
  when(allocate) {
    val r = records(metaTail(3, 0))
    r := 0.U.asTypeOf(new HintRecord)
    r.lhrSel := camIndex
    for (i <- 0 until 16) r.lhrHead(i) := Cat(lhrHead(i), 0.U(1.W))
  }
  when(io.s3Fire && s3Owned && !io.redirect.valid) {
    s3Owner.set := s3Set
    s3Owner.way := selectedWay
    // Train only the CAM CFI selected by the final prediction. Earlier taken
    // branches must not train this Context using their unrelated FTQ target.
    s3Owner.valid := contextHit && s3HintEvent
    s3Owner.conf := Mux(contextHit, selectedConf, 0.U)
  }
  val resolveOwners = io.resolve.map(r => io.owners(r.bits.ftqPtr.value))
  val resolveIndices = resolveOwners.map(_.index)
  val resolveUnique = io.resolve.indices.map { lane =>
    val r = io.resolve(lane)
    val owner = resolveOwners(lane)
    val duplicate = (0 until lane).map(j => io.resolve(j).valid &&
      io.resolve(j).bits.sourceBackend && io.resolve(j).bits.eligible &&
      io.resolve(j).bits.ftqPtr === r.bits.ftqPtr &&
      io.resolve(j).bits.offset === r.bits.offset).foldLeft(false.B)(_ || _)
    val killedCfi = io.redirect.valid && r.bits.ftqPtr === backendBoundary &&
      (r.bits.cfiPc > io.redirect.bits.cfiPc.toUInt ||
        (io.redirect.bits.flushItself && r.bits.cfiPc === io.redirect.bits.cfiPc.toUInt))
    r.valid && r.bits.sourceBackend && r.bits.fresh && owner.valid &&
      owner.ptr === r.bits.ftqPtr && !ownerKilled(owner) && !killedCfi && !duplicate
  }
  val confRequests = io.resolve.indices.map { lane =>
    val resolved = io.resolve(lane).bits
    resolveUnique(lane) && io.enabled && !io.load.valid && io.measureActive &&
      resolved.eligible && resolved.baselineValid && records(resolveIndices(lane)).valid
  }
  val confAccepted = Wire(Vec(io.resolve.length, Bool()))
  val confIncrease = Wire(Vec(io.resolve.length, Bool()))
  val confDecrease = Wire(Vec(io.resolve.length, Bool()))
  for (lane <- io.resolve.indices) {
    val earlier = if (lane == 0) 0.U else PopCount(confAccepted.take(lane))
    confAccepted(lane) := confRequests(lane) && confOccupancy +& earlier < 32.U
    val r = records(resolveIndices(lane))
    val actual = io.resolve(lane).bits.target
    val old = r.conf
    // For conf>0 this matching final CFI actually used Hint, so the existing
    // FTQ prediction target is exactly the Hint target. conf0 uses baseline.
    val correct = io.resolve(lane).bits.baselineTarget === actual
    val next = Mux(old === 0.U, (!correct).asUInt,
      Mux(correct, Mux(old === 3.U, 3.U, old + 1.U), old - 1.U))
    confIncrease(lane) := confAccepted(lane) && next > old
    confDecrease(lane) := confAccepted(lane) && next < old
    when(resolveUnique(lane) && io.resolve(lane).bits.eligible) {
      r.lhrTarget := actual(2, 1)
    }
    when(confAccepted(lane)) {
      val event = confEvents((confTail + earlier)(4, 0))
      event.setway := Cat(r.set, r.way)
      event.conf := next
    }
  }
  // These counters describe accepted resolve requests, including writes later
  // retained as approximate wrong-path training. They no longer count SRAM apply.
  confUpCount := confUpCount + PopCount(confIncrease)
  confDownCount := confDownCount + PopCount(confDecrease)
  val acceptedCount = PopCount(confAccepted)
  val droppedCount = PopCount(confRequests) - acceptedCount
  when(acceptedCount =/= 0.U) { confTail := confTail + acceptedCount }
  when(droppedCount =/= 0.U) { confDropped := confDropped + droppedCount }
  val flush = io.redirect.valid || io.s3Override
  val killCount = PopCount(io.owners.map(ownerKilled))
  val commitOwner = io.owners(io.commit.bits.value)
  val release = io.commit.valid && commitOwner.valid && commitOwner.ptr === io.commit.bits &&
    !ownerKilled(commitOwner)
  when(release) {
    assert(commitOwner.index === metaHead(3, 0))
    metaHead := metaHead + 1.U
  }
  when(allocate || killCount =/= 0.U) {
    metaTail := (metaTail + allocate.asUInt - killCount)(4, 0)
  }

  if (!env.FPGAPlatform) {
    val predictionObserver = Module(new HintPredictionDpi)
    predictionObserver.io.clock := clock
    predictionObserver.io.valid := !reset.asBool && (io.profile || io.enabled) && io.s3Fire
    predictionObserver.io.ftqTag := io.s3Ptr.asUInt
    predictionObserver.io.queryCycle := s3QueryCycle.get
    // The observer identifies the queried CAM branch even when an earlier
    // baseline taken branch wins. Functional history still uses final s3CfiPc.
    val camStart = canonical(cam(s3CamIndex).pc)
    predictionObserver.io.cfiPc := (camStart & ~31.U(64.W)) + (s3CamPosition << 1)
    predictionObserver.io.baselineTarget := io.s3BaselineTarget
    predictionObserver.io.finalTarget := io.s3Prediction.target.toUInt.asSInt.pad(64).asUInt
    predictionObserver.io.contextTarget := canonical(selectedTarget)
    predictionObserver.io.contextHash := 0.U // host joins the exact S1 query key
    predictionObserver.io.contextHit := contextHit
    predictionObserver.io.usedHint := io.s3UsedHint
    predictionObserver.io.conf := Mux(contextHit, selectedConf, 0.U)
    val observation = io.s3Observation.get
    predictionObserver.io.camClassified := observation.camSelected
    predictionObserver.io.isCall := s3CamIsCall
    predictionObserver.io.finalTaken := io.s3Prediction.taken
    predictionObserver.io.finalPosition := io.s3Prediction.cfiPosition
    predictionObserver.io.finalIndirect := io.s3Prediction.attribute.needIttage && !io.s3Prediction.attribute.isReturn
    predictionObserver.io.finalIsCall := io.s3Prediction.attribute.isCall
    predictionObserver.io.s3Override := io.s3Override
    predictionObserver.io.baselineTaken := observation.original.taken
    predictionObserver.io.baselinePosition := observation.original.cfiPosition
    predictionObserver.io.ittageHit := observation.ittageHit
    predictionObserver.io.ittageTarget := observation.ittageTarget
    predictionObserver.io.btbTargetValid := observation.btbTargetValid
    predictionObserver.io.btbTarget := observation.btbTarget
    predictionObserver.io.fallThroughTarget := observation.fallThroughTarget
  }

  // The prediction read has fixed priority in HintContextTable. FIFO payloads
  // are final values computed from the prediction snapshot: stale overwrite
  // is intentional, with no FIFO forwarding and no read-modify-write.
  contextTable.io.trainRead.valid := false.B
  contextTable.io.trainRead.bits := 0.U
  val trainEvent = confEvents(confHead(4, 0))
  contextTable.io.writeConf.valid := confHead =/= confTail && !clearing
  contextTable.io.writeConf.bits.index := trainEvent.setway
  contextTable.io.writeConf.bits.conf := trainEvent.conf
  val applyUpdate = contextTable.io.writeConf.fire
  when(applyUpdate) {
    confHead := confHead + 1.U
  }
  val trainingEmpty = confHead === confTail
  val loaderQuiescent = metaHead === metaTail && trainingEmpty &&
    !s2ReadValid && !s3ReadValid && !s3SlotValid
  assert((metaTail - metaHead)(4, 0) <= 16.U)
  assert(PopCount(io.owners.map(_.valid)) === (metaTail - metaHead)(4, 0))
  assert(confOccupancy <= 32.U)
  for (i <- 0 until 16) {
    assert(PopCount(io.owners.map(o => o.valid && records(o.index).lhrSel === i.U)) <= 8.U)
  }
  when(io.load.valid) { assert(!io.enabled && !io.profile) }
  when(io.load.fire) { assert(loaderQuiescent) }
  when(clearing) { assert(!io.enabled) }
  def count(event: Bool): UInt = {
    val r = RegInit(0.U(32.W))
    when(event) { r := r + 1.U }
    r
  }
  io.stats.lookupPortFallback := count(io.enabled && io.s1Fire && candidate && tableBusy && !clearing)
  io.stats.metadataFallback := count(io.enabled && io.s1Fire && candidate && (metaFull || lhrFull))
  io.stats.rollbackFallback := 0.U
  io.stats.clearingFallback := count(io.enabled && io.s1Fire && candidate && clearing)
  io.stats.s3Confirmed := count(io.enabled && io.s3Fire && s3HintEvent)
  io.stats.recordAllocate := count(allocate)
  // Count exactly the same admitted CAM-qualified writes as the restore path.
  val lhrAppend = (io.s1Fire && s1HintEvent && !flush) || restoredLhrAppend
  io.stats.lhrWrites := count((io.enabled || io.profile) && lhrAppend)
  io.stats.confFullWait := 0.U // FIFO full drops a new event; it never waits.
  io.stats.confApply := count(applyUpdate)
  io.stats.confUndo := 0.U
  val acceptedTotal = RegInit(0.U(32.W))
  acceptedTotal := acceptedTotal + acceptedCount
  io.stats.confAccepted := acceptedTotal
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
        // Global histories belong to the live prediction stream, including
        // non-H2P FTQ snapshots. A table reload must never invalidate them.
        cam.foreach(_ := 0.U.asTypeOf(new HintCamEntry))
        lhr.foreach(_.foreach(_ := 0.U))
        lhrHead.foreach(_ := 0.U)
        metaHead := 0.U
        metaTail := 0.U
        confHead := 0.U
        confTail := 0.U
        confDropped := 0.U
      }
      is(1.U) { when(io.load.bits.index < 16.U) { cam(io.load.bits.index(3, 0)) := io.load.bits.data(57, 0).asTypeOf(new HintCamEntry) } }
      is(2.U) { assert(contextTable.io.load.fire) }
      is(3.U) { assert(io.load.bits.index < 32.U); assert(slotTable.io.load.fire) }
    }
  }
}
