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

class HintProfileRecord(implicit p: Parameters) extends Bundle {
  val ptr = new FtqPtr
  val live = Bool()
  val cutoffValid = Bool()
  val cutoffWord = UInt(47.W)
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
  val trainConf = UInt(2.W)
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
  val camIndex = UInt(4.W)
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
    val profileEvent = Output(Valid(new HintProfileEvent))
    val profileTrace = Option.when(!env.FPGAPlatform)(Output(Vec(backendParams.BrhCnt + 5, Valid(new HintProfileTraceEvent))))
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
  private val metaHead = RegInit(0.U(5.W))
  private val metaDrain = RegInit(0.U(5.W))
  private val metaTail = RegInit(0.U(5.W))
  private val confLog = Reg(Vec(32, new HintConfReq))
  private val confHead = RegInit(0.U(6.W))
  private val confWrite = RegInit(0.U(6.W))
  private val confTail = RegInit(0.U(6.W))
  if (!env.FPGAPlatform) {
    val pendingObserver = Module(new xiangshan.frontend.ftq.HintPendingDpi)
    pendingObserver.clock := clock
    pendingObserver.pending := confTail - confWrite
    pendingObserver.outstanding := confTail - confHead
    pendingObserver.meta := PopCount(records.map(r => r.valid && r.ready && r.measure && r.contextHit && !r.enqueued))
  }
  private val rollback = RegInit(false.B)
  private val rollbackBoundary = Reg(new FtqPtr)
  private val rollbackCutoffWord = Reg(UInt(47.W))
  private val rollbackKillWhole = Reg(Bool())
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
  io.debugMetaDrain.foreach(_ := metaDrain)
  io.debugMetaTail.foreach(_ := metaTail)
  io.debugConfHead.foreach(_ := confHead)
  io.debugConfWrite.foreach(_ := confWrite)
  io.debugConfTail.foreach(_ := confTail)
  io.debugRollback.foreach(_ := rollback)

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
  val pendingWrite = confWrite =/= confTail
  val tableBusy = clearing || rollback || io.redirect.valid ||
    (io.load.valid && io.load.bits.kind === 2.U) || pendingWrite
  io.canAccept := true.B
  io.debugLhrFull.foreach(_ := false.B)
  io.debugMetaFull.foreach(_ := metaFull)
  val lookupFire = io.s1Fire && candidate && !tableBusy

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
  val writeBroadcast = WireDefault(false.B)
  val broadcastSetway = WireDefault(0.U(12.W))
  val broadcastConf = WireDefault(0.U(2.W))
  def mergeConf(row: UInt, setway: UInt): UInt =
    Mux(writeBroadcast && broadcastSetway === setway,
      Cat(row(24), broadcastConf, row(21, 0)), row)
  val s2Row0 = RegEnable(context(Cat(set, 0.U(1.W))), lookupFire)
  val s2Row1 = RegEnable(context(Cat(set, 1.U(1.W))), lookupFire)
  val s2ReadValid = RegEnable(lookupFire, io.s1Fire)
  val s3ReadValid = RegEnable(s2ReadValid, io.s2Fire)
  val s2Candidate = RegEnable(candidate, io.s1Fire)
  val s2Pc = RegEnable(candidatePc, io.s1Fire)
  val s2CamIndex = RegEnable(camIndex, io.s1Fire)
  val s2Set = RegEnable(set, io.s1Fire)
  val s2Tag = RegEnable(tag, io.s1Fire)
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
  val s3Row0 = RegEnable(mergeConf(s2Row0, Cat(s2Set, 0.U(1.W))), io.s2Fire)
  val s3Row1 = RegEnable(mergeConf(s2Row1, Cat(s2Set, 1.U(1.W))), io.s2Fire)
  val s3Candidate = RegEnable(s2Candidate, io.s2Fire)
  val s3Pc = RegEnable(s2Pc, io.s2Fire)
  val s3CamIndex = RegEnable(s2CamIndex, io.s2Fire)
  val s3Set = RegEnable(s2Set, io.s2Fire)
  val s3Tag = RegEnable(s2Tag, io.s2Fire)
  val way0Hit = s3Row0(24) && s3Row0(16, 0) === s3Tag
  val way1Hit = s3Row1(24) && s3Row1(16, 0) === s3Tag
  val contextHit = io.enabled && s3ReadValid && s3Candidate && (way0Hit || way1Hit)
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
  // Current apply writes remain in confLog, including a concurrently retiring
  // head. Earlier retired writes are already broadcast into cached rows. Do
  // not depend on current write arbitration here: it includes S3 override.
  val selectedConf = forwardedConf(selectedSetway, selectedRow(23, 22))
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
    !clearing && !rollback && !io.redirect.valid && io.s3Prediction.taken &&
    io.s3Prediction.attribute.needIttage && !io.s3Prediction.attribute.isReturn &&
    io.s3CfiPc === s3Pc
  io.s3Lookup.set := s3Set
  io.s3Lookup.way := selectedWay
  io.s3Lookup.target := canonical(slots(selectedRow(21, 17)))

  val s1Exact = VecInit((0 until 16).map(i => cam(i)(51) && canonical(cam(i)(46, 0)) === io.s1CfiPc))
  val s1ExactIndex = PriorityEncoder(s1Exact)
  val s1HintEvent = s1Exact.asUInt.orR && io.s1Prediction.taken && io.s1Prediction.attribute.needIttage
  val s3HintEvent = s3Candidate && io.s3Prediction.taken &&
    io.s3Prediction.attribute.needIttage && !io.s3Prediction.attribute.isReturn && io.s3CfiPc === s3Pc
  val s3Exact = VecInit((0 until 16).map(i => cam(i)(51) && canonical(cam(i)(46, 0)) === io.s3CfiPc))
  when(io.enabled && io.s3Fire && io.s3UsedHint && io.s3Override) {
    overrideCount := overrideCount + 1.U
  }

  // The observer retains one committed branch event per FTQ entry. It has no
  // path into io.canAccept or the prediction mux and is inert outside profile.
  val backendBoundary = Mux(io.redirect.bits.reapply,
    io.redirect.bits.newFtqIdx - 1.U, io.redirect.bits.newFtqIdx)
  def backendKills(ptr: FtqPtr, pcWord: UInt): Bool =
    ptr > backendBoundary || (ptr === backendBoundary &&
      (!io.redirect.bits.reapply || pcWord > io.redirect.bits.cfiPc.toUInt(47, 1)))
  def s3Kills(ptr: FtqPtr, pcWord: UInt): Bool =
    ptr > io.s3Ptr || (ptr === io.s3Ptr && io.s3Prediction.taken && pcWord > io.s3CfiPc(47, 1))
  def killedNow(ptr: FtqPtr, pcWord: UInt): Bool =
    Mux(io.redirect.valid, backendKills(ptr, pcWord), io.s3Override && s3Kills(ptr, pcWord))
  val profileCycle = RegInit(0.U(64.W))
  profileCycle := profileCycle + 1.U
  val profileRows = RegInit(VecInit(Seq.fill(64)(0.U.asTypeOf(new HintProfileRecord))))
  when(io.profile && io.s1Fire) {
    val row = profileRows(io.s1Ptr.value)
    row := 0.U.asTypeOf(new HintProfileRecord)
    row.ptr := io.s1Ptr
    row.live := true.B
    row.event.ftqTag := Cat(io.s1Ptr.flag, io.s1Ptr.value)
    row.event.queryCycle := profileCycle
    row.event.blockStart := io.s1StartPc
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
      val existingKilled = killedNow(row.ptr, row.event.pc(47, 1))
      val allowed = row.live && (!row.cutoffValid || resolve.bits.cfiPc(47, 1) <= row.cutoffWord)
      when(resolve.valid && resolve.bits.eligible && resolve.bits.sourceBackend && allowed &&
        row.ptr === resolve.bits.ftqPtr && (!row.resolved || existingKilled) &&
        !killedNow(resolve.bits.ftqPtr, resolve.bits.cfiPc(47, 1))) {
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
    commitRow.live && commitRow.ptr === io.commit.bits &&
    (!commitRow.cutoffValid || commitRow.event.pc(47, 1) <= commitRow.cutoffWord) &&
    !killedNow(commitRow.ptr, commitRow.event.pc(47, 1))
  io.profileEvent.bits := commitRow.event
  when(io.profile && io.commit.valid) {
    profileRows(io.commit.bits.value).resolved := false.B
    profileRows(io.commit.bits.value).live := false.B
  }
  when(io.profile && io.s3Override && !io.redirect.valid) {
    for (i <- 0 until 64) {
      val row = profileRows(i)
      when(row.ptr > io.s3Ptr) { row.resolved := false.B; row.live := false.B }
      when(row.ptr === io.s3Ptr && io.s3Prediction.taken) {
        row.cutoffValid := true.B
        row.cutoffWord := Mux(row.cutoffValid && row.cutoffWord < io.s3CfiPc(47, 1),
          row.cutoffWord, io.s3CfiPc(47, 1))
        val replacing = io.resolve.map(r => r.valid && r.bits.sourceBackend && r.bits.eligible && row.live &&
          r.bits.ftqPtr === row.ptr && r.bits.cfiPc(47, 1) <= io.s3CfiPc(47, 1) &&
          (!row.cutoffValid || r.bits.cfiPc(47, 1) <= row.cutoffWord)).reduce(_ || _)
        when(row.resolved && row.event.pc(47, 1) > io.s3CfiPc(47, 1) && !replacing) { row.resolved := false.B }
      }
    }
  }
  when(io.profile && io.redirect.valid) {
    for (i <- 0 until 64) {
      val row = profileRows(i)
      when(row.ptr >= io.redirect.bits.newFtqIdx) {
        row.resolved := false.B
        row.live := false.B
      }
      when(io.redirect.bits.reapply && row.ptr === backendBoundary) {
        row.cutoffValid := true.B
        row.cutoffWord := Mux(row.cutoffValid && row.cutoffWord < io.redirect.bits.cfiPc.toUInt(47, 1),
          row.cutoffWord, io.redirect.bits.cfiPc.toUInt(47, 1))
        val replacing = io.resolve.map(r => r.valid && r.bits.sourceBackend && r.bits.eligible &&
          r.bits.ftqPtr === row.ptr && r.bits.cfiPc(47, 1) <= io.redirect.bits.cfiPc.toUInt(47, 1) &&
          (!row.cutoffValid || r.bits.cfiPc(47, 1) <= row.cutoffWord)).reduce(_ || _)
        when(row.resolved && row.event.pc(47, 1) > io.redirect.bits.cfiPc.toUInt(47, 1) && !replacing) {
          row.resolved := false.B
        }
      }
    }
  }

  io.profileTrace.foreach { events =>
    events := 0.U.asTypeOf(events)
    def emit(index: Int, valid: Bool, kind: Int, ptr: FtqPtr, query: UInt,
             pc: UInt, target: UInt, offset: UInt, mispredict: Bool,
             survived: Bool, observed: Bool, resolved: Bool,
             sourceBackend: Bool, eligible: Bool): Unit = {
      val e = events(index)
      e.valid := valid
      e.bits.eventKind := kind.U
      e.bits.cycle := profileCycle
      e.bits.queryCycle := query
      e.bits.ftqTag := ptr.asUInt
      e.bits.pc := pc
      e.bits.target := target
      e.bits.offset := offset
      e.bits.mispredict := mispredict
      e.bits.survived := survived
      e.bits.observed := observed
      e.bits.resolved := resolved
      e.bits.sourceBackend := sourceBackend
      e.bits.eligible := eligible
    }
    emit(0, io.profile && io.s1Fire, 0, io.s1Ptr, profileCycle,
      candidatePc, io.s1Prediction.target.toUInt.asSInt.pad(64).asUInt,
      ((candidatePc - io.s1StartPc) >> 1)(5, 0), false.B, true.B,
      candidate, false.B, false.B, false.B)
    io.resolve.zipWithIndex.foreach { case (r, i) =>
      val row = profileRows(r.bits.ftqPtr.value)
      val accepted = row.live && row.ptr === r.bits.ftqPtr && r.bits.eligible && r.bits.sourceBackend &&
        (!row.cutoffValid || r.bits.cfiPc(47, 1) <= row.cutoffWord) &&
        (!row.resolved || killedNow(row.ptr, row.event.pc(47, 1))) &&
        !killedNow(r.bits.ftqPtr, r.bits.cfiPc(47, 1))
      emit(i + 1, io.profile && r.valid, 1, r.bits.ftqPtr, row.event.queryCycle,
        r.bits.cfiPc, r.bits.target, r.bits.offset, r.bits.mispredict,
        accepted, row.event.observed, row.resolved, r.bits.sourceBackend, r.bits.eligible)
    }
    val redirectRow = profileRows(backendBoundary.value)
    emit(backendParams.BrhCnt + 2, io.profile && io.redirect.valid, 2,
      backendBoundary, redirectRow.event.queryCycle,
      io.redirect.bits.cfiPc.toUInt.asSInt.pad(64).asUInt,
      io.redirect.bits.target.toUInt.asSInt.pad(64).asUInt,
      ((io.redirect.bits.cfiPc.toUInt - redirectRow.event.blockStart) >> 1)(5, 0),
      true.B, io.redirect.bits.reapply, redirectRow.event.observed,
      redirectRow.resolved, false.B, false.B)
    emit(backendParams.BrhCnt + 3, io.profile && io.commit.valid, 3,
      io.commit.bits, commitRow.event.queryCycle, commitRow.event.pc,
      commitRow.event.actual, ((commitRow.event.pc - commitRow.event.blockStart) >> 1)(5, 0),
      false.B, io.profileEvent.valid, commitRow.event.observed,
      commitRow.resolved, true.B, commitRow.event.eligible)
    val s3Row = profileRows(io.s3Ptr.value)
    emit(backendParams.BrhCnt + 4, io.profile && io.s3Override, 4,
      io.s3Ptr, s3QueryCycle, io.s3CfiPc,
      io.s3Prediction.target.toUInt.asSInt.pad(64).asUInt,
      ((io.s3CfiPc - s3Row.event.blockStart) >> 1)(5, 0),
      true.B, !io.redirect.valid, s3Row.event.observed,
      s3Row.resolved, false.B, false.B)
    events.foreach { event =>
      val dpi = Module(new HintProfileTraceDpi)
      dpi.io.clock := clock
      dpi.io.valid := event.valid
      dpi.io.eventKind := event.bits.eventKind
      dpi.io.cycle := event.bits.cycle
      dpi.io.queryCycle := event.bits.queryCycle
      dpi.io.ftqTag := event.bits.ftqTag
      dpi.io.offset := event.bits.offset
      dpi.io.pc := event.bits.pc
      dpi.io.target := event.bits.target
      dpi.io.mispredict := event.bits.mispredict
      dpi.io.survived := event.bits.survived
      dpi.io.observed := event.bits.observed
      dpi.io.resolved := event.bits.resolved
      dpi.io.sourceBackend := event.bits.sourceBackend
      dpi.io.eligible := event.bits.eligible
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
      lhr(s1ExactIndex)(lhrHead(s1ExactIndex)(2, 0)) := pred.target.toUInt(2, 1)
      lhrHead(s1ExactIndex) := lhrHead(s1ExactIndex) + 1.U
    }
  }
  when((io.enabled || io.profile) && io.s3Override && !io.redirect.valid) {
    val pred = io.s3Prediction
    when(io.profile) { restoreProfileWindow(s3Snapshot, profileRows(io.s3Ptr.value)) }
    restoreAndApply(s3Snapshot, io.s3CfiPc, pred.target.toUInt,
      pred.taken, pred.attribute.needIttage, pred.attribute.hasPush,
      pred.attribute.hasPop, s3Exact.asUInt.orR && pred.attribute.needIttage, PriorityEncoder(s3Exact))
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

  // The S3 actual CFI owns metadata; no speculative S1 reservation.
  val allocate = io.enabled && io.s3Fire && s3HintEvent && contextHit &&
    !metaFull && !clearing && !rollback && !io.redirect.valid
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
    r.trainConf := selectedConf
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
      when(records(i).valid && records(i).ptr === io.commit.bits &&
        !killedNow(records(i).ptr, cam(records(i).camIndex)(46, 0))) {
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
      when(i.U < (confTail - confHead) && confLog(idx).ptr === io.commit.bits &&
        !killedNow(confLog(idx).ptr, cam(confLog(idx).camIndex)(46, 0))) {
        confLog(idx).committed := true.B
      }
    }
  }
  val releaseMeta = metaHead =/= metaTail && records(metaHead(3, 0)).committed &&
    records(metaHead(3, 0)).enqueued
  when(releaseMeta) {
    records(metaHead(3, 0)).valid := false.B
    metaHead := metaHead + 1.U
  }
  val flush = io.redirect.valid || io.s3Override
  val killed = VecInit((0 until 16).map { i =>
    records(i).valid && !records(i).committed &&
      killedNow(records(i).ptr, cam(records(i).camIndex)(46, 0))
  })
  val killCount = PopCount(killed)
  when(flush) {
    // S3 arrivals are in allocation order; only backend redirects can remove
    // an existing metadata suffix, and they prohibit a concurrent allocation.
    when(allocate) { assert(killCount === 0.U) }
    for (i <- 0 until 16) when(killed(i)) { records(i).valid := false.B }
    val newTail = (metaTail - killCount)(4, 0)
    metaTail := (newTail + allocate.asUInt)(4, 0)
    when((metaDrain - metaHead)(4, 0) > (newTail - metaHead)(4, 0)) { metaDrain := newTail }
  }
  val oldestIndex = metaDrain(3, 0)
  val oldestRecord = records(oldestIndex)
  val drain = metaDrain =/= metaTail && oldestRecord.valid && oldestRecord.ready &&
    !rollback && !flush
  val confFull = (confTail - confHead) === 32.U
  // The cached training confidence receives every physical write broadcast.
  // Draining metadata never consumes a second Context read port.
  val priorConf = forwardedConf(oldestRecord.setway,
    Mux(writeBroadcast && broadcastSetway === oldestRecord.setway, broadcastConf, oldestRecord.trainConf))
  val nextConf = Mux(oldestRecord.correct, Mux(priorConf === 3.U, 3.U, priorConf + 1.U),
    Mux(priorConf === 0.U, 0.U, priorConf - 1.U))
  val needsConf = oldestRecord.contextHit && oldestRecord.measure
  when(drain && (!needsConf || !confFull)) {
    records(oldestIndex).enqueued := true.B
    metaDrain := metaDrain + 1.U
    when(needsConf) {
      {
        confLog(confTail(4, 0)).ptr := oldestRecord.ptr
        confLog(confTail(4, 0)).camIndex := oldestRecord.camIndex
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

  // A redirect reverses applied younger writes from newest to oldest. Hint
  // queries fall back while ordinary frontend prediction continues.
  val rollbackTrigger = io.enabled && (io.redirect.valid || io.s3Override)
  val triggerBoundary = Mux(io.redirect.valid, backendBoundary,
    Mux(io.s3Prediction.taken, io.s3Ptr, io.s3Ptr + 1.U))
  val triggerCutoffWord = Mux(io.redirect.valid, io.redirect.bits.cfiPc.toUInt(47, 1), io.s3CfiPc(47, 1))
  val triggerKillWhole = Mux(io.redirect.valid, !io.redirect.bits.reapply, !io.s3Prediction.taken)
  val strongerBoundary = triggerBoundary < rollbackBoundary ||
    (triggerBoundary === rollbackBoundary &&
      (triggerKillWhole || (!rollbackKillWhole && triggerCutoffWord < rollbackCutoffWord)))
  val useTriggerBoundary = rollbackTrigger && (!rollback || strongerBoundary)
  val effectiveBoundary = Mux(useTriggerBoundary,
    triggerBoundary, rollbackBoundary)
  val effectiveCutoffWord = Mux(useTriggerBoundary, triggerCutoffWord, rollbackCutoffWord)
  val effectiveKillWhole = Mux(useTriggerBoundary, triggerKillWhole, rollbackKillWhole)
  when(rollbackTrigger) {
    rollback := true.B
    when(!rollback || strongerBoundary) {
      rollbackBoundary := triggerBoundary
      rollbackCutoffWord := triggerCutoffWord
      rollbackKillWhole := triggerKillWhole
    }
  }
  val lastIndex = (confTail - 1.U)(4, 0)
  val last = confLog(lastIndex)
  val lastKilled = last.ptr > effectiveBoundary || (last.ptr === effectiveBoundary &&
    (effectiveKillWhole || cam(last.camIndex)(46, 0) > effectiveCutoffWord))
  val lastCommitsNow = io.commit.valid && last.ptr === io.commit.bits && !lastKilled
  val undo = rollback && confTail =/= confHead && !last.committed && !lastCommitsNow &&
    lastKilled
  val applyUpdate = !clearing && !rollback && !rollbackTrigger &&
    !(io.load.valid && io.load.bits.kind === 2.U) &&
    confWrite =/= confTail
  val retire = confHead =/= confTail && confLog(confHead(4, 0)).applied &&
    confLog(confHead(4, 0)).committed && !rollback && !rollbackTrigger
  when(rollback) {
    when(undo) {
      when(last.applied) {
        val row = context(last.setway)
        context(last.setway) := Cat(row(24), last.oldConf, row(21, 0))
      }
      confTail := confTail - 1.U
      when(confWrite === confTail) { confWrite := confWrite - 1.U }
    }.otherwise {
      when(!rollbackTrigger) { rollback := false.B }
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

  // A row captured in S1 can outlive its confidence log entry. Broadcast writes
  // into both cached stages and the metadata training copy before retiring logs.
  when(applyUpdate) {
    writeBroadcast := true.B
    broadcastSetway := confLog(confWrite(4, 0)).setway
    broadcastConf := confLog(confWrite(4, 0)).newConf
  }
  when(undo && last.applied) {
    writeBroadcast := true.B
    broadcastSetway := last.setway
    broadcastConf := last.oldConf
  }
  when(writeBroadcast) {
    when(!lookupFire) {
      s2Row0 := mergeConf(s2Row0, Cat(s2Set, 0.U(1.W)))
      s2Row1 := mergeConf(s2Row1, Cat(s2Set, 1.U(1.W)))
    }
    when(!io.s2Fire) {
      s3Row0 := mergeConf(s3Row0, Cat(s3Set, 0.U(1.W)))
      s3Row1 := mergeConf(s3Row1, Cat(s3Set, 1.U(1.W)))
    }
    for (i <- 0 until 16) {
      when(records(i).valid && records(i).setway === broadcastSetway) {
        records(i).trainConf := broadcastConf
      }
    }
  }

  assert((metaTail - metaHead)(4, 0) <= 16.U)
  assert((metaDrain - metaHead)(4, 0) <= (metaTail - metaHead)(4, 0))
  assert((confTail - confHead)(5, 0) <= 32.U)
  assert((confWrite - confHead)(5, 0) <= (confTail - confHead)(5, 0))
  when(io.enabled && !clearing) {
    assert((confTail - confWrite)(5, 0) <= 1.U)
    assert((confTail - confHead)(5, 0) <= 17.U)
  }
  when(io.load.valid) {
    assert(!io.enabled && !io.profile)
    assert(metaHead === metaTail && confHead === confTail)
  }
  when(clearing) { assert(!io.enabled) }
  def count(event: Bool): UInt = {
    val r = RegInit(0.U(32.W))
    when(event) { r := r + 1.U }
    r
  }
  io.stats.lookupPortFallback := count(io.enabled && io.s1Fire && candidate && tableBusy && !clearing && !rollback)
  io.stats.metadataFallback := count(io.enabled && io.s3Fire && s3HintEvent && contextHit && metaFull)
  io.stats.rollbackFallback := count(io.enabled && io.s1Fire && candidate && rollback)
  io.stats.clearingFallback := count(io.enabled && io.s1Fire && candidate && clearing)
  io.stats.s3Confirmed := count(io.enabled && io.s3Fire && s3HintEvent)
  io.stats.recordAllocate := count(allocate)
  val redirectH2p = VecInit((0 until 16).map(i => cam(i)(51) &&
    canonical(cam(i)(46, 0)) === io.redirect.bits.cfiPc.toUInt)).asUInt.orR
  val lhrAppend = (io.s1Fire && s1HintEvent && !flush) ||
    (io.s3Override && !io.redirect.valid && io.s3Prediction.taken &&
      io.s3Prediction.attribute.needIttage && s3Exact.asUInt.orR) ||
    (io.redirect.valid && io.redirect.bits.reapply && io.redirect.bits.taken &&
      io.redirect.bits.attribute.needIttage && redirectH2p)
  io.stats.lhrWrites := count((io.enabled || io.profile) && lhrAppend)
  io.stats.confFullWait := count(drain && needsConf && confFull)
  io.stats.confApply := count(applyUpdate)
  io.stats.confUndo := count(undo && last.applied)
  io.stats.confAccepted := count(drain && needsConf && !confFull)
  io.stats.confCanceled := count(undo && !last.applied)
  io.stats.confRetired := count(retire)

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
        metaHead := 0.U
        metaDrain := 0.U
        metaTail := 0.U
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
