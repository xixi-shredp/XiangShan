// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.
//
// Acknowledgement
// This implementation is inspired by several key papers:
// [1] Glenn Reinman, Todd Austin, and Brad Calder. "[A scalable front-end architecture for fast instruction delivery.]
// (https://doi.org/10.1109/ISCA.1999.765954)" 26th International Symposium on Computer Architecture (ISCA). 1999.

package xiangshan.frontend.ftq

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.DataHoldBypass
import utility.GTimer
import utility.HasCircularQueuePtrHelper
import utility.HasPerfEvents
import utility.UIntToMask
import utility.XSError
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import utility.XSPerfRolling
import utility.XSPerfSeqAccumulate
import utility.XSPerfSeqRolling
import xiangshan.RedirectLevel
import xiangshan.TopDownCounters
import xiangshan.backend.CtrlToFtqIO
import xiangshan.frontend.BackendRedirectTopdown
import xiangshan.frontend.BlameBpuSource
import xiangshan.frontend.BpuToFtqIO
import xiangshan.frontend.ExceptionType
import xiangshan.frontend.FetchRequestBundle
import xiangshan.frontend.FrontendTopDownBundle
import xiangshan.frontend.FtqToBpuIO
import xiangshan.frontend.FtqToICacheIO
import xiangshan.frontend.FtqToIfuIO
import xiangshan.frontend.IfuToFtqIO
import xiangshan.frontend.PcInit
import xiangshan.frontend.TwoPrefetchCase
import xiangshan.frontend.bpu.BpuCommitMeta
import xiangshan.frontend.bpu.BpuPredictionSource
import xiangshan.frontend.bpu.BpuRedirectMeta
import xiangshan.frontend.bpu.BpuResolveMeta
import xiangshan.frontend.bpu.BpuTrain
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.bpu.BranchInfo
import xiangshan.frontend.bpu.CompareMatrix
import xiangshan.frontend.bpu.HalfAlignHelper
import xiangshan.frontend.bpu.hint.HintTraceResolved
import xiangshan.frontend.icache.ICacheDataHelper
import xiangshan.frontend.icache.ICacheToFtqIO
import xiangshan.frontend.icache.TwoFetchFailReason

/** Bind resolve coordinates to the authoritative resident FTQ window.
  * FTQ offsets name the final halfword relative to that window, for either producer.
  */
object HintResolveCoordinates {
  def internalPc(windowStart: UInt, ftqOffset: UInt, instOffsetBits: Int): UInt =
    (windowStart + (ftqOffset << instOffsetBits)).asSInt.pad(64).asUInt
}

/** Shared production arbitration for registered trace state and same-cycle commit bypass. */
object HintTraceNextState {
  def generationMatches(meta: HintTraceResolved, ptr: FtqPtr, oldest: FtqPtr, newest: FtqPtr): Bool =
    meta.generation === ptr.flag && ptr >= oldest && ptr < newest

  // Later resolve lanes have priority; each lane already includes preceding same-entry lanes.
  def select[T <: Data](base: T, writes: Seq[(Bool, T)]): T = MuxCase(base, writes.reverse)

  def apply(
      base: HintTraceResolved,
      index: UInt,
      writes: Seq[(Bool, UInt, HintTraceResolved)],
      redirectValid: Bool,
      redirectNewPtr: FtqPtr,
      redirectPtr: FtqPtr,
      redirectOffset: UInt,
      flushItself: Bool
  )(implicit p: Parameters): HintTraceResolved = {
    val selected = select(base, writes.map { case (valid, value, updated) =>
      (valid && value === index) -> updated
    })
    val next = WireInit(selected)
    val ptr = FtqPtr(selected.generation, index)
    when(redirectValid) {
      when(ptr >= redirectNewPtr) {
        next.valid := false.B
        next.eligible := false.B
      }.elsewhen(ptr === redirectPtr) {
        // The redirecting FTQ survives, but its suffix does not. Retain a
        // boundary even when no indirect has resolved, so delayed wrong-path
        // resolves cannot repopulate the entry after the redirect pulse.
        val cutoff = redirectOffset - flushItself.asUInt
        when(!selected.valid || selected.offset > cutoff) {
          next := 0.U.asTypeOf(chiselTypeOf(selected))
          next.generation := selected.generation
          next.valid := true.B
          next.offset := cutoff
        }
      }
    }
    next
  }
}

class Ftq(implicit p: Parameters) extends FtqModule
    with HalfAlignHelper
    with HasPerfEvents
    with HasCircularQueuePtrHelper
    with IfuRedirectReceiver
    with BackendRedirectReceiver
    with ICacheDataHelper {

  class FtqIO extends FtqBundle {
    val fromBpu: BpuToFtqIO = Flipped(new BpuToFtqIO)
    val toBpu:   FtqToBpuIO = new FtqToBpuIO

    val fromIfu: IfuToFtqIO = Flipped(new IfuToFtqIO)
    val toIfu:   FtqToIfuIO = new FtqToIfuIO

    val fromICache: ICacheToFtqIO = Flipped(new ICacheToFtqIO)
    val toICache:   FtqToICacheIO = new FtqToICacheIO

    val fromBackend: CtrlToFtqIO = Flipped(new CtrlToFtqIO)
    val toBackend:   FtqToCtrlIO = new FtqToCtrlIO

    // Topdown analysis
    val backendRedirectTopdown: BackendRedirectTopdown = Output(new BackendRedirectTopdown)
  }

  val io: FtqIO = IO(new FtqIO)

  // FTQ pointers. All the pointers mean the pointed entry is *to be* processed, not already processed.
  // For example, bpuPtr points to the entry that BPU prepares to write,
  // and commitPtr points to the entry to be committed by backend.
  private val bpuPtr    = RegInit(FtqPtrVec())
  private val pfPtr     = RegInit(FtqPtrVec(2))
  private val fetchPtr  = RegInit(FtqPtrVec(3))
  private val commitPtr = RegInit(FtqPtrVec(2))

  // pnrPtr is the "Point of No Return" of Bpu: the oldest entry Bpu can still take back. Every older entry has left
  // Bpu's last override stage, so it cannot be flushed from within Ftq, and can safely be handed to later pipelines
  private val pnrPtr = RegInit(FtqPtrVec())

  XSError(bpuPtr < fetchPtr && !isFull(bpuPtr(0), fetchPtr(0)), "fetchPtr runs ahead of bpuPtr")
  XSError(bpuPtr < pfPtr && !isFull(bpuPtr(0), pfPtr(0)), "pfPtr runs ahead of bpuPtr")
  XSError(pnrPtr > bpuPtr && !isFull(pnrPtr(0), bpuPtr(0)), "pnrPtr runs ahead of bpuPtr")

  // an entry has passed the point of no return, so Bpu can no longer override it
  private def passedPnr(ptr: FtqPtr): Bool = pnrPtr > ptr

  // entryQueue stores predictions made by BPU.
  private val entryQueue = Reg(Vec(FtqSize, new FtqEntry))
  // Pointer generation and validity already belong to the FTQ. The entry keeps
  // only a valid+four-bit Hint queue association, with no duplicate pointer/PC.
  for (i <- 0 until FtqSize) {
    val ptr = HintOwnership.residentPtr(i.U, commitPtr(0))
    io.toBpu.hintOwners(i).valid := HintOwnership.active(ptr, commitPtr(0), bpuPtr(0)) &&
      entryQueue(i).hint.valid
    io.toBpu.hintOwners(i).ptr := ptr
    io.toBpu.hintOwners(i).index := entryQueue(i).hint.bits
  }

  // metaQueueRedirect stores speculation information needed by BPU when redirect happens.
  private val metaQueueRedirect = Reg(Vec(FtqSize, new BpuRedirectMeta))

  // metaQueue stores information needed to train BPU.
  private val metaQueueResolve = Reg(Vec(FtqSize, new BpuResolveMeta))
  private val metaQueueCommit  = Reg(Vec(FtqSize, new BpuCommitMeta))

  // resolveQueue caches branch resolve information from backend.
  private val resolveQueue = Module(new ResolveQueue)

  // commitQueue caches branch commit information from backend.
  private val commitQueue = Module(new CommitQueue)

  // perfQueue stores information for performance monitoring. These queues should not exist in hardware
  private val perfQueue = Reg(Vec(FtqSize, new PerfMeta))
  // These generation checks observe the existing sim perfQueue; no prediction guard/storage.
  private val debugPerfGeneration = Option.when(!env.FPGAPlatform)(Reg(Vec(FtqSize, new FtqPtr)))
  private val debugPerfValid = Option.when(!env.FPGAPlatform)(RegInit(VecInit(Seq.fill(FtqSize)(false.B))))

  private val (backendRedirectFtqIdxInAdvance, backendRedirect) = receiveBackendRedirect(io.fromBackend)

  private val specTopAddr = metaQueueRedirect(io.fromIfu.wbRedirect.bits.ftqIdx.value).ras.topRetAddr.toUInt
  private val (ifuRedirectFtqIdxInAdvance, ifuRedirect, ifuResolve) = receiveIfuRedirect(
    io.fromIfu.wbRedirect,
    specTopAddr,
    backendRedirect.valid
  )

  // redirectFtqIdxInAdvance is always one cycle ahead of redirect
  private val redirectFtqIdxInAdvance = Mux(
    backendRedirectFtqIdxInAdvance.valid,
    backendRedirectFtqIdxInAdvance.bits,
    ifuRedirectFtqIdxInAdvance.bits
  )

  private val redirect = Mux(backendRedirect.valid, backendRedirect, ifuRedirect)

  // Instruction page fault, guest page fault, and access fault are checked by backend and sent with redirect requests.
  private val backendException = RegInit(ExceptionType.None)
  private val hasSatpFlush     = RegInit(false.B)
  private val backendFlagPtr   = RegInit(FtqPtr(false.B, 0.U))
  private def hasBackendFlag   = backendException.hasException || hasSatpFlush
  when(backendRedirect.valid) {
    val exception = ExceptionType.fromBackend(backendRedirect.bits)
    backendException := exception
    hasSatpFlush     := backendRedirect.bits.satpFlush
    when(backendRedirect.bits.hasBackendFault || backendRedirect.bits.satpFlush) {
      backendFlagPtr := backendRedirect.bits.newFtqIdx
    }
  }.elsewhen(distanceBetween(fetchPtr(0), backendFlagPtr) >= 3.U) {
    // We cannot clear backendException flag too early (e.g. once fetch fire),
    // bpu may do an override and the flag can be lost in such case.
    // Here we use a magic number 3 (the length of ifu pipeline):
    //   if fetchPtr is ahead 3 fetch blocks, the marked block should be in ibuffer and cannot be flushed by bpu.
    backendException := ExceptionType.None
    hasSatpFlush     := false.B
  }

  // --------------------------------------------------------------------------------
  // Interaction with BPU
  // --------------------------------------------------------------------------------

  private val bpTrainStallCnt = RegInit(0.U((log2Ceil(BpTrainStallLimit) + 1).W))
  when(io.toBpu.train.valid && !io.toBpu.train.ready) {
    bpTrainStallCnt := bpTrainStallCnt + 1.U
  }.otherwise {
    bpTrainStallCnt := 0.U
  }

  // We limit the distance between BP and IF and stall counts of BP train so that branch update can be written back to
  // BPU
  io.fromBpu.prediction.ready := distanceBetween(bpuPtr(0), commitPtr(0)) < FtqSize.U &&
    distanceBetween(bpuPtr(0), fetchPtr(0)) < BpRunAheadDistance.U &&
    bpTrainStallCnt < BpTrainStallLimit.U
  io.fromBpu.meta.ready := true.B

  private val prediction = io.fromBpu.prediction

  private val bpuS3Redirect = prediction.valid && prediction.bits.s3Override

  io.toBpu.bpuPtr := bpuPtr(0)
  private val bpuEnqueue = prediction.fire && !redirect.valid

  private val predictionPtr = MuxCase(
    bpuPtr(0),
    Seq(
      prediction.bits.s3Override -> io.fromBpu.s3FtqPtr
    )
  )
  private val writeEntry = (prediction.fire || bpuS3Redirect) && !redirect.valid

  when(prediction.bits.s3Override) {
    bpuPtr := io.fromBpu.s3FtqPtr + 1.U
  }.elsewhen(bpuEnqueue) {
    bpuPtr := bpuPtr + 1.U
  }

  when(writeEntry) {
    entryQueue(predictionPtr.value).startPc     := prediction.bits.startPc
    entryQueue(predictionPtr.value).target      := prediction.bits.target
    entryQueue(predictionPtr.value).taken       := prediction.bits.taken
    entryQueue(predictionPtr.value).endPosition := prediction.bits.endPosition
    // A correction reuses the resident association; a newly allocated window
    // always initializes all five bits, including a rejected Hint admission.
    when(!prediction.bits.s3Override) {
      entryQueue(predictionPtr.value).hint := io.fromBpu.hintAllocation
    }
  }

  private val s3Trace = WireInit(VecInit(metaQueueResolve.map(_.hintTrace)))
  private val s3PerfQueue = WireInit(perfQueue)
  when(io.fromBpu.meta.valid) {
    val s3BpuPtr = io.fromBpu.s3FtqPtr.value
    metaQueueRedirect(s3BpuPtr) := io.fromBpu.meta.bits.redirectMeta
    metaQueueResolve(s3BpuPtr)  := io.fromBpu.meta.bits.resolveMeta
    s3Trace(s3BpuPtr) := io.fromBpu.meta.bits.resolveMeta.hintTrace
    s3Trace(s3BpuPtr).generation := io.fromBpu.s3FtqPtr.flag
    metaQueueCommit(s3BpuPtr)   := io.fromBpu.meta.bits.commitMeta

    s3PerfQueue(s3BpuPtr).bpuPerf := io.fromBpu.perfMeta
    debugPerfGeneration.foreach(_(s3BpuPtr) := io.fromBpu.s3FtqPtr)
    debugPerfValid.foreach(_(s3BpuPtr) := true.B)
    s3PerfQueue(s3BpuPtr).isCfi.foreach(_ := false.B)
    s3PerfQueue(s3BpuPtr).mispredict := false.B
  }

  // The entry sitting in s3 always leaves s3 in the next cycle, whether or not it overrides, and an override writes
  // its final prediction straight into Ftq instead of sending it around the Bpu pipeline again. So by the next cycle
  // it has passed the point of no return, and pnrPtr can be advanced past it.
  // A redirect resets pnrPtr along with the other pointers, see the redirect section below.
  when(io.fromBpu.meta.valid) {
    pnrPtr := io.fromBpu.s3FtqPtr + 1.U
  }

  // The entry in s3 can still be overridden, so pnrPtr must never have passed it. In steady state pnrPtr equals
  // s3FtqPtr, so this also catches pnrPtr being advanced too far.
  XSError(
    io.fromBpu.meta.valid && pnrPtr > io.fromBpu.s3FtqPtr,
    "pnrPtr passed the entry that is still in Bpu s3\n"
  )

  resolveQueue.io.bpuEnqueue    := bpuEnqueue
  resolveQueue.io.bpuEnqueuePtr := predictionPtr

  // --------------------------------------------------------------------------------
  // Interaction with ICache and IFU
  // --------------------------------------------------------------------------------

  when(io.toICache.toPrefetch.fire) {
    val twoPrefetchValid = io.toICache.toPrefetch.bits.twoPrefetchCase.valid
    pfPtr := Mux(twoPrefetchValid, pfPtr + 2.U, pfPtr + 1.U)
  }
  when(io.toICache.toMainPipe.fire) {
    fetchPtr := Mux(io.fromICache.fromMainPipe.realTwoFetchValid, fetchPtr + 2.U, fetchPtr + 1.U)
  }

  // TODO: wait for Ifu/ICache to remove bpu s2 flush
  for (stage <- 3 to 3) {
    val redirect = if (stage == 3) prediction.bits.s3Override else false.B
    val ftqIdx   = if (stage == 3) io.fromBpu.s3FtqPtr else 0.U.asTypeOf(new FtqPtr)

    io.toICache.flushFromBpu.stage(stage).valid := redirect
    io.toICache.flushFromBpu.stage(stage).bits  := ftqIdx
    io.toIfu.flushFromBpu.stage(stage).valid    := redirect
    io.toIfu.flushFromBpu.stage(stage).bits     := ftqIdx

    when(redirect) {
      when(pfPtr >= ftqIdx) {
        pfPtr := ftqIdx
      }
      when(fetchPtr >= ftqIdx) {
        fetchPtr := ftqIdx
      }
    }
  }

  if (sys.env.get("H2P_HINT_DIAG").contains("1")) {
    when(bpuS3Redirect) {
      printf(p"H2P_DIAG_S3 cycle=${GTimer()} ftq=${io.fromBpu.s3FtqPtr.value} " +
        p"start_pc=${Hexadecimal(prediction.bits.startPc.toUInt)} target=${Hexadecimal(prediction.bits.target.toUInt)} " +
        p"taken=${prediction.bits.taken} pos=${prediction.bits.endPosition} " +
        p"prediction_fire=${prediction.fire} backend_redirect=${backendRedirect.valid} ifu_redirect=${ifuRedirect.valid} " +
        p"ifu_s3_flush=${io.toIfu.flushFromBpu.stage(3).valid} " +
        p"icache_s3_flush=${io.toICache.flushFromBpu.stage(3).valid} " +
        p"bpu_ptr=${bpuPtr(0).value} pnr_ptr=${pnrPtr(0).value} pf_ptr=${pfPtr(0).value} fetch_ptr=${fetchPtr(0).value}\n")
    }
  }

  // --------------------------------------------------------------------------------
  // 2-prefetch
  // --------------------------------------------------------------------------------

  private val prefetchReq = VecInit(
    Wire(new FtqPrefetchReq).fromFtqEntry(entryQueue(pfPtr(0).value)),
    Wire(new FtqPrefetchReq).fromFtqEntry(entryQueue(pfPtr(1).value))
  )

  private val canTwoPrefetch =
    // to simplify ICache/Ifu bpuFlush logic, we ask the second fetch block to be flushed within Ftq, so it must have
    // passed the point of no return. The first one may still be overridden, ICache/Ifu can flush it themselves.
    // bpu -> | fb4 | fb3 | fb2 | fb1 | fb0 | -> prefetch
    //        bpuPtr             pnrPtr      pfPtr
    passedPnr(pfPtr(1)) &&
      // they also need to be on the same page, to prevent extra itlb port
      prefetchReq(0).vPageNumber === prefetchReq(1).vPageNumber &&
      // and they cannot have known exception or pending satp flush, or we may mark them on the wrong fetch block
      !(hasBackendFlag && (backendFlagPtr === pfPtr(0) || backendFlagPtr === pfPtr(1)))

  // (io.toICache.toPrefetch.fire && twoPrefetchValid) is passed to apply(..., canAssert) to prevent assert(x-state)
  private val twoPrefetchCase = TwoPrefetchCase(prefetchReq, io.toICache.toPrefetch.fire && canTwoPrefetch)

  // FIXME: backend redirect delay should be more than ITLB csr delay
  io.toICache.toPrefetch.valid := bpuPtr(0) > pfPtr(0) && !redirect.valid
  io.toICache.toPrefetch.bits.req.zipWithIndex.foreach { case (req, i) =>
    req.startVAddr    := prefetchReq(i).startVAddr
    req.nextLineVAddr := prefetchReq(i).nextLineVAddr
    req.vSetIdx       := prefetchReq(i).vSetIdx
    req.isCrossLine   := prefetchReq(i).isCrossLine
    req.ftqIdx        := pfPtr(i)
    if (i == 0) {
      req.backendException := Mux(backendFlagPtr === pfPtr(0), backendException, ExceptionType.None)
    } else { // we can do 2-prefetch only when !hasBackendFlag, so setting backendException on i != 0 is useless
      req.backendException := ExceptionType.None
    }
    req.isSoftPrefetch := false.B
  }
  io.toICache.toPrefetch.bits.twoPrefetchCase := Mux(canTwoPrefetch, twoPrefetchCase, TwoPrefetchCase.Conflict)

  // --------------------------------------------------------------------------------
  // 2-fetch
  // --------------------------------------------------------------------------------

  private val fetchReq = VecInit(
    Wire(new FtqFetchReq).fromFtqEntry(entryQueue(fetchPtr(0).value)),
    Wire(new FtqFetchReq).fromFtqEntry(entryQueue(fetchPtr(1).value))
  )

  // as in 2-prefetch, the second fetch block must have passed the point of no return
  private val rawTwoFetchValid = passedPnr(fetchPtr(1)) &&
    (fetchReq(0).size +& fetchReq(1).size) <= FetchBlockInstNum.U && // the unit of fetchReq size is half-word
    fetchReq(0).vPageNumber === fetchReq(1).vPageNumber &&
    !(hasBackendFlag && (backendFlagPtr === fetchPtr(0) || backendFlagPtr === fetchPtr(1)))

  io.toICache.toMainPipe.valid := bpuPtr(0) > fetchPtr(0) && !redirect.valid &&
    distanceBetween(fetchPtr(0), commitPtr(0)) < (FtqSize - 1).U
  io.toICache.toMainPipe.bits.req.zipWithIndex.foreach { case (req, i) =>
    req.valid         := (if (i == 0) true.B else rawTwoFetchValid)
    req.startVAddr    := fetchReq(i).startVAddr
    req.nextLineVAddr := fetchReq(i).nextLineVAddr
    req.taken         := fetchReq(i).taken
    req.endPosition   := fetchReq(i).endPosition
    req.bankSel       := fetchReq(i).bankSel
    req.ftqIdx        := fetchPtr(i)
    req.vSetIdx       := fetchReq(i).vSetIdx
    if (i == 0) {
      req.hasBackendException := backendFlagPtr === fetchPtr(i) && backendException.hasException
      req.hasSatpFlush        := backendFlagPtr === fetchPtr(i) && hasSatpFlush
    } else {
      // similar to 2-prefetch, setting these flags on i != 0 is useless, ICache/Ifu cannot handle them
      req.hasBackendException := false.B
      req.hasSatpFlush        := false.B
    }

  }

  // --------------------------------------------------------------------------------
  // Interaction with backend
  // --------------------------------------------------------------------------------

  io.toBackend.wen     := writeEntry
  io.toBackend.ftqIdx  := predictionPtr.value
  io.toBackend.startPc := prediction.bits.startPc

  // BJU issues a request for its own FTQ entry. Capture the target at the request edge
  // so the response has the same latency as the backend's registered PC read address.
  // An S3 override may write the requested slot at that edge; return the final target.
  io.fromBackend.predTargetRead.zip(io.toBackend.predTargetReadData).foreach { case (req, data) =>
    val target = Mux(
      writeEntry && predictionPtr === req.bits,
      prediction.bits.target,
      entryQueue(req.bits.value).target
    )
    data := RegEnable(target, req.valid)
  }

  // --------------------------------------------------------------------------------
  // Redirect from backend and IFU
  // --------------------------------------------------------------------------------

  io.toICache.redirectFlush := redirect.valid
  when(redirect.valid) {
    val newFtqIdx = redirect.bits.newFtqIdx // redirect.newFtqIdx is a def, make it a val here to prevent dup logic
    Seq(bpuPtr, pfPtr, fetchPtr).foreach(_ := newFtqIdx)
    // nothing from newFtqIdx on has been written yet, so everything before it has passed the point of no return.
    // This is assigned after the s3 update above, so a redirect wins over it, as it does for the other pointers.
    pnrPtr := newFtqIdx
  }

  io.toIfu.redirect.valid := backendRedirect.valid
  // TODO: only valid should be needed
  io.toIfu.redirect.bits := DontCare

  io.toBpu.redirect.valid          := redirect.valid
  io.toBpu.redirect.bits.startPc   := entryQueue(redirect.bits.ftqIdx.value).startPc
  // Redirect.pc differs across producers: load replay already supplies a real PC.
  // The resident FTQ entry and its instruction offset have one common contract.
  // writeEntry is disabled on redirect, so this reads the original generation.
  io.toBpu.redirect.bits.cfiPc     := getCfiPcFromOffset(
    entryQueue(redirect.bits.ftqIdx.value).startPc.unGuard, redirect.bits.ftqOffset)
  io.toBpu.redirect.bits.target    := redirect.bits.target
  io.toBpu.redirect.bits.taken     := redirect.bits.taken
  io.toBpu.redirect.bits.attribute := redirect.bits.attribute
  io.toBpu.redirect.bits.meta      := RegNext(metaQueueRedirect(redirectFtqIdxInAdvance.value))
  io.toBpu.redirect.bits.newFtqIdx := redirect.bits.newFtqIdx
  io.toBpu.redirect.bits.reapply   := redirect.bits.newFtqIdx =/= redirect.bits.ftqIdx
  io.toBpu.redirect.bits.flushItself := redirect.bits.flushItself()
  io.toBpu.redirectFromIFU         := ifuRedirect.valid

  (io.fromBackend.resolve :+ ifuResolve).zip(io.toBpu.hintResolve).zipWithIndex.foreach { case ((src, dst), sourceIndex) =>
    val predicted = entryQueue(src.bits.ftqIdx.value)
    dst.valid := src.valid && HintOwnership.active(src.bits.ftqIdx, commitPtr(0), bpuPtr(0))
    dst.bits.ftqPtr := src.bits.ftqIdx
    // Use the resident FTQ identity directly rather than a separately forwarded base.
    // Backend and IFU offsets both address the final halfword within this window.
    dst.bits.cfiPc := HintResolveCoordinates.internalPc(
      predicted.startPc.unGuard.toUInt, src.bits.ftqOffset, instOffsetBits)
    dst.bits.target := src.bits.target.toUInt.asSInt.pad(64).asUInt
    dst.bits.eligible := src.bits.taken && src.bits.attribute.needIttage
    dst.bits.baselineTarget := predicted.target.toUInt.asSInt.pad(64).asUInt
    dst.bits.baselineValid := predicted.taken &&
      getCfiPcFromPosition(predicted.startPc, predicted.endPosition).toUInt.asSInt.pad(64).asUInt === dst.bits.cfiPc
    dst.bits.offset := src.bits.ftqOffset
    dst.bits.sourceBackend := (sourceIndex < io.fromBackend.resolve.length).B
    dst.bits.fresh := true.B
  }

  if (!env.FPGAPlatform) {
    val dc = xiangshan.DiagnosticDpi.cycle()
    Seq(backendRedirect, ifuRedirect, redirect).zipWithIndex.foreach { case (r, source) =>
      val raw = r.bits
      val ent = entryQueue(raw.ftqIdx.value)
      xiangshan.DiagnosticDpi.emit(clock,reset,dc,10,source,r.valid,
        Seq(raw.ftqIdx.asUInt,raw.newFtqIdx.asUInt,raw.pc,raw.ftqOffset,
          raw.isRVC.asUInt,raw.level.asUInt,raw.robIdx.asUInt,
          raw.target,raw.taken.asUInt,raw.attribute.asUInt,
          ent.startPc.toUInt,io.toBpu.redirect.bits.cfiPc.toUInt,
          Cat(raw.debugIsMemVio,raw.debugIsCtrl,raw.isMisPred,raw.flushItself()),
          (source == 2).B.asUInt))
    }
    (io.fromBackend.resolve :+ ifuResolve).zipWithIndex.foreach { case (r, lane) =>
      val ent = entryQueue(r.bits.ftqIdx.value)
      xiangshan.DiagnosticDpi.emit(clock,reset,dc,11,lane,r.valid,
        Seq(r.bits.ftqIdx.asUInt,r.bits.pc.toUInt,r.bits.ftqOffset,
          r.bits.debug_isRVC.get.asUInt,r.bits.target.toUInt,r.bits.taken.asUInt,
          r.bits.mispredict.asUInt,r.bits.attribute.asUInt,ent.startPc.toUInt,
          io.toBpu.hintResolve(lane).bits.cfiPc,
          getRealCfiPcFromOffset(r.bits.pc,r.bits.ftqOffset,r.bits.debug_isRVC.get),
          (lane < io.fromBackend.resolve.length).B.asUInt))
    }
    xiangshan.DiagnosticDpi.emit(clock,reset,dc,12,0,writeEntry,
      Seq(predictionPtr.asUInt,prediction.bits.startPc.toUInt,
        prediction.bits.endPosition.asUInt,prediction.bits.target.toUInt,
        prediction.bits.s3Override.asUInt))
  }

  resolveQueue.io.backendRedirect    := backendRedirect.valid
  resolveQueue.io.backendRedirectPtr := backendRedirect.bits.ftqIdx

  // --------------------------------------------------------------------------------
  // Resolve and train BPU
  // --------------------------------------------------------------------------------

  resolveQueue.io.backendResolve := io.fromBackend.resolve
  resolveQueue.io.ifuResolve     := ifuResolve

  private val trainCache      = RegInit(0.U.asTypeOf(Valid(new BpuTrain)))
  private val trainIndexCache = RegInit(0.U.asTypeOf(new FtqPtr))

  resolveQueue.io.bpuTrain.ready := !trainCache.valid || io.toBpu.train.fire

  private val flushTrainCache =
    backendRedirect.valid && trainCache.valid && trainIndexCache > backendRedirect.bits.ftqIdx

  private val trainBranches      = resolveQueue.io.bpuTrain.bits.branches
  private val trainCompareMatrix = CompareMatrix(VecInit(trainBranches.map(_.bits.cfiPosition)))
  // All branches after the first mispredicted branch must not update the predictors.
  private val trainFirstMispredictMask = trainCompareMatrix.getLowerElementMask(
    VecInit(trainBranches.map(b => b.valid && b.bits.mispredict))
  )

  when(flushTrainCache) {
    trainCache.valid := false.B
  }.elsewhen(resolveQueue.io.bpuTrain.fire) {
    // Due to timing considerations, resolve queue does not flush resolves in the first cycle of a redirect. As a
    // result, these resolves may be enqueued and flushed in the next cycle. However, they may also be dequeued in the
    // next cycle without being flushed in time. Therefore, the redirect is propagated one cycle later here to prevent
    // this case.
    val needFlush = backendRedirect.valid && resolveQueue.io.bpuTrain.bits.ftqIdx > backendRedirect.bits.ftqIdx ||
      RegNext(backendRedirect.valid) && resolveQueue.io.bpuTrain.bits.ftqIdx > RegNext(backendRedirect.bits.ftqIdx)

    trainCache.valid     := !needFlush
    trainCache.bits.meta := metaQueueResolve(resolveQueue.io.bpuTrain.bits.ftqIdx.value)
    trainCache.bits.startPcVec.foreach { dup =>
      dup.zipWithIndex.foreach { case (startPc, i) =>
        if (i == 0)
          startPc := resolveQueue.io.bpuTrain.bits.startPc // do not align startPcVec.head
        else
          startPc := getAlignedPc(resolveQueue.io.bpuTrain.bits.startPc + (i << FetchBlockAlignWidth).U)
      }
    }
    trainCache.bits.branches.zip(trainBranches).zip(trainFirstMispredictMask).foreach {
      case ((cachedBranch, branch), mask) =>
        cachedBranch.bits  := branch.bits
        cachedBranch.valid := branch.valid && mask
    }
    trainCache.bits.perfMeta     := perfQueue(resolveQueue.io.bpuTrain.bits.ftqIdx.value).bpuPerf
    trainCache.bits.debug_source := resolveQueue.io.bpuTrain.bits.debug_source
    trainIndexCache              := resolveQueue.io.bpuTrain.bits.ftqIdx
  }.elsewhen(io.toBpu.train.fire) {
    trainCache.valid := false.B
  }

  io.toBpu.train.valid := trainCache.valid && !flushTrainCache
  io.toBpu.train.bits  := trainCache.bits

  // default next state receives s3 prediction meta
  perfQueue := s3PerfQueue

  // resolve override next state
  private val lastPerfMetas = Wire(Vec(backendParams.BrhCnt, new PerfMeta))
  io.fromBackend.resolve.zipWithIndex.foreach { case (branch, i) =>
    val ftqIdx = branch.bits.ftqIdx.value
    val lastPerfMeta = WireInit(MuxCase(
      s3PerfQueue(ftqIdx),
      (0 until i).reverse.map { j =>
        val prevBranch = io.fromBackend.resolve(j)
        (prevBranch.valid && prevBranch.bits.ftqIdx.value === ftqIdx) -> lastPerfMetas(j)
      }
    ))
    val curPerfMeta = WireInit(lastPerfMeta)

    when(branch.valid) {
      val newBranchInfo = Wire(new BranchInfo)
      newBranchInfo.fromResolve(branch.bits)

      val curOH     = UIntToOH(newBranchInfo.cfiPosition, FetchBlockInstNum)
      val validMask = UIntToMask(newBranchInfo.cfiPosition +& 1.U, FetchBlockInstNum)
      val beforeKnownMispredict =
        Mux(
          lastPerfMeta.mispredict,
          newBranchInfo.cfiPosition < lastPerfMeta.mispredictBranchInfo.cfiPosition,
          true.B
        )

      when(beforeKnownMispredict) {
        curPerfMeta.isCfi(newBranchInfo.cfiPosition)   := true.B
        curPerfMeta.cfiAttr(newBranchInfo.cfiPosition) := newBranchInfo.attribute
        curPerfMeta.debug_actualTarget.foreach(_(newBranchInfo.cfiPosition) := branch.bits.target.toUInt)
        curPerfMeta.debug_actualPc.foreach(_(newBranchInfo.cfiPosition) := newBranchInfo.debug_realCfiPc.get)
        when(branch.bits.mispredict) {
          curPerfMeta.mispredict           := true.B
          curPerfMeta.mispredictBranchInfo := newBranchInfo
          curPerfMeta.isCfi                := ((lastPerfMeta.isCfi.asUInt | curOH) & validMask).asBools
        }
      }
      perfQueue(ftqIdx) := curPerfMeta
    }
    lastPerfMetas(i) := curPerfMeta
  }

  // A compact actual terminal CFI is part of the existing resolve metadata.
  // It contains no contexts and is retained only until this FTQ commits.
  private val traceResolveByLane = Wire(Vec(backendParams.BrhCnt, new HintTraceResolved))
  private val traceResolveWrite = Wire(Vec(backendParams.BrhCnt, Bool()))
  io.fromBackend.resolve.zipWithIndex.foreach { case (resolved, lane) =>
    val ptr = resolved.bits.ftqIdx
    val resident = entryQueue(ptr.value)
    val previous = WireInit(MuxCase(s3Trace(ptr.value),
      (0 until lane).reverse.map { earlier =>
        val r = io.fromBackend.resolve(earlier)
        (traceResolveWrite(earlier) && r.bits.ftqIdx === ptr) -> traceResolveByLane(earlier)
      }))
    val sameGeneration = HintTraceNextState.generationMatches(previous, ptr, commitPtr(0), bpuPtr(0))
    val killed = redirect.valid && (ptr >= redirect.bits.newFtqIdx ||
      (ptr === redirect.bits.ftqIdx && (resolved.bits.ftqOffset > redirect.bits.ftqOffset ||
        (redirect.bits.flushItself() && resolved.bits.ftqOffset === redirect.bits.ftqOffset))))
    // A younger terminal must not train when an earlier taken/mispredicted
    // branch is resolving on another lane in this cycle, regardless of lane order.
    val earlierBoundary = io.fromBackend.resolve.map { other =>
      other.valid && other.bits.ftqIdx === ptr &&
        (other.bits.taken || other.bits.mispredict) &&
        other.bits.ftqOffset < resolved.bits.ftqOffset
    }.reduce(_ || _)
    io.toBpu.hintResolve(lane).bits.fresh := HintTracePrefix.fresh(previous,
      resolved.bits.ftqOffset, sameGeneration, killed, earlierBoundary)
    val terminal = resolved.bits.taken && resolved.bits.attribute.needIttage
    val originalPosition = getCfiPcFromPosition(resident.startPc, resident.endPosition).toUInt
    val resolvedPosition = getCfiPcFromOffset(resident.startPc.unGuard, resolved.bits.ftqOffset).toUInt
    val (_, actualCarry) = getAlignedPosition(resident.startPc, resolved.bits.ftqOffset)
    val candidate = WireInit(previous)
    candidate.target := resolved.bits.target.toUInt.asSInt.pad(64).asUInt(47, 1)
    candidate.offset := resolved.bits.ftqOffset
    candidate.isRVC := resolved.bits.isRVC
    candidate.baselineMiss := !resident.taken || originalPosition =/= resolvedPosition ||
      resident.target.toUInt =/= resolved.bits.target.toUInt
    val updated = HintTracePrefix.resolve(previous, candidate,
      resolved.valid, sameGeneration, killed,
      resolved.bits.taken || resolved.bits.mispredict, terminal, actualCarry)
    traceResolveWrite(lane) := resolved.valid && sameGeneration && !killed
    traceResolveByLane(lane) := updated
  }
  // One final next-state feeds both storage and commit. Redirect tests the newly
  // selected terminal, never the previous cycle's suffix offset.
  private val traceNext = Wire(Vec(FtqSize, new HintTraceResolved))
  private val traceWrites = io.fromBackend.resolve.zipWithIndex.map { case (r, lane) =>
    (traceResolveWrite(lane), r.bits.ftqIdx.value, traceResolveByLane(lane))
  }
  for (i <- 0 until FtqSize) {
    traceNext(i) := HintTraceNextState(s3Trace(i), i.U, traceWrites,
      redirect.valid, redirect.bits.newFtqIdx, redirect.bits.ftqIdx,
      redirect.bits.ftqOffset, redirect.bits.flushItself())
    metaQueueResolve(i).hintTrace := traceNext(i)
  }

  // --------------------------------------------------------------------------------
  // Commit and train BPU
  // --------------------------------------------------------------------------------

  // Backend may send commit for on entry multiple times, but the entry is actually committed when it is committed for
  // the first time. The rest of the commits can be ignored.
  private val robCommitPtr = DataHoldBypass(
    io.fromBackend.commit.bits,
    FtqPtr(true.B, (FtqSize - 1).U),
    io.fromBackend.commit.valid
  )
  private val commit = commitPtr <= robCommitPtr
  when(commit) {
    commitPtr := commitPtr + 1.U
  }

  io.toBpu.hintCommit.valid := commit
  io.toBpu.hintCommit.bits := commitPtr(0)

  private val traceCommitMeta = traceNext(commitPtr(0).value)
  private val traceCommitEntry = entryQueue(commitPtr(0).value)
  private val traceCommitPc = getRealCfiPcFromOffset(traceCommitEntry.startPc.unGuard,
    traceCommitMeta.offset, traceCommitMeta.isRVC).asSInt.pad(64).asUInt
  // Trace eligibility is actual resolve/redirect state, independent of the
  // prediction and of simulation-only performance observations.
  private val (_, traceCommitCarry) = getAlignedPosition(
    traceCommitEntry.startPc, traceCommitMeta.offset)
  private val traceCommitSurvives = HintTracePrefix.commitAllowed(traceCommitMeta,
    traceCommitMeta.generation === commitPtr(0).flag, traceCommitCarry)
  io.toBpu.hintTraceCommit.valid := commit && traceCommitSurvives
  io.toBpu.hintTraceCommit.bits.snapshot := metaQueueRedirect(commitPtr(0).value).hint
  io.toBpu.hintTraceCommit.bits.actualPc := traceCommitPc
  io.toBpu.hintTraceCommit.bits.actualTarget := Cat(Fill(16, traceCommitMeta.target(46)), traceCommitMeta.target, 0.U(1.W))
  io.toBpu.hintTraceCommit.bits.baselineMiss := traceCommitMeta.baselineMiss
  io.toBpu.hintTraceCommit.bits.queryStartPc := traceCommitEntry.startPc.toUInt.asSInt.pad(64).asUInt
  io.toBpu.hintTraceCommit.bits.ftqTag := commitPtr(0).asUInt
  io.toBpu.hintTraceCommit.bits.pcId := 0.U // actual CAM index is selected by Hint
  io.toBpu.hintTraceCommit.bits.queryValid := traceCommitMeta.generation === commitPtr(0).flag
  io.toBpu.hintTraceCommit.bits.eligible := traceCommitMeta.eligible
  // Simulation-only transport identity; this Option and its performance read
  // are absent from hardware. No trace qualification or payload depends on it.
  io.toBpu.hintTraceCommit.bits.queryCycle.foreach(_ := perfQueue(commitPtr(0).value).bpuPerf.debug_hintQueryCycle.get)
  io.toBpu.hintTraceCommit.bits.baselinePc.foreach(_ := getCfiPcFromPosition(traceCommitEntry.startPc,
    traceCommitEntry.endPosition).toUInt.asSInt.pad(64).asUInt)
  io.toBpu.hintTraceCommit.bits.baselineTarget.foreach(_ := traceCommitEntry.target.toUInt.asSInt.pad(64).asUInt)
  io.toBpu.hintTraceCommit.bits.baselineValid.foreach(_ := traceCommitEntry.taken &&
    getCfiPcFromPosition(traceCommitEntry.startPc, traceCommitEntry.endPosition).toUInt ===
      getCfiPcFromOffset(traceCommitEntry.startPc.unGuard, traceCommitMeta.offset).toUInt)

  commitQueue.io.backendCommit := io.fromBackend.callRetCommit

  io.toBpu.commit.valid                     := commitQueue.io.bpuTrain.valid
  io.toBpu.commit.bits.meta                 := metaQueueCommit(commitQueue.io.bpuTrain.bits.ftqPtr.value)
  io.toBpu.commit.bits.attribute.branchType := DontCare
  io.toBpu.commit.bits.attribute.rasAction  := commitQueue.io.bpuTrain.bits.rasAction

  // --------------------------------------------------------------------------------
  // Performance monitoring
  // --------------------------------------------------------------------------------

  // Topdown analysis
  io.backendRedirectTopdown.backendRedirect         := backendRedirect.valid
  io.backendRedirectTopdown.controlFlowRedirect     := backendRedirect.bits.debugIsCtrl
  io.backendRedirectTopdown.memoryViolationRedirect := backendRedirect.bits.debugIsMemVio

  io.backendRedirectTopdown.btbMissBubble    := false.B // TODO: add more info to distinguish
  io.backendRedirectTopdown.tageMissBubble   := backendRedirect.bits.attribute.isConditional
  io.backendRedirectTopdown.scMissBubble     := false.B // TODO: add SC info
  io.backendRedirectTopdown.ittageMissBubble := backendRedirect.bits.attribute.needIttage
  io.backendRedirectTopdown.rasMissBubble    := backendRedirect.bits.attribute.isReturn

  private val topdownStage = RegInit(0.U.asTypeOf(new FrontendTopDownBundle))
  // only driven by clock, not valid-ready
  topdownStage := io.fromBpu.topdownReasons
  topdownStage.backendRedirectOverride(io.backendRedirectTopdown)
  io.toIfu.topdownInfo := topdownStage

  when(!(distanceBetween(bpuPtr(0), commitPtr(0)) < FtqSize.U)) {
    topdownStage.reasons(TopDownCounters.FtqFullStall.id) := true.B
  }.elsewhen(
    !(distanceBetween(bpuPtr(0), fetchPtr(0)) < BpRunAheadDistance.U && bpTrainStallCnt < BpTrainStallLimit.U)
  ) {
    topdownStage.reasons(TopDownCounters.FtqUpdateBubble.id) := true.B
  }

  // Hardware performance monitors
  val perfEvents: Seq[(String, UInt)] = Seq()
  generatePerfEvent()

  // XSPerfCounters
  private val redirectCfiOffset = getAlignedPosition(
    PcInit(redirect.bits.pc),
    redirect.bits.ftqOffset
  )._1
  private val redirectPerfMeta = perfQueue(backendRedirect.bits.ftqIdx.value).bpuPerf
  private val commitPerfMeta   = perfQueue(commitPtr(0).value)

  if (!env.FPGAPlatform) {
    // Sample each actual ROB retirement, without waiting for FTQ block reclamation.
    // FTQ metadata is still resident; full ptr generation + exact decoded PC/offset are checked.
    for (retired <- io.fromBackend.debugRetired.get) {
      val meta = perfQueue(retired.bits.ftqPtr.value)
      val (position, carry) = getAlignedPosition(entryQueue(retired.bits.ftqPtr.value).startPc,
        retired.bits.ftqOffset)
      val observedAttribute = BranchAttribute.decode(retired.bits.instr, retired.valid)
      val realPc = getRealCfiPcFromOffset(entryQueue(retired.bits.ftqPtr.value).startPc,
        retired.bits.ftqOffset, retired.bits.isRVC).asSInt.pad(64).asUInt
      val observer = Module(new CommittedIndirectDpi)
      observer.clock := clock
      observer.io.valid := !reset.asBool && retired.valid && observedAttribute.needIttage
      observer.io.metadataValid := debugPerfValid.get(retired.bits.ftqPtr.value) &&
        debugPerfGeneration.get(retired.bits.ftqPtr.value) === retired.bits.ftqPtr &&
        !carry && meta.isCfi(position) && meta.cfiAttr(position).needIttage &&
        meta.debug_actualPc.get(position).asSInt.pad(64).asUInt === retired.bits.pc &&
        realPc === retired.bits.pc
      observer.io.pc := retired.bits.pc
      observer.io.actual := meta.debug_actualTarget.get(position).asSInt.pad(64).asUInt
      observer.io.ordinal := retired.bits.ordinal
      observer.io.instr := retired.bits.instr
      observer.io.isRVC := retired.bits.isRVC
      observer.io.ftqTag := retired.bits.ftqPtr.asUInt
      observer.io.offset := retired.bits.ftqOffset
      observer.io.queryCycle := meta.bpuPerf.debug_hintQueryCycle.get
      val finalPred = meta.bpuPerf.bpPred
      val baselinePred = meta.bpuPerf.debug_preHintPrediction.get
      observer.io.baselineTarget := baselinePred.target.toUInt.asSInt.pad(64).asUInt
      // Accuracy counts the target supplied at the actual CFI, even when its
      // predicted type differs. Type approximation does not itself imply a miss.
      observer.io.baselineValid := baselinePred.taken && baselinePred.cfiPosition === position
      observer.io.finalTarget := finalPred.target.toUInt.asSInt.pad(64).asUInt
      observer.io.finalValid := finalPred.taken && finalPred.cfiPosition === position
      observer.io.usedHint := meta.bpuPerf.debug_usedHint.get &&
        meta.bpuPerf.bpSource.s3Override && observer.io.finalValid
      when(observer.io.valid && !observer.io.metadataValid) {
        printf(p"ROB_TARGET_METADATA_DIAG ordinal=${retired.bits.ordinal} pc=${Hexadecimal(retired.bits.pc)} " +
          p"instr=${Hexadecimal(retired.bits.instr)} ftq=${retired.bits.ftqPtr.asUInt} offset=${retired.bits.ftqOffset} " +
          p"valid=${debugPerfValid.get(retired.bits.ftqPtr.value)} generation=${debugPerfGeneration.get(retired.bits.ftqPtr.value).asUInt} " +
          p"start=${Hexadecimal(entryQueue(retired.bits.ftqPtr.value).startPc.toUInt)} position=${position} carry=${carry} " +
          p"isRVC=${retired.bits.isRVC} realPc=${Hexadecimal(realPc)} isCfi=${meta.isCfi(position)} " +
          p"needIttage=${meta.cfiAttr(position).needIttage} actualPc=${Hexadecimal(meta.debug_actualPc.get(position))} " +
          p"actualTarget=${Hexadecimal(meta.debug_actualTarget.get(position))}\n")
      }
    }
  }

  // Elaboration-only diagnostics. The regular RTL has no trace logic or output.
  if (sys.env.get("H2P_HINT_DIAG").contains("1")) {
    val actual = backendRedirect.bits
    val perf = redirectPerfMeta
    val entry = entryQueue(actual.ftqIdx.value)
    val allMatch = actual.taken === perf.bpPred.taken &&
      redirectCfiOffset === perf.bpPred.cfiPosition &&
      actual.attribute === perf.bpPred.attribute &&
      actual.target === perf.bpPred.target.toUInt
    when(backendRedirect.valid) {
      printf(p"H2P_DIAG_FTQ cycle=${GTimer()} ftq=${actual.ftqIdx.value} pc=${Hexadecimal(actual.pc)} " +
        p"mispred=${actual.isMisPred} actual_taken=${actual.taken} actual_pos=${redirectCfiOffset} " +
        p"actual_attr=${Hexadecimal(actual.attribute.asUInt)} actual_target=${Hexadecimal(actual.target)} " +
        p"final_taken=${perf.bpPred.taken} final_pos=${perf.bpPred.cfiPosition} " +
        p"final_attr=${Hexadecimal(perf.bpPred.attribute.asUInt)} final_target=${Hexadecimal(perf.bpPred.target.toUInt)} " +
        p"s1_taken=${perf.s1Prediction.taken} s1_pos=${perf.s1Prediction.cfiPosition} " +
        p"s1_attr=${Hexadecimal(perf.s1Prediction.attribute.asUInt)} s1_target=${Hexadecimal(perf.s1Prediction.target.toUInt)} " +
        p"s3_taken=${perf.s3Prediction.taken} s3_pos=${perf.s3Prediction.cfiPosition} " +
        p"s3_attr=${Hexadecimal(perf.s3Prediction.attribute.asUInt)} s3_target=${Hexadecimal(perf.s3Prediction.target.toUInt)} " +
        p"s3_override=${perf.bpSource.s3Override} s1_source=${Hexadecimal(perf.bpSource.s1Source.asUInt)} " +
        p"s3_source=${Hexadecimal(perf.bpSource.s3Source.asUInt)} all_match=${allMatch} " +
        p"entry_taken=${entry.taken} entry_pos=${entry.endPosition} entry_target=${Hexadecimal(entry.target.toUInt)} " +
        p"bpu_ptr=${bpuPtr(0).value} fetch_ptr=${fetchPtr(0).value} pnr_ptr=${pnrPtr(0).value}\n")
    }
  }

  XSPerfSeqAccumulate(
    "squash_cycles_bp_wrong_redirect",
    backendRedirect.valid && backendRedirect.bits.isMisPred,
    Seq(
      ("wrong_taken", redirect.bits.taken =/= redirectPerfMeta.bpPred.taken),
      ("wrong_position", redirectCfiOffset =/= redirectPerfMeta.bpPred.cfiPosition),
      ("wrong_attribute", !(redirect.bits.attribute === redirectPerfMeta.bpPred.attribute)),
      ("wrong_target", redirect.bits.target =/= redirectPerfMeta.bpPred.target.toUInt)
    ),
    withPriority = true
  )

  XSPerfSeqAccumulate(
    "squash_cycles_bp_wrong_redirect_wrong_target",
    backendRedirect.valid && backendRedirect.bits.isMisPred &&
      redirect.bits.taken === redirectPerfMeta.bpPred.taken &&
      redirectCfiOffset === redirectPerfMeta.bpPred.cfiPosition &&
      redirect.bits.attribute === redirectPerfMeta.bpPred.attribute &&
      redirect.bits.target =/= redirectPerfMeta.bpPred.target.toUInt,
    Seq(
      ("conditional", redirect.bits.attribute.isConditional),
      ("direct", redirect.bits.attribute.isDirect),
      ("indirect", redirect.bits.attribute.isIndirect),
      ("indirect_ret_call", redirect.bits.attribute.isReturnAndCall && redirect.bits.attribute.isIndirect)
    )
  )

  private val perf_mispredS1SourceVec = BpuPredictionSource.Stage1.getValidSeq(redirectPerfMeta.bpSource.s1Source)
  private val perf_mispredS3SourceVec = BpuPredictionSource.Stage3.getValidSeq(redirectPerfMeta.bpSource.s3Source)

  XSPerfSeqAccumulate(
    "resolve_branch_mispredicts_s1_source",
    backendRedirect.valid && backendRedirect.bits.isMisPred && !redirectPerfMeta.bpSource.s3Override,
    perf_mispredS1SourceVec
  )

  XSPerfSeqAccumulate(
    "resolve_branch_mispredicts_s3_source",
    backendRedirect.valid && backendRedirect.bits.isMisPred && redirectPerfMeta.bpSource.s3Override,
    perf_mispredS3SourceVec
  )
  XSPerfAccumulate("resolve_redirects", backendRedirect.valid)
  XSPerfAccumulate("resolve_branch_mispredicts", backendRedirect.valid && backendRedirect.bits.isMisPred)
  XSPerfAccumulate("resolve_other_redirects", backendRedirect.valid && !backendRedirect.bits.isMisPred)

  // Commit-time statistics, should be correct-path only
  XSPerfSeqAccumulate(
    "commit_branch",
    commit,
    Seq(
      ("num", true.B, PopCount(commitPerfMeta.isCfi)),
      ("mispredicts", true.B, commitPerfMeta.mispredict)
    )
  )

  private def PerfNumCfiWithAttr(
      perfMeta: PerfMeta,
      withAttr: BranchAttribute => Bool
  ): UInt =
    PopCount(perfMeta.isCfi zip perfMeta.cfiAttr map { case (v, attr) => v && withAttr(attr) })

  XSPerfSeqAccumulate(
    "commit_branch_type",
    commit,
    Seq(
      ("conditional", true.B, PerfNumCfiWithAttr(commitPerfMeta, attr => attr.isConditional)),
      ("direct", true.B, PerfNumCfiWithAttr(commitPerfMeta, attr => attr.isDirect)),
      ("indirect", true.B, PerfNumCfiWithAttr(commitPerfMeta, attr => attr.isIndirect)),
      (
        "indirect_retcall",
        true.B,
        PerfNumCfiWithAttr(commitPerfMeta, attr => attr.isReturnAndCall && attr.isIndirect)
      ),
      ("call", true.B, PerfNumCfiWithAttr(commitPerfMeta, attr => attr.isCall)),
      ("ret", true.B, PerfNumCfiWithAttr(commitPerfMeta, attr => attr.isReturn))
    )
  )

  private val perf_commitHasMispredict = commit && commitPerfMeta.mispredict
  private val perf_commitHasMispredictConditional =
    perf_commitHasMispredict && commitPerfMeta.mispredictBranchInfo.attribute.isConditional

  XSPerfSeqAccumulate(
    "commit_branch_mispredicts_s1_mispred_s1_source",
    perf_commitHasMispredict && !commitPerfMeta.bpuPerf.bpSource.s3Override,
    BpuPredictionSource.Stage1.getValidSeq(commitPerfMeta.bpuPerf.bpSource.s1Source)
  )
  XSPerfSeqAccumulate(
    "commit_branch_mispredicts_s1_source",
    perf_commitHasMispredict,
    BpuPredictionSource.Stage1.getValidSeq(commitPerfMeta.bpuPerf.bpSource.s1Source)
  )
  XSPerfSeqAccumulate(
    "commit_branch_mispredicts_s3_source",
    perf_commitHasMispredict,
    BpuPredictionSource.Stage3.getValidSeq(commitPerfMeta.bpuPerf.bpSource.s3Source)
  )
  XSPerfSeqAccumulate(
    "commit_branch_mispredicts_reason",
    perf_commitHasMispredict,
    BlameBpuSource.BlameType.getValidSeq(BlameBpuSource(
      perf_commitHasMispredict,
      commitPerfMeta.bpuPerf,
      commitPerfMeta.mispredictBranchInfo
    ))
  )
  XSPerfSeqAccumulate(
    "commit_conditional_branch_mispredicts_reason",
    perf_commitHasMispredictConditional,
    BlameBpuSource.BlameType.getValidSeq(BlameBpuSource(
      perf_commitHasMispredictConditional,
      commitPerfMeta.bpuPerf,
      commitPerfMeta.mispredictBranchInfo
    ))
  )

  private val mispredictAttr = commitPerfMeta.mispredictBranchInfo.attribute
  XSPerfSeqAccumulate(
    "commit_branch_mispredicts_type",
    perf_commitHasMispredict,
    Seq(
      ("conditional", mispredictAttr.isConditional),
      ("direct", mispredictAttr.isDirect),
      ("indirect", mispredictAttr.isIndirect),
      ("indirect_retcall", mispredictAttr.isReturnAndCall && mispredictAttr.isIndirect),
      ("call", mispredictAttr.isCall),
      ("ret", mispredictAttr.isReturn)
    )
  )

  XSPerfSeqRolling(
    "rolling_commit_mispredict",
    perf_commitHasMispredict,
    Seq(
      ("conditional", mispredictAttr.isConditional),
      ("direct", mispredictAttr.isDirect),
      ("indirect", mispredictAttr.isIndirect),
      ("indirect_retcall", mispredictAttr.isReturnAndCall && mispredictAttr.isIndirect),
      ("call", mispredictAttr.isCall),
      ("ret", mispredictAttr.isReturn)
    ),
    10000,
    clock,
    reset
  )

  XSPerfRolling("rolling_ifu_redirect", io.fromIfu.wbRedirect.valid, 10000, clock, reset)

  XSPerfHistogram(
    "distance_between_bpu_commit",
    distanceBetween(bpuPtr(0), commitPtr(0)),
    true.B,
    0,
    FtqSize + 1
  )
  XSPerfHistogram(
    "distance_between_ifu_commit",
    distanceBetween(fetchPtr(0), commitPtr(0)),
    true.B,
    0,
    FtqSize + 1
  )
  XSPerfHistogram(
    "distance_between_bpu_ifu",
    distanceBetween(bpuPtr(0), fetchPtr(0)),
    true.B,
    0,
    FtqSize + 1
  )
  XSPerfAccumulate(
    "total_commits",
    commit
  )
  XSPerfSeqAccumulate(
    "2prefetch",
    io.toICache.toPrefetch.fire && io.toICache.toPrefetch.bits.twoPrefetchCase.valid,
    Seq(
      ("total", true.B)
    ) ++ io.toICache.toPrefetch.bits.twoPrefetchCase.getValidSeq
  )
  XSPerfSeqAccumulate(
    "2prefetch_fail_reason",
    io.toICache.toPrefetch.fire && !io.toICache.toPrefetch.bits.twoPrefetchCase.valid,
    Seq(
      ("fb_not_passed_pnr", !passedPnr(pfPtr(1))),
      ("fb1_exception", backendException.hasException && backendFlagPtr === pfPtr(0)),
      ("fb2_exception", backendException.hasException && backendFlagPtr === pfPtr(1)),
      ("page_conflict", prefetchReq(0).vPageNumber =/= prefetchReq(1).vPageNumber),
      ("sram_conflict", twoPrefetchCase.isConflict)
    ),
    withPriority = true
  )
  XSPerfAccumulate(
    "total_fetch",
    io.toICache.toMainPipe.fire
  )
  XSPerfAccumulate(
    "1fetch",
    io.toICache.toMainPipe.fire && !io.fromICache.fromMainPipe.realTwoFetchValid
  )
  XSPerfAccumulate(
    "2fetch",
    io.toICache.toMainPipe.fire && io.fromICache.fromMainPipe.realTwoFetchValid
  )
  XSPerfSeqAccumulate(
    "2fetch_fail_reason",
    io.toICache.toMainPipe.fire && !io.fromICache.fromMainPipe.realTwoFetchValid,
    Seq(
      ("fb_not_passed_pnr", !passedPnr(fetchPtr(1))),
      ("fb1_exception", backendException.hasException && backendFlagPtr === fetchPtr(0)),
      ("fb2_exception", backendException.hasException && backendFlagPtr === fetchPtr(1)),
      ("total_size", (fetchReq(0).size +& fetchReq(1).size) > FetchBlockInstNum.U),
      ("page_conflict", fetchReq(0).vPageNumber =/= fetchReq(1).vPageNumber)
    ) ++ TwoFetchFailReason.getValidSeq(io.fromICache.fromMainPipe.perf_twoFetchFailReason),
    withPriority = true
  )
  if (!env.FPGAPlatform) {
    val dc = xiangshan.DiagnosticDpi.cycle()
    io.toBpu.train.bits.branches.zipWithIndex.foreach { case (b,lane) =>
      xiangshan.DiagnosticDpi.emit(clock,reset,dc,18,lane,io.toBpu.train.fire,
        Seq(trainIndexCache.asUInt,io.toBpu.train.bits.startPc.toUInt,
          b.valid.asUInt,b.bits.cfiPosition,b.bits.target.toUInt,b.bits.taken.asUInt,
          b.bits.mispredict.asUInt,b.bits.attribute.asUInt,
          b.bits.debug_realCfiPc.get,io.toBpu.train.bits.debug_source))
    }
    val rawIfu = io.fromIfu.wbRedirect
    xiangshan.DiagnosticDpi.emit(clock,reset,dc,19,0,rawIfu.valid,
      Seq(rawIfu.bits.ftqIdx.asUInt,rawIfu.bits.pc,rawIfu.bits.ftqOffset,
        rawIfu.bits.isRVC.asUInt,rawIfu.bits.target,rawIfu.bits.taken.asUInt,
        rawIfu.bits.attribute.asUInt,rawIfu.bits.canTrain.asUInt,backendRedirect.valid.asUInt))
  }

}
