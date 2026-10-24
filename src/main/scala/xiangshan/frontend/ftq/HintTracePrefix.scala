package xiangshan.frontend.ftq

import chisel3._
import xiangshan.frontend.bpu.hint.HintTraceResolved

/** Actual FTQ prefix, retained in the existing resolve metadata.
  * valid identifies a known terminal boundary; eligible distinguishes an actual
  * taken indirect from a non-profiled taken branch or a misprediction boundary.
  * Keeping the latter prevents a late wrong-path resolve from refilling a suffix.
  */
object HintTracePrefix {
  def within(previous: HintTraceResolved, offset: UInt): Bool =
    !previous.valid || offset <= previous.offset

  def fresh(previous: HintTraceResolved, offset: UInt, sameGeneration: Bool,
      killed: Bool, earlierBoundary: Bool): Bool =
    sameGeneration && !killed && within(previous, offset) && !earlierBoundary &&
      !(previous.valid && previous.eligible && previous.offset === offset)

  def resolve(
      previous: HintTraceResolved,
      candidate: HintTraceResolved,
      resolvedValid: Bool,
      sameGeneration: Bool,
      killed: Bool,
      endsWindow: Bool,
      terminal: Bool,
      carry: Bool
  ): HintTraceResolved = {
    val updated = WireInit(previous)
    when(resolvedValid && sameGeneration && !killed && !carry && endsWindow &&
      within(previous, candidate.offset)) {
      updated := candidate
      updated.valid := true.B
      updated.eligible := terminal
      updated.generation := previous.generation
    }
    updated
  }

  def commitAllowed(meta: HintTraceResolved, generationMatches: Bool, carry: Bool): Bool =
    meta.valid && meta.eligible && generationMatches && !carry
}
