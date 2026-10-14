package xiangshan.frontend.ftq

import chisel3._
import xiangshan.frontend.bpu.hint.HintTraceResolved

/** Stateless actual-CFI survival rules shared by FTQ and its module tests.
  * The caller supplies existing resolve-order perf metadata; no history,
  * prediction, pointer, or additional generation state is stored here.
  */
object HintTracePrefix {
  def resolve(
      previous: HintTraceResolved,
      candidate: HintTraceResolved,
      resolvedValid: Bool,
      sameGeneration: Bool,
      killed: Bool,
      terminal: Bool,
      knownMispredict: Bool,
      cutoff: UInt,
      candidatePosition: UInt,
      previousPosition: UInt,
      candidateCarry: Bool,
      previousCarry: Bool
  ): HintTraceResolved = {
    val updated = WireInit(previous)
    val candidateSurvives = !candidateCarry &&
      (!knownMispredict || candidatePosition <= cutoff)
    val previousSurvives = previous.valid && !previousCarry &&
      (!knownMispredict || previousPosition <= cutoff)
    when(resolvedValid && sameGeneration && !killed) {
      // An earlier conditional can invalidate a saved indirect terminal.
      when(!previousSurvives) {
        updated := 0.U.asTypeOf(chiselTypeOf(previous))
        updated.generation := previous.generation
      }
      when(terminal && candidateSurvives &&
        (!previousSurvives || candidate.offset <= previous.offset)) {
        updated := candidate
        updated.valid := true.B
        updated.eligible := true.B
        updated.generation := previous.generation
      }
    }
    updated
  }

  def commitAllowed(
      meta: HintTraceResolved,
      generationMatches: Bool,
      isCfi: Bool,
      needIttage: Bool,
      carry: Bool,
      knownMispredict: Bool,
      position: UInt,
      cutoff: UInt
  ): Bool = meta.valid && generationMatches && !carry && isCfi && needIttage &&
    (!knownMispredict || position <= cutoff)
}
