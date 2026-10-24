package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.GuardedPc
import xiangshan.frontend.bpu.{BpuModule, BranchAttribute, Prediction}

/** Offline CAM ABI: old 52 bits, native end-halfword position, then call bit. */
class HintCamEntry extends Bundle {
  val isCall = Bool()                 // 57
  val position = UInt(5.W)           // 56:52
  val valid = Bool()                  // 51
  val mask = UInt(4.W)               // 50:47
  val pc = UInt(47.W)                // 46:0, prediction-window start
}

/** Shared S3 override predicate, including identity-only corrections. */
object HintPredictionDiff {
  def apply(finalPrediction: Prediction, earlier: Prediction): UInt = {
    val taken = finalPrediction.taken =/= earlier.taken
    val position = finalPrediction.taken && finalPrediction.cfiPosition =/= earlier.cfiPosition
    val attribute = finalPrediction.taken && finalPrediction.attribute =/= earlier.attribute
    val target = finalPrediction.taken && finalPrediction.target =/= earlier.target
    Cat(target, attribute, position, taken)
  }
}

/** CAM classification is independent of target lookup admission and confidence.
  * The baseline has already selected its earliest taken branch. A CAM branch
  * replaces the same position, but cannot hide an earlier taken branch.
  */
class HintCfiSelection(implicit p: Parameters) extends BpuModule {
  val io = IO(new Bundle {
    val original = Input(new Prediction)
    val classify = Input(Bool())
    val position = Input(UInt(CfiPositionWidth.W))
    val isCall = Input(Bool())
    val ittageHit = Input(Bool())
    val ittageTarget = Input(GuardedPc())
    val btbTarget = Input(Valid(GuardedPc()))
    val fallThroughTarget = Input(GuardedPc())
    val hintUsable = Input(Bool())
    val hintTarget = Input(GuardedPc())
    val camSelected = Output(Bool())
    val usedHint = Output(Bool())
    val beforeHint = Output(new Prediction)
    val prediction = Output(new Prediction)
  })
  require(CfiPositionWidth == 5, "58-bit Hint CAM uses a five-bit native CFI position")
  io.camSelected := io.classify && (!io.original.taken || io.position <= io.original.cfiPosition)
  io.usedHint := io.camSelected && io.hintUsable
  io.beforeHint := io.original
  when(io.camSelected) {
    io.beforeHint.cfiPosition := io.position
    io.beforeHint.taken := true.B
    io.beforeHint.attribute.branchType := BranchAttribute.BranchType.Indirect
    io.beforeHint.attribute.rasAction := Mux(io.isCall,
      BranchAttribute.RasAction.Push, BranchAttribute.RasAction.None)
    io.beforeHint.target := Mux(io.ittageHit, io.ittageTarget,
      Mux(io.btbTarget.valid, io.btbTarget.bits, io.fallThroughTarget))
  }
  io.prediction := io.beforeHint
  when(io.usedHint) { io.prediction.target := io.hintTarget }
}
