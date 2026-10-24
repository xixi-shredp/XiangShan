package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.BackendV2NoRVVNoLLCConfig
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.frontend.GuardedPc
import xiangshan.frontend.bpu.{BpuModule, HalfAlignHelper}

class HintCfiSelectionProbe(implicit p: Parameters) extends BpuModule with HalfAlignHelper {
  private val selection = Module(new HintCfiSelection)
  val io = IO(new Bundle {
    val select = selection.io.cloneType
    val startPc = Input(GuardedPc())
    val isRVC = Input(Bool())
    val endPc = Output(UInt(64.W))
    val firstPc = Output(UInt(64.W))
    val needIttage = Output(Bool())
    val hasPush = Output(Bool())
    val hasPop = Output(Bool())
  })
  io.select <> selection.io
  private val prediction = selection.io.prediction
  io.endPc := getCfiPcFromPosition(io.startPc, prediction.cfiPosition).toUInt.pad(64)
  io.firstPc := getRealCfiPcFromOffset(io.startPc,
    getFtqOffset(io.startPc, prediction.cfiPosition), io.isRVC).pad(64)
  io.needIttage := prediction.attribute.needIttage
  io.hasPush := prediction.attribute.hasPush
  io.hasPop := prediction.attribute.hasPop
}

class HintCfiSelectionTest extends AnyFlatSpec with ChiselSim {
  private val config = new BackendV2NoRVVNoLLCConfig
  implicit val params: Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
  }
  behavior of "Production CAM CFI selection"
  it should "classify independently of target admission and retain native halfword/call semantics" in {
    simulate(new HintCfiSelectionProbe) { top =>
      def zeroInputs(d: Data): Unit = d match {
        case r: Record => r.elements.values.foreach(zeroInputs)
        case v: Vec[_] => v.getElements.foreach(zeroInputs)
        case b: Bool if DataMirror.directionOf(b) == ActualDirection.Input => b.poke(false.B)
        case u: UInt if DataMirror.directionOf(u) == ActualDirection.Input => u.poke(0.U)
        case _ => ()
      }
      def pc(p: GuardedPc, value: Int): Unit = p.addr.poke((value >> 1).U)
      zeroInputs(top.io)
      val io = top.io.select
      pc(top.io.startPc, 0x1002); top.io.isRVC.poke(true.B)
      pc(io.original.target, 0x2000); pc(io.ittageTarget, 0x3000)
      pc(io.btbTarget.bits, 0x4000); pc(io.fallThroughTarget, 0x5000); pc(io.hintTarget, 0x6000)
      io.original.cfiPosition.poke(12.U); io.position.poke(8.U)
      // Hint Off must preserve the existing type, position and target exactly.
      io.original.taken.poke(true.B); io.original.attribute.branchType.poke(2.U)
      io.classify.poke(false.B); io.hintUsable.poke(true.B)
      io.camSelected.expect(false.B); io.usedHint.expect(false.B)
      io.prediction.attribute.branchType.expect(2.U); io.prediction.cfiPosition.expect(12.U)
      io.prediction.target.addr.expect((0x2000 >> 1).U)
      io.classify.poke(true.B)
      // Same-position misclassification and missing-BTB cases all classify.
      for ((kind, ras, taken) <- Seq((0, 0, false), (1, 0, true), (2, 0, true), (3, 1, true));
           isCall <- Seq(false, true)) {
        io.original.attribute.branchType.poke(kind.U); io.original.attribute.rasAction.poke(ras.U)
        io.original.taken.poke(taken.B); io.original.cfiPosition.poke(8.U)
        io.isCall.poke(isCall.B); io.hintUsable.poke(false.B)
        io.camSelected.expect(true.B); io.prediction.taken.expect(true.B)
        io.prediction.cfiPosition.expect(8.U); io.prediction.attribute.branchType.expect(3.U)
        top.io.needIttage.expect(true.B); top.io.hasPush.expect(isCall.B); top.io.hasPop.expect(false.B)
      }
      // Earlier taken conditional/direct/return wins even if Hint is usable.
      for ((kind, ras) <- Seq((1, 0), (2, 0), (3, 1))) {
        io.original.taken.poke(true.B); io.original.cfiPosition.poke(3.U)
        io.original.attribute.branchType.poke(kind.U); io.original.attribute.rasAction.poke(ras.U)
        io.hintUsable.poke(true.B); io.camSelected.expect(false.B); io.usedHint.expect(false.B)
        io.prediction.cfiPosition.expect(3.U); io.prediction.target.addr.expect((0x2000 >> 1).U)
        io.prediction.attribute.branchType.expect(kind.U); io.prediction.attribute.rasAction.expect(ras.U)
      }
      // conf0/miss/busy/full all supply usable=false; classification remains,
      // while target fallback is ITTAGE then qualified same-position BTB then fallthrough.
      io.original.cfiPosition.poke(12.U); io.hintUsable.poke(false.B)
      for ((ittage, btb, target) <- Seq((true, true, 0x3000), (true, false, 0x3000),
        (false, true, 0x4000), (false, false, 0x5000))) {
        io.ittageHit.poke(ittage.B); io.btbTarget.valid.poke(btb.B)
        io.camSelected.expect(true.B); io.usedHint.expect(false.B)
        io.beforeHint.target.addr.expect((target >> 1).U)
        io.prediction.target.addr.expect((target >> 1).U)
      }
      io.hintUsable.poke(true.B); io.usedHint.expect(true.B)
      io.prediction.target.addr.expect((0x6000 >> 1).U)
      io.beforeHint.target.addr.expect((0x5000 >> 1).U)
      // Native position is relative to the 32B base, including its next-segment bit.
      // The same endpoint is legal for a compressed instruction or a split 32b instruction.
      for ((start, pos, rvc, end, first) <- Seq(
        (0x1002, 1, true, 0x1002, 0x1002), (0x1002, 1, false, 0x1002, 0x1000),
        (0x101e, 16, false, 0x1020, 0x101e), (0x103e, 16, true, 0x1040, 0x1040),
        (0x1002, 31, false, 0x103e, 0x103c))) {
        pc(top.io.startPc, start); io.position.poke(pos.U); top.io.isRVC.poke(rvc.B)
        io.original.taken.poke(false.B)
        top.io.endPc.expect(end.U); top.io.firstPc.expect(first.U)
      }
      println("HINT_CFI_SELECTION_PASS mode_gate=1 wrong_type=1 earlier_taken=1 fallback_priority=1 independent_admission=1 native_position=1 rvc_split=1 call_push=1")
    }
  }
}
