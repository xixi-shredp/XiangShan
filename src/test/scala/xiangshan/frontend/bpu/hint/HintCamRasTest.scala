package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.BackendV2NoRVVNoLLCConfig
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{DebugOptionsKey, XSCoreParamsKey, XSTileKey}
import xiangshan.frontend.GuardedPc
import xiangshan.frontend.bpu.{BpuModule, Prediction, StageCtrl}
import xiangshan.frontend.bpu.ras.{MicroRas, Ras}

/** Real RAS modules consume production selection and prediction-difference logic. */
class HintCamRasProbe(implicit p: Parameters) extends BpuModule {
  val selection = Module(new HintCfiSelection)
  val micro = Module(new MicroRas)
  val ras = Module(new Ras)
  val io = IO(new Bundle {
    val select = selection.io.cloneType
    val stages = Input(new StageCtrl)
    val s1StartPc = Input(GuardedPc())
    val s3StartPc = Input(GuardedPc())
    val s1Prediction = Input(new Prediction)
    val overrideReason = Output(UInt(4.W))
    val microValid = Output(Bool())
    val microTarget = Output(GuardedPc())
    val mainTarget = Output(GuardedPc())
    val pushes = Output(UInt(16.W))
  })
  io.select <> selection.io
  for (predictor <- Seq(micro.io, ras.io)) {
    predictor.enable := true.B
    predictor.stageCtrl := io.stages
    predictor.startPc := io.s1StartPc
    predictor.train := 0.U.asTypeOf(predictor.train)
  }
  micro.io.specIn.startPc := io.s1StartPc
  micro.io.specIn.cfiPosition := io.s1Prediction.cfiPosition
  micro.io.specIn.attribute := io.s1Prediction.attribute
  micro.io.hasRedirect := false.B
  private val overrideReason = HintPredictionDiff(selection.io.prediction, io.s1Prediction)
  io.overrideReason := overrideReason
  micro.io.overrideData.valid := io.stages.s3_fire && overrideReason.orR
  micro.io.overrideData.bits.startPc := io.s3StartPc
  micro.io.overrideData.bits.cfiPosition := selection.io.prediction.cfiPosition
  micro.io.overrideData.bits.attribute := selection.io.prediction.attribute
  micro.io.fullRetAddr := ras.io.topRetAddr
  ras.io.specIn.valid := io.stages.s3_fire
  ras.io.specIn.bits.startPc := io.s3StartPc.toUInt
  ras.io.specIn.bits.cfiPosition := selection.io.prediction.cfiPosition
  ras.io.specIn.bits.attribute := selection.io.prediction.attribute
  ras.io.commit := 0.U.asTypeOf(ras.io.commit)
  ras.io.redirect := 0.U.asTypeOf(ras.io.redirect)
  io.microValid := micro.io.specOut.isCanUse
  io.microTarget := micro.io.specOut.retTarget
  io.mainTarget := ras.io.topRetAddr
  io.pushes := ras.io.redirectMeta.tosw.asUInt
}

class HintCamRasTest extends AnyFlatSpec with ChiselSim {
  private val config = new BackendV2NoRVVNoLLCConfig
  implicit val params: Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
    case DebugOptionsKey => config(DebugOptionsKey).copy(FPGAPlatform = true)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, true, false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.NORMAL, 0)
  }
  behavior of "CAM-selected call and real RAS state"
  it should "push C+2 once, repair wrong S1 actions and suppress later CAM calls" in {
    simulate(new HintCamRasProbe) { top =>
      def zero(d: Data): Unit = d match {
        case r: Record => r.elements.values.foreach(zero)
        case v: Vec[_] => v.getElements.foreach(zero)
        case b: Bool if DataMirror.directionOf(b) == ActualDirection.Input => b.poke(false.B)
        case u: UInt if DataMirror.directionOf(u) == ActualDirection.Input => u.poke(0.U)
        case _ => ()
      }
      def pc(p: GuardedPc, value: Int): Unit = p.addr.poke((value >> 1).U)
      val io = top.io
      def resetCase(): Unit = {
        zero(io); top.reset.poke(true.B); top.clock.step(3); top.reset.poke(false.B)
      }
      def call(start: Int, pos: Int): Unit = {
        pc(io.s3StartPc, start); io.select.position.poke(pos.U)
        io.select.classify.poke(true.B); io.select.isCall.poke(true.B)
        io.stages.s3_fire.poke(true.B)
      }
      for ((start, pos, returnPc) <- Seq((0x1002, 1, 0x1004),
        (0x101e, 16, 0x1022), (0x103e, 16, 0x1042))) {
        resetCase(); call(start, pos)
        top.clock.step(); io.stages.s3_fire.poke(false.B)
        io.microValid.expect(true.B); io.microTarget.addr.expect((returnPc >> 1).U)
        io.mainTarget.addr.expect((returnPc >> 1).U); io.pushes.expect(1.U)
        top.clock.step(2); io.pushes.expect(1.U)
        // One pop returns to the initial stack, catching duplicate pushes even
        // when two duplicate return addresses could otherwise look identical.
        io.select.classify.poke(false.B); io.select.original.taken.poke(true.B)
        io.select.original.attribute.branchType.poke(3.U)
        io.select.original.attribute.rasAction.poke(1.U)
        io.stages.s3_fire.poke(true.B)
        top.clock.step(); io.stages.s3_fire.poke(false.B)
        io.mainTarget.addr.expect(0.U)
      }
      resetCase()
      // S1 initially calls a direct branch. S3 corrects only its branch type at
      // the same position/target; MicroRAS must replace, not double-push, S1 state.
      pc(io.s1StartPc, 0x103e); io.s1Prediction.cfiPosition.poke(16.U)
      io.s1Prediction.taken.poke(true.B)
      io.s1Prediction.attribute.branchType.poke(2.U); io.s1Prediction.attribute.rasAction.poke(2.U)
      io.stages.s1_fire.poke(true.B); top.clock.step()
      io.microTarget.addr.expect((0x1042 >> 1).U)
      io.stages.s1_fire.poke(false.B); io.stages.s2_fire.poke(true.B); top.clock.step()
      io.stages.s2_fire.poke(false.B)
      io.select.original.taken.poke(true.B); io.select.original.cfiPosition.poke(16.U)
      io.select.original.attribute.branchType.poke(2.U); io.select.original.attribute.rasAction.poke(2.U)
      call(0x103e, 16); io.overrideReason.expect(4.U); top.clock.step()
      io.stages.s3_fire.poke(false.B)
      io.pushes.expect(1.U); io.microTarget.addr.expect((0x1042 >> 1).U)
      // Incorrect speculative push and pop are each erased by a selected S3
      // earlier conditional; CAM's later isCall must not create an extra push.
      for (wrongAction <- Seq(2, 1)) {
        pc(io.s1StartPc, 0x1000); io.s1Prediction.cfiPosition.poke(4.U)
        io.s1Prediction.attribute.branchType.poke(3.U)
        io.s1Prediction.attribute.rasAction.poke(wrongAction.U)
        io.stages.s1_fire.poke(true.B); top.clock.step(); io.stages.s1_fire.poke(false.B)
        io.select.original.cfiPosition.poke(3.U)
        io.select.original.attribute.branchType.poke(1.U); io.select.original.attribute.rasAction.poke(0.U)
        io.select.position.poke(16.U); io.select.isCall.poke(true.B)
        io.stages.s3_fire.poke(true.B)
        io.select.camSelected.expect(false.B)
        top.clock.step(); io.stages.s3_fire.poke(false.B)
        io.microValid.expect(true.B); io.microTarget.addr.expect((0x1042 >> 1).U)
        io.mainTarget.addr.expect((0x1042 >> 1).U); io.pushes.expect(1.U)
      }
      println("HINT_CAM_RAS_PASS real_micro_and_main=1 native_c_plus_2=1 unaligned_second_segment=1 one_push=1 wrong_s1_push_pop_repaired=1 earlier_taken_suppresses_call=1")
    }
  }
}
