package xiangshan.frontend.ifu

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.BackendV2NoRVVNoLLCConfig
import xiangshan._
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.wrapper.JumpUnit
import xiangshan.frontend.GuardedPcInit

/** Exercise the production predecode and jump modules without a core or predictor replacement. */
class PredCheckerJumpHarness(implicit p: Parameters) extends IfuModule {
  val io = IO(new Bundle {
    val reqValid = Input(Bool())
    val active = Input(Vec(IBufferEnqueueWidth, Bool()))
    val predTaken = Input(Vec(IBufferEnqueueWidth, Bool()))
    val branchType = Input(Vec(IBufferEnqueueWidth, UInt(2.W)))
    val rasAction = Input(Vec(IBufferEnqueueWidth, UInt(2.W)))
    val isRvc = Input(Vec(IBufferEnqueueWidth, Bool()))
    val invalidTaken = Input(Vec(IBufferEnqueueWidth, Bool()))
    val blockSel = Input(Vec(IBufferEnqueueWidth, Bool()))
    val crossBlock = Input(Vec(IBufferEnqueueWidth, Bool()))
    val offset = Input(Vec(IBufferEnqueueWidth, UInt(FetchBlockInstOffsetWidth.W)))
    val pc = Input(Vec(IBufferEnqueueWidth, UInt(64.W)))
    val jumpOffset = Input(Vec(IBufferEnqueueWidth, UInt(64.W)))
    val lane = Input(UInt(log2Ceil(IBufferEnqueueWidth).W))
    val actualTarget = Input(UInt(64.W))
    val predTarget = Input(UInt(64.W))
    val fixedTaken = Output(Vec(IBufferEnqueueWidth, Bool()))
    val fixedValid = Output(Vec(IBufferEnqueueWidth, Bool()))
    val ifuRedirectValid = Output(Bool())
    val ifuRedirectTarget = Output(UInt(64.W))
    val ifuRedirectTaken = Output(Bool())
    val ifuRedirectLane = Output(UInt(log2Ceil(IBufferEnqueueWidth).W))
    val ifuRedirectBlock = Output(Bool())
    val ifuRedirectOffset = Output(UInt(FetchBlockInstOffsetWidth.W))
    val jumpRedirectValid = Output(Bool())
    val jumpActualTarget = Output(UInt(64.W))
    val jumpTrainMispredict = Output(Bool())
  })
  val checker = Module(new PredChecker)
  checker.io.req.valid := io.reqValid
  checker.io.req.bits := 0.U.asTypeOf(checker.io.req.bits)
  for (i <- 0 until IBufferEnqueueWidth) {
    val in = checker.io.req.bits.instrVec(i)
    in.valid := io.active(i)
    in.isPredTaken := io.predTaken(i)
    in.isRvc := io.isRvc(i)
    in.invalidTaken := io.invalidTaken(i)
    in.blockSel := io.blockSel(i)
    in.startOffset := io.offset(i)
    in.endOffset := io.offset(i)
    in.isCrossBlockInstr := io.crossBlock(i)
    val pd = checker.io.req.bits.pdInfoVec(i)
    pd.valid := io.active(i)
    pd.isRVC := io.isRvc(i)
    pd.brAttribute.branchType := io.branchType(i)
    pd.brAttribute.rasAction := io.rasAction(i)
    checker.io.req.bits.instrPcVec(i) := GuardedPcInit(io.pc(i)(GuardedVAddrBits - 1, 0))
    checker.io.req.bits.jumpOffsetVec(i) := GuardedPcInit(io.jumpOffset(i)(GuardedVAddrBits - 1, 0))
  }
  io.fixedTaken := checker.io.resp.stage1Out.fixedTaken
  io.fixedValid := checker.io.resp.stage1Out.fixedInstrValid
  val red = checker.io.resp.stage2Out.checkerRedirect
  io.ifuRedirectValid := red.valid
  io.ifuRedirectTarget := red.bits.target.toUInt
  io.ifuRedirectTaken := red.bits.taken
  io.ifuRedirectLane := red.bits.misIdx.bits
  io.ifuRedirectBlock := red.bits.selectBlock
  io.ifuRedirectOffset := red.bits.endOffset

  val jump = Module(new JumpUnit(FuConfig.JmpCfg))
  val lane = io.lane
  jump.io.flush := 0.U.asTypeOf(jump.io.flush)
  jump.io.in.bits := 0.U.asTypeOf(jump.io.in.bits)
  jump.io.in.valid := io.reqValid && io.fixedValid(lane) &&
    (io.branchType(lane) === 2.U || io.branchType(lane) === 3.U)
  jump.io.out.ready := true.B
  jump.io.in.bits.ctrl.fuOpType := Mux(io.branchType(lane) === 3.U, JumpOpType.jalr, JumpOpType.jal)
  jump.io.in.bits.ctrl.isRVC.get := io.isRvc(lane)
  jump.io.in.bits.ctrl.rasAction.get := io.rasAction(lane)
  jump.io.in.bits.ctrl.predictInfo.get.fixedTaken := io.fixedTaken(lane)
  jump.io.in.bits.ctrl.predictInfo.get.predTaken := io.predTaken(lane)
  jump.io.in.bits.ctrl.predictInfo.get.target := io.predTarget
  jump.io.in.bits.ctrl.toRobValid := true.B
  jump.io.in.bits.data.src(0) := io.actualTarget
  jump.io.in.bits.data.pc.get := io.pc(lane)
  jump.io.in.bits.data.imm := Mux(io.branchType(lane) === 2.U, io.jumpOffset(lane), 0.U)
  jump.io.in.bits.data.nextPcOffset.get := Mux(io.isRvc(lane), 2.U, 4.U)
  jump.io.in.bits.ctrlPipe.foreach(_.foreach(_ := jump.io.in.bits.ctrl))
  jump.io.in.bits.validPipe.foreach(_.foreach(_ := jump.io.in.valid))
  jump.io.in.bits.dataPipe.foreach(_.foreach(_ := jump.io.in.bits.data))
  jump.io.instrAddrTransType.get := 0.U.asTypeOf(jump.io.instrAddrTransType.get)
  jump.io.instrAddrTransType.get.bare := true.B
  io.jumpRedirectValid := jump.io.out.bits.res.redirect.get.valid
  io.jumpActualTarget := jump.io.out.bits.res.redirect.get.bits.target
  io.jumpTrainMispredict := jump.io.toFrontendBJUResolve.get.bits.mispredict
}

class PredCheckerJumpTest extends AnyFlatSpec with ChiselSim {
  private val cfg = new BackendV2NoRVVNoLLCConfig
  implicit val p: Parameters = cfg.alterPartial {
    case XSCoreParamsKey => cfg(XSTileKey).head
  }
  behavior of "Unpredicted indirect backend recovery"
  it should "recover a sequential no-pop JR even when unrelated target bits equal the actual target" in {
    simulate(new PredCheckerJumpHarness) { m =>
      val n = m.io.active.length
      def clear(): Unit = {
        m.io.reqValid.poke(true.B)
        m.io.lane.poke(5.U)
        m.io.actualTarget.poke(0xa98de.U)
        m.io.predTarget.poke(0xa98de.U)
        for (i <- 0 until n) {
          m.io.active(i).poke(false.B)
          m.io.predTaken(i).poke(false.B)
          m.io.branchType(i).poke(0.U)
          m.io.rasAction(i).poke(0.U)
          m.io.isRvc(i).poke(false.B)
          m.io.invalidTaken(i).poke(false.B)
          m.io.blockSel(i).poke(false.B)
          m.io.crossBlock(i).poke(false.B)
          m.io.offset(i).poke(i.U)
          m.io.pc(i).poke((0x1000 + i * 2).U)
          m.io.jumpOffset(i).poke(0x200.U)
        }
      }
      def branch(i: Int, kind: Int, ras: Int, pred: Boolean, rvc: Boolean, pc: Int = 0xa98ae): Unit = {
        m.io.active(i).poke(true.B)
        m.io.branchType(i).poke(kind.U)
        m.io.rasAction(i).poke(ras.U)
        m.io.predTaken(i).poke(pred.B)
        m.io.isRvc(i).poke(rvc.B)
        m.io.pc(i).poke(pc.U)
      }

      // Actual failure: compressed JR resumes at PC+2, while old target bits happen to be correct.
      clear()
      branch(5, 3, 0, pred = false, rvc = true)
      m.io.offset(5).poke(7.U)
      m.io.fixedValid(5).expect(true.B)
      m.io.fixedTaken(5).expect(false.B)
      m.io.jumpActualTarget.expect(0xa98de.U)
      m.io.jumpRedirectValid.expect(true.B)
      m.io.jumpTrainMispredict.expect(true.B)
      m.clock.step()
      m.io.ifuRedirectValid.expect(true.B)
      m.io.ifuRedirectTarget.expect(0xa98b0.U)
      m.io.ifuRedirectTaken.expect(true.B) // IFU history contract remains taken.
      m.io.ifuRedirectOffset.expect(7.U)

      // Already predicted JR: correct target remains fast; wrong target must recover.
      clear()
      branch(5, 3, 0, pred = true, rvc = true)
      m.io.fixedTaken(5).expect(true.B)
      m.io.jumpRedirectValid.expect(false.B)
      m.io.jumpTrainMispredict.expect(false.B)
      m.io.predTarget.poke(0xa98dc.U)
      m.io.jumpRedirectValid.expect(true.B)
      m.io.jumpTrainMispredict.expect(true.B)

      // Exact per-instruction contracts for legal CFI classes, validity, prediction, and RVC lengths.
      val attrs = Seq((0, 0), (1, 0), (2, 0), (2, 2), (3, 0), (3, 2), (3, 1), (3, 3))
      for ((kind, ras) <- attrs; pred <- Seq(false, true); rvc <- Seq(false, true); valid <- Seq(false, true)) {
        clear()
        branch(5, kind, ras, pred, rvc)
        m.io.active(5).poke(valid.B)
        val expected = if (kind == 2) true else if (kind == 3) !valid || (ras & 1) != 0 || pred else pred && kind != 0
        m.io.fixedTaken(5).expect(expected.B)
        m.io.fixedValid(5).expect(valid.B)
        if (valid && kind == 3 && (ras & 1) == 0 && !pred)
          m.io.jumpRedirectValid.expect(true.B)
        if (valid && kind == 3 && (ras & 1) != 0) {
          m.io.fixedTaken(5).expect(true.B) // Return/co-routine RAS correction path is untouched.
          m.io.jumpRedirectValid.expect(false.B)
        }
      }

      // An early missing JR preserves the prefix mask and suppresses later taken CFIs from both blocks.
      clear()
      branch(0, 1, 0, pred = false, rvc = true, pc = 0x1010)
      branch(1, 3, 0, pred = false, rvc = true, pc = 0x1012)
      branch(2, 2, 0, pred = true, rvc = true, pc = 0x1020)
      branch(3, 3, 1, pred = true, rvc = true, pc = 0x1022)
      m.io.blockSel(2).poke(true.B)
      m.io.blockSel(3).poke(true.B)
      m.io.fixedValid(0).expect(true.B)
      m.io.fixedValid(1).expect(true.B)
      m.io.fixedTaken(0).expect(false.B)
      m.io.fixedTaken(1).expect(false.B)
      for (i <- 2 until n) m.io.fixedValid(i).expect(false.B)
      m.clock.step()
      m.io.ifuRedirectLane.expect(1.U)
      m.io.ifuRedirectTarget.expect(0x1014.U)
      m.io.ifuRedirectTaken.expect(true.B)

      // Cross-block RVI and RVC JRs retain the correct sequential target and selected-block identity.
      for (rvc <- Seq(false, true)) {
        clear()
        branch(5, 3, 0, pred = false, rvc = rvc, pc = 0x101e)
        m.io.blockSel(5).poke(true.B)
        m.io.crossBlock(5).poke((!rvc).B)
        m.io.offset(5).poke((if (rvc) 15 else 16).U)
        m.io.jumpRedirectValid.expect(true.B)
        m.clock.step()
        m.io.ifuRedirectValid.expect(true.B)
        m.io.ifuRedirectTarget.expect((if (rvc) 0x1020 else 0x1022).U)
        m.io.ifuRedirectBlock.expect(true.B)
        m.io.ifuRedirectTaken.expect(true.B)
      }

      // Direct JAL still uses its decoded target for the IFU correction.
      clear()
      branch(5, 2, 2, pred = false, rvc = false, pc = 0x1200)
      m.io.predTarget.poke(0x1400.U)
      m.io.fixedTaken(5).expect(true.B)
      m.io.jumpRedirectValid.expect(false.B)
      m.clock.step()
      m.io.ifuRedirectValid.expect(true.B)
      m.io.ifuRedirectTarget.expect(0x1400.U)
      m.io.ifuRedirectTaken.expect(true.B)
    }
  }
}
