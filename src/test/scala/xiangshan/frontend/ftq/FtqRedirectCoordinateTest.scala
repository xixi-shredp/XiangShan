package xiangshan.frontend.ftq

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import top.BackendV2NoRVVNoLLCConfig
import xiangshan.{DebugOptionsKey, XSCoreParamsKey, XSTileKey}
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}

/** Actual finite FTQ entry registers and both redirect receivers; no copied PC formula. */
class FtqRedirectCoordinateTest extends AnyFlatSpec with ChiselSim {
  private val config = new BackendV2NoRVVNoLLCConfig
  implicit val params: org.chipsalliance.cde.config.Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
    case DebugOptionsKey => config(DebugOptionsKey).copy(FPGAPlatform = true)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, true, false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.NORMAL, 0)
  }
  behavior of "FTQ canonical redirect endpoint"
  it should "use the resident entry rather than producer PC and forward actual flush level" in {
    simulate(new Ftq) { m =>
      def inputs(d: Data): Unit = d match {
        case x: Bool if DataMirror.directionOf(x) == ActualDirection.Input => x.poke(false.B)
        case x: UInt if DataMirror.directionOf(x) == ActualDirection.Input => x.poke(0.U)
        case x: Vec[_] => x.getElements.foreach(inputs)
        case x: Record => x.elements.values.foreach(inputs)
        case _ => ()
      }
      def ptr(p: FtqPtr, id: Int): Unit = {
        p.flag.poke(((id & 64) != 0).B); p.value.poke((id & 63).U)
      }
      def reset(): Unit = {
        inputs(m.io); m.reset.poke(true.B); m.clock.step(3); m.reset.poke(false.B)
        m.clock.step(2)
      }
      def predict(start: Int): Unit = {
        val p=m.io.fromBpu.prediction
        p.bits.startPc.addr.poke((start >> 1).U); p.bits.target.addr.poke(((start+64) >> 1).U)
        p.bits.endPosition.poke(31.U); p.bits.taken.poke(false.B); p.bits.s3Override.poke(false.B)
        p.valid.poke(true.B); p.ready.expect(true.B); m.clock.step(); p.valid.poke(false.B)
      }
      var checks=0
      // Exact regex replay, EXU base and an unrelated raw producer PC must agree.
      for(raw <- Seq(0xca58e,0xca566,0x4000); self <- Seq(false,true)) {
        reset(); predict(0xca566)
        val r=m.io.fromBackend.redirect
        ptr(r.bits.ftqIdx,0); r.bits.ftqOffset.poke(20.U); r.bits.isRVC.poke(true.B)
        r.bits.pc.poke(raw.U); r.bits.target.poke(0xca58e.U); r.bits.level.poke(self.B)
        r.valid.poke(true.B); m.clock.step(); r.valid.poke(false.B)
        m.io.toBpu.redirect.valid.expect(true.B)
        m.io.toBpu.redirect.bits.cfiPc.addr.expect((0xca58e >> 1).U)
        m.io.toBpu.redirect.bits.target.addr.expect((0xca58e >> 1).U)
        m.io.toBpu.redirect.bits.flushItself.expect(self.B)
        m.io.toBpu.redirect.bits.reapply.expect(true.B)
        // A competing S3 entry update on the actual redirect edge is suppressed.
        val p=m.io.fromBpu.prediction
        p.bits.s3Override.poke(true.B); p.bits.startPc.addr.poke((0x6000 >> 1).U)
        p.bits.target.addr.poke((0x7000 >> 1).U); ptr(m.io.fromBpu.s3FtqPtr,0)
        p.valid.poke(true.B)
        m.io.toBpu.redirect.bits.cfiPc.addr.expect((0xca58e >> 1).U)
        m.clock.step(); p.valid.poke(false.B); checks+=6
      }
      // Nonaligned start, endpoint across the block edge, IFU always flushAfter.
      reset(); predict(0x103e)
      val i=m.io.fromIfu.wbRedirect
      ptr(i.bits.ftqIdx,0); i.bits.ftqOffset.poke(3.U); i.bits.isRVC.poke(false.B)
      i.bits.pc.poke(0xdead.U); i.bits.target.poke(0x2000.U)
      i.bits.taken.poke(false.B); i.bits.canTrain.poke(false.B); i.valid.poke(true.B)
      m.clock.step(); i.valid.poke(false.B)
      m.io.toBpu.redirect.valid.expect(true.B)
      m.io.toBpu.redirect.bits.cfiPc.addr.expect((0x1044 >> 1).U)
      m.io.toBpu.redirect.bits.flushItself.expect(false.B)
      m.io.toBpu.redirect.bits.target.addr.expect((0x2000 >> 1).U)
      checks+=4
      // The first noncompressed instruction has endpoint offset1 but whole FTQ is killed.
      reset(); predict(0x103e)
      val r=m.io.fromBackend.redirect
      ptr(r.bits.ftqIdx,0); r.bits.ftqOffset.poke(1.U); r.bits.isRVC.poke(false.B)
      r.bits.pc.poke(0x103e.U); r.bits.target.poke(0x103e.U); r.bits.level.poke(true.B)
      r.valid.poke(true.B); m.clock.step(); r.valid.poke(false.B)
      m.io.toBpu.redirect.bits.cfiPc.addr.expect((0x1040 >> 1).U)
      m.io.toBpu.redirect.bits.flushItself.expect(true.B)
      m.io.toBpu.redirect.bits.reapply.expect(false.B)
      checks+=3
      println(s"FTQ_REDIRECT_COORDINATE_REAL_RTL_PASS checks=$checks raw_pc_independent=1 forced_state=0")
    }
  }
}
