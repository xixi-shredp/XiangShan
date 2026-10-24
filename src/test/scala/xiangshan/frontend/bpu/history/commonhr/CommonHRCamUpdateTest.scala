package xiangshan.frontend.bpu.history.commonhr

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.BackendV2NoRVVNoLLCConfig
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{DebugOptionsKey, XSCoreParamsKey, XSTileKey}

/** Exercise the production CommonHR, including its real history queue/redirect. */
class CommonHRCamUpdateTest extends AnyFlatSpec with ChiselSim {
  private val config = new BackendV2NoRVVNoLLCConfig
  implicit val params: Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
    case DebugOptionsKey => config(DebugOptionsKey).copy(FPGAPlatform = true)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, true, false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.NORMAL, 0)
  }
  behavior of "CommonHR CAM-selected indirect CFI"
  it should "count only earlier deduplicated conditionals without a BTB onehot and restore native positions" in {
    simulate(new CommonHR) { top =>
      def zero(d: Data): Unit = d match {
        case r: Record => r.elements.values.foreach(zero)
        case v: Vec[_] => v.getElements.foreach(zero)
        case b: Bool if DataMirror.directionOf(b) == ActualDirection.Input => b.poke(false.B)
        case u: UInt if DataMirror.directionOf(u) == ActualDirection.Input => u.poke(0.U)
        case _ => ()
      }
      val io = top.io
      def stages(s0: Boolean = false, s1: Boolean = false, s2: Boolean = false, s3: Boolean = false): Unit = {
        io.stageCtrl.s0_fire.poke(s0.B); io.stageCtrl.s1_fire.poke(s1.B)
        io.stageCtrl.s2_fire.poke(s2.B); io.stageCtrl.s3_fire.poke(s3.B)
      }
      def positions(values: Seq[Int]): Unit = {
        require(values.length <= io.s2CondHitMask.length)
        for (i <- io.s2CondHitMask.indices) {
          io.s2CondHitMask(i).poke((i < values.length).B)
          io.s2CfiPositions(i).poke(values.lift(i).getOrElse(0).U)
          io.s2CfiTargets(i).addr.poke(0.U)
        }
      }
      def update(cam: Boolean, position: Int, conditional: Boolean = false, correction: Boolean = false): Unit = {
        zero(io.update)
        io.update.taken.poke(true.B); io.update.camSelected.poke(cam.B)
        io.update.s3Override.poke(correction.B)
        io.update.firstTakenBranch.valid.poke(true.B)
        io.update.firstTakenBranch.bits.taken.poke(true.B)
        io.update.firstTakenBranch.bits.cfiPosition.poke(position.U)
        io.update.firstTakenBranch.bits.attribute.branchType.poke((if (conditional) 1 else 3).U)
        if (conditional) {
          io.update.firstTakenBranchOH(0).poke(true.B)
          io.update.attributes(0).branchType.poke(1.U)
        }
      }
      zero(io); top.reset.poke(true.B); top.clock.step(3); top.reset.poke(false.B)
      // Fill the existing three-stage history queue before the first S3 event.
      stages(s0 = true); top.clock.step()
      stages(s0 = true, s1 = true); top.clock.step()
      positions(Seq(2)); stages(s0 = true, s1 = true, s2 = true); top.clock.step()
      update(cam = false, position = 2, conditional = true)
      stages(s0 = true, s1 = true, s2 = false, s3 = true)
      io.s0_commonHR.ghr.expect(1.U); io.s0_commonHR.bw.expect(1.U)
      top.clock.step()
      // A duplicate conditional counts once. An old conditional at CAM position8
      // is reclassified and excluded from both update and saved recovery mask.
      positions(Seq(2, 5, 5, 8, 12)); stages(s2 = true); top.clock.step()
      update(cam = true, position = 8); stages(s0 = true, s3 = true)
      io.update.firstTakenBranchOH.foreach(_.expect(false.B))
      io.s0_commonHR.ghr.expect(4.U); io.s0_commonHR.bw.expect(4.U)
      io.s3DedupHitMask(0).expect(true.B); io.s3DedupHitMask(1).expect(true.B)
      io.s3DedupHitMask(2).expect(false.B); io.s3DedupHitMask(3).expect(false.B)
      io.s3DedupHitMask(4).expect(true.B)
      top.clock.step()
      // S3 correction follows the same CAM-only path with no fabricated OH.
      positions(Seq(3, 3, 8)); stages(s2 = true); top.clock.step()
      update(cam = true, position = 8, correction = true); stages(s0 = true, s3 = true)
      top.clock.step()
      zero(io.update); positions(Seq.empty)
      // Refill the two younger windows canceled by S3 correction. These windows
      // have no conditionals, so the previous CAM update must remain 4<<1=8.
      stages(s0 = true); top.clock.step(2)
      stages(s2 = true); top.clock.step()
      stages(s0 = true, s3 = true)
      io.s0_commonHR.ghr.expect(8.U); io.s0_commonHR.bw.expect(8.U)
      top.clock.step(); stages()

      // Redirect in the second 32B segment uses full native position21, not
      // cfiPc[4:1]=5. Earlier positions2 and17 must both contribute zeros.
      zero(io.redirect); io.redirect.valid.poke(true.B)
      io.redirect.cfiPc.addr.poke((0x102a >> 1).U); io.redirect.cfiPosition.poke(21.U)
      io.redirect.target.addr.poke((0x2000 >> 1).U)
      io.redirect.taken.poke(true.B); io.redirect.attribute.branchType.poke(3.U)
      io.redirect.meta.ghr.poke(3.U); io.redirect.meta.bw.poke(5.U)
      for ((pos, i) <- Seq(2, 17, 21, 24, 17, 1).zipWithIndex) {
        io.redirect.meta.position(i).poke(pos.U)
        io.redirect.meta.attribute(i).branchType.poke((if (i == 5) 2 else 1).U)
        io.redirect.meta.hitMask(i).poke((pos != 21).B)
      }
      top.clock.step(); io.redirect.valid.poke(false.B)
      io.s0_commonHR.ghr.expect(12.U); io.s0_commonHR.bw.expect(20.U)
      top.clock.step()
      println("COMMON_HR_CAM_UPDATE_PASS no_btb_oh=1 dedup_prefix=1 same_position_removed=1 s3_correction=1 younger_windows=1 native_redirect_position=1")
    }
  }
}
