package xiangshan.frontend.ftq

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{DebugOptionsKey, XSCoreParamsKey, XSTileKey}

/** Exercises the real FTQ entry queue and its backend target-read port. */
class FtqTargetReadTest extends AnyFlatSpec with ChiselSim {
  private val config = new DefaultConfig
  implicit val params: org.chipsalliance.cde.config.Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
  }.alter((site, here, up) => {
    case LogUtilsOptionsKey => LogUtilsOptions(
      here(DebugOptionsKey).EnableDebug,
      here(DebugOptionsKey).EnablePerfDebug,
      here(DebugOptionsKey).FPGAPlatform,
      here(DebugOptionsKey).EnableXMR
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      enablePerfPrint = false,
      enablePerfDB = false,
      perfLevel = XSPerfLevel.NORMAL,
      perfDBHartID = 0
    )
  })

  behavior of "FTQ backend predicted-target transport"

  it should "return the current entry even without a next entry, and handle overrides, redirects and wrap" in {
    simulate(new Ftq) { m =>
      def ptr(port: FtqPtr, value: Int, flag: Boolean = false): Unit = {
        port.value.poke(value.U)
        port.flag.poke(flag.B)
      }
      def prediction(start: Int, target: Int, s3: Boolean = false): Unit = {
        m.io.fromBpu.prediction.bits.startPc.addr.poke((start >> 1).U)
        m.io.fromBpu.prediction.bits.target.addr.poke((target >> 1).U)
        m.io.fromBpu.prediction.bits.taken.poke(true.B)
        m.io.fromBpu.prediction.bits.endPosition.poke(0.U)
        m.io.fromBpu.prediction.bits.s3Override.poke(s3.B)
        m.io.fromBpu.prediction.valid.poke(true.B)
      }
      val read = m.io.fromBackend.predTargetRead(0)
      val response = m.io.toBackend.predTargetReadData(0)
      def request(value: Int, flag: Boolean = false): Unit = {
        ptr(read.bits, value, flag)
        read.valid.poke(true.B)
      }
      def expectTarget(target: Int): Unit = response.addr.expect((target >> 1).U)
      def stopPrediction(): Unit = {
        m.io.fromBpu.prediction.valid.poke(false.B)
        m.io.fromBpu.prediction.bits.s3Override.poke(false.B)
      }
      def ifuRedirect(idx: Int): Unit = {
        ptr(m.io.fromIfu.wbRedirect.bits.ftqIdx, idx)
        m.io.fromIfu.wbRedirect.bits.ftqOffset.poke(0.U)
        m.io.fromIfu.wbRedirect.bits.isRVC.poke(true.B)
        m.io.fromIfu.wbRedirect.bits.pc.poke(0.U)
        m.io.fromIfu.wbRedirect.bits.target.poke(0.U)
        m.io.fromIfu.wbRedirect.bits.taken.poke(false.B)
        m.io.fromIfu.wbRedirect.bits.canTrain.poke(false.B)
        m.io.fromIfu.wbRedirect.bits.attribute.branchType.poke(0.U)
        m.io.fromIfu.wbRedirect.bits.attribute.rasAction.poke(0.U)
        m.io.fromIfu.wbRedirect.valid.poke(true.B)
        m.clock.step() // IFU redirect is registered before it reaches FTQ.
        m.io.fromIfu.wbRedirect.valid.poke(false.B)
        m.io.toICache.redirectFlush.expect(true.B)
      }

      stopPrediction()
      m.io.fromBpu.meta.valid.poke(false.B)
      ptr(m.io.fromBpu.s3FtqPtr, 0)
      m.io.fromIfu.wbRedirect.valid.poke(false.B)
      m.io.fromBackend.redirect.valid.poke(false.B)
      m.io.fromBackend.ftqIdxAhead.valid.poke(false.B)
      m.io.fromBackend.commit.valid.poke(false.B)
      m.io.fromBackend.callRetCommit.foreach(_.valid.poke(false.B))
      m.io.fromBackend.resolve.foreach(_.valid.poke(false.B))
      m.io.fromBackend.predTargetRead.foreach(_.valid.poke(false.B))
      m.io.toBpu.train.ready.poke(false.B)
      m.io.toICache.toPrefetch.ready.poke(false.B)
      m.io.toICache.toMainPipe.ready.poke(false.B)

      // Only entry 0 exists: the old backend idx+1 lookup would see an unwritten slot.
      prediction(0x1000, 0x2000)
      m.io.fromBpu.prediction.ready.expect(true.B)
      m.io.toBackend.wen.expect(true.B)
      request(0)
      m.clock.step()
      expectTarget(0x2000) // Same-edge normal write/read bypass, one-cycle response.
      stopPrediction()
      request(0)
      m.clock.step()
      expectTarget(0x2000) // Stored target remains valid without entry 1.

      read.valid.poke(false.B)
      ptr(read.bits, 1)
      m.clock.step()
      expectTarget(0x2000) // Invalid request holds the last registered response.

      prediction(0x1004, 0x3000)
      request(1)
      m.io.fromBpu.prediction.ready.expect(true.B)
      m.clock.step()
      expectTarget(0x3000)
      stopPrediction()
      request(0)
      expectTarget(0x3000) // Request 0 has not crossed the clock edge yet.
      m.clock.step()
      expectTarget(0x2000)

      // S3 replaces entry 0 at the request edge; backend must receive the final target.
      ptr(m.io.fromBpu.s3FtqPtr, 0)
      prediction(0x1000, 0x4000, s3 = true)
      request(0)
      m.io.toBackend.wen.expect(true.B)
      m.clock.step()
      expectTarget(0x4000)
      stopPrediction()

      // A redirect wins over the simultaneous S3 write, leaving entry 0 unchanged.
      read.valid.poke(false.B)
      ifuRedirect(0)
      prediction(0x1000, 0x5000, s3 = true)
      request(0)
      m.io.toBackend.wen.expect(false.B)
      m.clock.step()
      expectTarget(0x4000)
      stopPrediction()

      // Redirect after entry 62 positions the full pointer at 63. Enqueueing 63
      // wraps it to flag=1,value=0; the response must distinguish both generations.
      read.valid.poke(false.B)
      ifuRedirect(62)
      m.clock.step() // Apply the registered redirect before checking the new BPU pointer.
      m.io.toBpu.bpuPtr.value.expect(63.U)
      m.io.toBpu.bpuPtr.flag.expect(false.B)
      prediction(0x1f00, 0x6000)
      request(63)
      m.io.fromBpu.prediction.ready.expect(true.B)
      // Free one entry so the post-wrap enqueue at 0 is legal.
      ptr(m.io.fromBackend.commit.bits, 0)
      m.io.fromBackend.commit.valid.poke(true.B)
      m.clock.step()
      expectTarget(0x6000)
      m.io.fromBackend.commit.valid.poke(false.B)
      m.io.toBpu.bpuPtr.value.expect(0.U)
      m.io.toBpu.bpuPtr.flag.expect(true.B)

      prediction(0x1f04, 0x7000)
      request(0, flag = false) // stale generation: no same-edge bypass
      m.io.fromBpu.prediction.ready.expect(true.B)
      m.clock.step()
      expectTarget(0x4000)
      stopPrediction()
      request(0, flag = true)
      m.clock.step()
      expectTarget(0x7000)
    }
  }
}
