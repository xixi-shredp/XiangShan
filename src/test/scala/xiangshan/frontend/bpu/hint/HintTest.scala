package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import java.nio.file.{Files, Paths}
import org.json4s.{DefaultFormats, JNull, JValue}
import org.json4s.jackson.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import xiangshan.{XSCoreParamsKey, XSTileKey}

class HintTest extends AnyFlatSpec with ChiselSim {
  private val config = new DefaultConfig
  implicit val params: org.chipsalliance.cde.config.Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
  }

  // Keep this large table model in small C++ units while retaining the
  // ChiselSim/ScalaTest assertions; this affects test compilation only.
  override implicit def backendSettingsModifications: svsim.BackendSettingsModifications =
    new svsim.BackendSettingsModifications {
      override def apply(settings: svsim.Backend.Settings): svsim.Backend.Settings =
        settings match {
          case v: svsim.verilator.Backend.CompilationSettings =>
            v.withOutputSplit(Some(20000)).withOutputSplitCFuncs(Some(2000))
          case other => other
        }
    }

  behavior of "H2P Hint v3"
  it should "admit a fifth no-LHR write while preserving LHR and metadata admission limits" in {
    simulate(new Hint) { m =>
      def ptr(port: xiangshan.frontend.ftq.FtqPtr, id: Int): Unit = {
        port.flag.poke(false.B)
        port.value.poke(id.U)
      }
      def input(pc: Int, id: Int, taken: Boolean, target: Int): Unit = {
        m.io.s1StartPc.poke(pc.U)
        m.io.s1CfiPc.poke(pc.U)
        m.io.s3CfiPc.poke(pc.U)
        ptr(m.io.s1Ptr, id)
        ptr(m.io.s3Ptr, id)
        for (pred <- Seq(m.io.s1Prediction, m.io.s3Prediction)) {
          pred.taken.poke(taken.B)
          pred.target.addr.poke((target >> 1).U)
          pred.attribute.branchType.poke(3.U)
          pred.attribute.rasAction.poke(0.U)
          pred.cfiPosition.poke(0.U)
        }
      }
      def load(kind: Int, index: Int, data: BigInt): Unit = {
        m.io.load.bits.kind.poke(kind.U)
        m.io.load.bits.index.poke(index.U)
        m.io.load.bits.data.poke(data.U)
        m.io.load.valid.poke(true.B)
        m.io.load.ready.expect(true.B)
        m.clock.step()
        m.io.load.valid.poke(false.B)
      }
      def fire(pc: Int, id: Int, target: Int): Unit = {
        input(pc, id, taken = true, target = target)
        m.io.canAccept.expect(true.B)
        m.io.s1Fire.poke(true.B)
        m.clock.step()
        m.io.s1Fire.poke(false.B)
      }

      m.io.load.valid.poke(false.B)
      m.io.enabled.poke(false.B)
      m.io.profile.poke(false.B)
      m.io.measureActive.poke(false.B)
      m.io.s3UsedHint.poke(false.B)
      m.io.s3BaselineTarget.poke(0.U)
      m.io.s1Fire.poke(false.B)
      m.io.s2Fire.poke(false.B)
      m.io.s3Fire.poke(false.B)
      m.io.s3Override.poke(false.B)
      m.io.redirect.valid.poke(false.B)
      m.io.commit.valid.poke(false.B)
      for (r <- m.io.resolve) r.valid.poke(false.B)
      input(0x1200, 0, taken = false, target = 0x3000)
      m.clock.step(4096)
      m.io.enabled.poke(true.B)

      // Mask 0 ignores LHR for lookup, but an admitted H2P still writes its ring.
      load(1, 0, (BigInt(1) << 51) | BigInt(0x1000 >> 1))
      for (id <- 0 until 4) fire(0x1000, id, 0x3000 + 2 * id)
      fire(0x1000, 4, 0x3002) // fifth live reservation must fire
      input(0x1200, 5, taken = false, target = 0x3000)
      m.io.s1Fire.poke(true.B) // capture the post-write head without another CAM reservation
      m.clock.step()
      m.io.s1Fire.poke(false.B)
      m.io.s2Fire.poke(true.B)
      m.clock.step()
      m.io.s2Fire.poke(false.B)
      m.io.s3Snapshot.lhrHead(0).expect(5.U)

      // A mask that reads LHR still stops after four live writes, with free metadata.
      load(1, 1, (BigInt(1) << 51) | (BigInt(2) << 47) | BigInt(0x1100 >> 1))
      for (id <- 6 until 10) fire(0x1100, id, 0x3002)
      input(0x1100, 10, taken = true, target = 0x3002)
      m.io.canAccept.expect(false.B)

      // The bypass does not override the independent 16-entry metadata limit.
      for (id <- 10 until 17) fire(0x1000, id, 0x3002)
      input(0x1000, 17, taken = true, target = 0x3002)
      m.io.canAccept.expect(false.B)
    }
  }

  it should "stall at four live writes per LHR and restore the selected history on redirect" in {
    implicit val formats: DefaultFormats.type = DefaultFormats
    val goldenPath = Iterator.iterate(Paths.get("").toAbsolutePath)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve("manifest/hint-v3-vectors.json"))
      .find(path => Files.isRegularFile(path))
      .getOrElse(throw new IllegalStateException("manifest/hint-v3-vectors.json not found"))
    val golden = parse(Files.readString(goldenPath))
    def goldenHex(v: JValue, name: String): BigInt =
      BigInt((v \ name).extract[String].stripPrefix("0x"), 16)
    simulate(new Hint) { m =>
      def ptr(port: xiangshan.frontend.ftq.FtqPtr, value: Int): Unit = {
        port.flag.poke(false.B)
        port.value.poke(value.U)
      }
      def prediction(taken: Boolean, target: Int): Unit = {
        m.io.s1Prediction.taken.poke(taken.B)
        m.io.s1Prediction.attribute.branchType.poke(3.U)
        m.io.s1Prediction.attribute.rasAction.poke(0.U)
        m.io.s1Prediction.target.addr.poke((target >> 1).U)
        m.io.s1Prediction.cfiPosition.poke(0.U)
        m.io.s3Prediction.taken.poke(taken.B)
        m.io.s3Prediction.attribute.branchType.poke(3.U)
        m.io.s3Prediction.attribute.rasAction.poke(0.U)
        m.io.s3Prediction.target.addr.poke((target >> 1).U)
        m.io.s3Prediction.cfiPosition.poke(0.U)
      }
      def load(kind: Int, index: Int, data: BigInt): Unit = {
        m.io.load.bits.kind.poke(kind.U)
        m.io.load.bits.index.poke(index.U)
        m.io.load.bits.data.poke(data.U)
        m.io.load.valid.poke(true.B)
        m.io.load.ready.expect(true.B)
        m.clock.step()
        m.io.load.valid.poke(false.B)
      }
      def input(pc: Int, id: Int, taken: Boolean, target: Int): Unit = {
        m.io.s1StartPc.poke(pc.U)
        m.io.s1CfiPc.poke(pc.U)
        m.io.s3CfiPc.poke(pc.U)
        ptr(m.io.s1Ptr, id)
        ptr(m.io.s3Ptr, id)
        prediction(taken, target)
      }

      m.io.load.valid.poke(false.B)
      m.io.enabled.poke(false.B)
      m.io.profile.poke(false.B)
      m.io.measureActive.poke(false.B)
      m.io.s3UsedHint.poke(false.B)
      m.io.s3BaselineTarget.poke(0.U)
      m.io.s1Fire.poke(false.B)
      m.io.s2Fire.poke(false.B)
      m.io.s3Fire.poke(false.B)
      m.io.s3Override.poke(false.B)
      m.io.redirect.valid.poke(false.B)
      m.io.commit.valid.poke(false.B)
      for (r <- m.io.resolve) r.valid.poke(false.B)
      input(0x1000, 0, true, 0x3002)
      m.io.canAccept.expect(true.B) // disabled baseline bypasses Context clearing
      m.clock.step(4096)
      m.io.enabled.poke(true.B)
      m.io.canAccept.expect(true.B)

      // LHR-only mask. The CAM address is the first CFI in the fetch window.
      load(1, 0, (BigInt(1) << 51) | (BigInt(2) << 47) | BigInt(0x1000 >> 1))
      load(3, 0, BigInt(0x2000 >> 1))
      // After redirect, LHR low8=2. key=0x800 XOR rol28(2,3)=0x810.
      load(2, 32, (BigInt(1) << 24) | (BigInt(3) << 22) | BigInt(1))

      m.io.canAccept.expect(true.B)
      m.io.s1Fire.poke(true.B)
      m.clock.step()
      m.io.s1Fire.poke(false.B)
      m.io.s2Fire.poke(true.B)
      m.clock.step()
      m.io.s2Fire.poke(false.B)
      val saved = m.io.s3Snapshot
      val ithHead = saved.ithHead.peek().litValue
      val callHead = saved.callHead.peek().litValue
      val retHead = saved.retHead.peek().litValue
      val ithWrites = saved.ithWrites.peek().litValue
      val callWrites = saved.callWrites.peek().litValue
      val retWrites = saved.retWrites.peek().litValue
      val callLow12 = saved.callLow12.peek().litValue
      val retLow12 = saved.retLow12.peek().litValue
      val lhrHeads = saved.lhrHead.map(_.peek().litValue)
      assert(ithHead == 0 && lhrHeads(0) == 0)

      for (id <- 1 to 3) {
        input(0x1000, id, true, 0x3002)
        m.io.canAccept.expect(true.B)
        m.io.s1Fire.poke(true.B)
        m.clock.step()
        m.io.s1Fire.poke(false.B)
      }
      m.io.canAccept.expect(false.B)

      m.io.redirect.bits.meta.hint.ithHead.poke(ithHead.U)
      m.io.redirect.bits.meta.hint.callHead.poke(callHead.U)
      m.io.redirect.bits.meta.hint.retHead.poke(retHead.U)
      m.io.redirect.bits.meta.hint.ithWrites.poke(ithWrites.U)
      m.io.redirect.bits.meta.hint.callWrites.poke(callWrites.U)
      m.io.redirect.bits.meta.hint.retWrites.poke(retWrites.U)
      m.io.redirect.bits.meta.hint.callLow12.poke(callLow12.U)
      m.io.redirect.bits.meta.hint.retLow12.poke(retLow12.U)
      m.io.redirect.bits.meta.hint.lhrHead.zip(lhrHeads).foreach { case (port, value) => port.poke(value.U) }
      m.io.redirect.bits.cfiPc.addr.poke((0x1000 >> 1).U)
      m.io.redirect.bits.target.addr.poke((0x3004 >> 1).U)
      m.io.redirect.bits.taken.poke(true.B)
      m.io.redirect.bits.attribute.branchType.poke(3.U)
      m.io.redirect.bits.attribute.rasAction.poke(0.U)
      ptr(m.io.redirect.bits.newFtqIdx, 1)
      m.io.redirect.bits.reapply.poke(true.B)
      m.io.redirect.valid.poke(true.B)
      m.clock.step()
      m.io.redirect.valid.poke(false.B)
      // The confidence rollback port holds fetch for one cycle even when its log is empty.
      m.clock.step()
      m.io.canAccept.expect(true.B)

      // Probe the restored context without advancing history.
      input(0x1000, 4, false, 0x3002)
      m.io.s1Fire.poke(true.B)
      m.clock.step()
      m.io.s1Fire.poke(false.B)
      m.io.s2Fire.poke(true.B)
      m.clock.step()
      m.io.s2Fire.poke(false.B)
      m.io.s3Lookup.contextHit.expect(true.B)
      m.io.s3Lookup.usable.expect(true.B)
      m.io.s3Lookup.target.expect(0x2000.U)

      // Four distinct CAM rings can occupy all 16 records; the next candidate stalls.
      load(0, 0, 0)
      m.clock.step(4096)
      for (i <- 0 until 4) {
        val pc = 0x1000 + i * 0x40
        load(1, i, (BigInt(1) << 51) | BigInt(pc >> 1))
      }
      for (i <- 0 until 16) {
        val pc = 0x1000 + (i / 4) * 0x40
        input(pc, i, true, 0x3002)
        m.io.canAccept.expect(true.B)
        m.io.s1Fire.poke(true.B)
        m.clock.step()
        m.io.s1Fire.poke(false.B)
      }
      input(0x1000, 16, true, 0x3002)
      m.io.canAccept.expect(false.B)

      // A confidence-zero tag hit retains its shadow target for scoring.
      // An applied younger update must be undone after a redirect.
      load(0, 0, 0)
      m.clock.step(4096)
      load(1, 0, (BigInt(1) << 51) | BigInt(0x1000 >> 1))
      load(3, 0, BigInt(0x2000 >> 1))
      load(2, 0, (BigInt(1) << 24) | (BigInt(1) << 22) | BigInt(1))

      def confirm(id: Int, usable: Boolean): Unit = {
        input(0x1000, id, false, 0x3000)
        m.io.s1Fire.poke(true.B)
        m.clock.step()
        m.io.s1Fire.poke(false.B)
        m.io.s2Fire.poke(true.B)
        m.clock.step()
        m.io.s2Fire.poke(false.B)
        m.io.s3Lookup.contextHit.expect(true.B)
        m.io.s3Lookup.usable.expect(usable.B)
        m.io.s3Lookup.target.expect(0x2000.U)
        m.io.s3Prediction.taken.poke(true.B)
        m.io.s3Fire.poke(true.B)
        m.clock.step()
        m.io.s3Fire.poke(false.B)
      }
      def resolve(id: Int, target: Int): Unit = {
        val r = m.io.resolve(0)
        r.valid.poke(true.B)
        ptr(r.bits.ftqPtr, id)
        r.bits.cfiPc.poke(0x1000.U)
        r.bits.target.poke(target.U)
        r.bits.eligible.poke(true.B)
        m.io.measureActive.poke(true.B)
        m.clock.step()
        r.valid.poke(false.B)
        m.io.measureActive.poke(false.B)
        m.clock.step(2) // FTQ-order enqueue, then single-port table write.
      }

      confirm(0, usable = true)
      resolve(0, 0x3000) // wrong shadow: confidence 1 -> 0
      confirm(1, usable = false)
      resolve(1, 0x2000) // correct shadow: confidence 0 -> 1

      // A non-H2P redirect to entry 0 squashes the applied update from entry 1.
      m.io.redirect.bits.cfiPc.addr.poke((0x1400 >> 1).U)
      m.io.redirect.bits.attribute.branchType.poke(1.U)
      m.io.redirect.bits.reapply.poke(false.B)
      m.io.redirect.valid.poke(true.B)
      m.clock.step()
      m.io.redirect.valid.poke(false.B)
      m.clock.step(3)
      confirm(2, usable = false)

      // A later resolved FTQ entry must wait for an older unresolved entry.
      // Starting at confidence zero, younger correct then older wrong would
      // finish at zero if the two scores were applied in resolve order.
      load(0, 0, 0)
      m.clock.step(4096)
      load(1, 0, (BigInt(1) << 51) | BigInt(0x1000 >> 1))
      load(3, 0, BigInt(0x2000 >> 1))
      load(2, 0, (BigInt(1) << 24) | BigInt(1))
      confirm(0, usable = false)
      confirm(1, usable = false)
      resolve(1, 0x2000)
      confirm(2, usable = false) // the younger score is still blocked
      resolve(0, 0x3000)
      m.clock.step(4)
      confirm(3, usable = true) // old wrong 0->0, then young correct 0->1

      // The runtime measure gate also covers warmup and restore: a wrong
      // branch resolved outside the measured interval cannot reduce confidence.
      load(0, 0, 0)
      m.clock.step(4096)
      load(1, 0, (BigInt(1) << 51) | BigInt(0x1000 >> 1))
      load(3, 0, BigInt(0x2000 >> 1))
      load(2, 0, (BigInt(1) << 24) | (BigInt(1) << 22) | BigInt(1))
      confirm(0, usable = true)
      val warmup = m.io.resolve(0)
      warmup.valid.poke(true.B)
      ptr(warmup.bits.ftqPtr, 0)
      warmup.bits.cfiPc.poke(0x1000.U)
      warmup.bits.target.poke(0x3000.U)
      warmup.bits.eligible.poke(true.B)
      m.io.measureActive.poke(false.B)
      m.clock.step()
      warmup.valid.poke(false.B)
      m.clock.step(2)
      confirm(1, usable = true)

      // A Context loader write yields to an S1 read of the same set.
      // The in-flight lookup sees the old row; a later lookup sees the write.
      input(0x1000, 2, false, 0x3000)
      m.io.load.bits.kind.poke(2.U)
      m.io.load.bits.index.poke(0.U)
      m.io.load.bits.data.poke(((BigInt(1) << 24) | BigInt(1)).U)
      m.io.load.valid.poke(true.B)
      m.io.s1Fire.poke(true.B)
      m.io.load.ready.expect(false.B)
      m.clock.step()
      m.io.s1Fire.poke(false.B)
      m.io.s2Fire.poke(true.B)
      m.io.load.ready.expect(true.B)
      m.clock.step()
      m.io.s2Fire.poke(false.B)
      m.io.load.valid.poke(false.B)
      m.io.s3Lookup.contextHit.expect(true.B)
      m.io.s3Lookup.usable.expect(true.B)
      confirm(3, usable = false)

      // Repeated predicted candidates that do not become eligible resolves
      // must release their records at commit, with no confidence scoring.
      load(0, 0, 0)
      m.clock.step(4096)
      load(1, 0, (BigInt(1) << 51) | BigInt(0x1000 >> 1))
      load(3, 0, BigInt(0x2000 >> 1))
      load(2, 0, (BigInt(1) << 24) | (BigInt(1) << 22) | BigInt(1))
      for (id <- 0 until 20) {
        confirm(id, usable = true)
        if (id % 2 == 0) {
          val ineligible = m.io.resolve(0)
          ineligible.valid.poke(true.B)
          ptr(ineligible.bits.ftqPtr, id)
          ineligible.bits.cfiPc.poke(0x1000.U)
          ineligible.bits.target.poke(0x3000.U)
          ineligible.bits.eligible.poke(false.B)
          m.io.measureActive.poke(true.B)
          m.clock.step()
          ineligible.valid.poke(false.B)
          m.io.measureActive.poke(false.B)
        }
        m.io.commit.valid.poke(true.B)
        ptr(m.io.commit.bits, id)
        m.clock.step()
        m.io.commit.valid.poke(false.B)
        m.clock.step(3)
      }
      input(0x1000, 20, false, 0x3000)
      m.io.canAccept.expect(true.B)

      // Independent hash points for ITH, Call, Ret, and a simultaneous
      // Call+Ret update. A matching Context tag checks the upper key bits.
      load(0, 0, 0)
      m.clock.step(4096)
      load(3, 0, BigInt(0x2000 >> 1))
      def probe(mask: Int, set: Int, tag: Int): Unit = {
        load(1, 0, (BigInt(1) << 51) | (BigInt(mask) << 47) | BigInt(0x1000 >> 1))
        load(2, set * 2, (BigInt(1) << 24) | (BigInt(3) << 22) | BigInt(tag))
        input(0x1000, 0, false, 0x3000)
        m.io.s1Fire.poke(true.B)
        m.clock.step()
        m.io.s1Fire.poke(false.B)
        m.io.s2Fire.poke(true.B)
        m.clock.step()
        m.io.s2Fire.poke(false.B)
        m.io.s3Lookup.set.expect(set.U)
        m.io.s3Lookup.contextHit.expect(true.B)
        m.io.s3Lookup.usable.expect(true.B)
        m.io.s3Fire.poke(true.B)
        m.clock.step()
        m.io.s3Fire.poke(false.B)
      }
      def historyEvent(pc: Int, target: Int, branchType: Int, rasAction: Int): Unit = {
        input(pc, 1, true, target)
        m.io.s1Prediction.attribute.branchType.poke(branchType.U)
        m.io.s1Prediction.attribute.rasAction.poke(rasAction.U)
        m.io.s1Fire.poke(true.B)
        m.clock.step()
        m.io.s1Fire.poke(false.B)
      }
      probe(mask = 1, set = 0, tag = 1) // PC word 0x800
      historyEvent(0x1100, 0x2002, branchType = 3, rasAction = 0)
      probe(mask = 1, set = 4, tag = 1) // ITH cell 1, rotated by 2
      historyEvent(0x1200, 0x2000, branchType = 2, rasAction = 2)
      probe(mask = 4, set = 0, tag = 19) // Call low14=0x900
      historyEvent(0x1600, 0x2000, branchType = 3, rasAction = 1)
      probe(mask = 8, set = 0, tag = 45) // Ret low14=0xb00
      historyEvent(0x1800, 0x2000, branchType = 3, rasAction = 3)
      probe(mask = 4, set = 0, tag = 81) // Call low14=0x2800
      probe(mask = 8, set = 0, tag = 129) // Ret low14=0x2000

      // Sorted CAM entries select the earliest PC still inside the window.
      load(1, 0, (BigInt(1) << 51) | BigInt(0x1000 >> 1))
      load(1, 1, (BigInt(1) << 51) | BigInt(0x1010 >> 1))
      input(0x1008, 2, false, 0x3000)
      m.io.s1Fire.poke(true.B)
      m.clock.step()
      m.io.s1Fire.poke(false.B)
      m.io.s2Fire.poke(true.B)
      m.clock.step()
      m.io.s2Fire.poke(false.B)
      m.io.s3Lookup.pc.expect(0x1010.U)

      // Drive the packed image words and selected vectors from the shared
      // manifest, rather than copying their values into this test.
      val sample = golden \ "sample_image"
      load(0, 0, 0)
      m.clock.step(4096)
      load(1, 0, goldenHex(sample, "cam_word_0"))
      load(2, 0, goldenHex(sample, "context_word_0"))
      load(3, 0, goldenHex(sample, "slot_word_0"))
      input(0x1000, 0, false, 0x3000)
      m.io.s1Fire.poke(true.B)
      m.clock.step()
      m.io.s1Fire.poke(false.B)
      m.io.s2Fire.poke(true.B)
      m.clock.step()
      m.io.s2Fire.poke(false.B)
      m.io.s3Lookup.contextHit.expect(true.B)
      m.io.s3Lookup.target.expect(0x2000.U)

      for (v <- (golden \ "hash").children if (v \ "mask").extract[Int] == 0) {
        val pc = goldenHex(v, "pc")
        val set = (v \ "set").extract[Int]
        val tag = (v \ "tag").extract[Int]
        load(0, 0, 0)
        m.clock.step(4096)
        load(1, 0, (BigInt(1) << 51) | ((pc >> 1) & ((BigInt(1) << 47) - 1)))
        load(2, set * 2, (BigInt(1) << 24) | (BigInt(3) << 22) | BigInt(tag))
        load(3, 0, BigInt(0x2000 >> 1))
        m.io.s1StartPc.poke(pc.U)
        m.io.s1CfiPc.poke(pc.U)
        prediction(false, 0x3000)
        m.io.s1Fire.poke(true.B)
        m.clock.step()
        m.io.s1Fire.poke(false.B)
        m.io.s2Fire.poke(true.B)
        m.clock.step()
        m.io.s2Fire.poke(false.B)
        m.io.s3Lookup.pc.expect(pc.U)
        m.io.s3Lookup.set.expect(set.U)
        m.io.s3Lookup.contextHit.expect(true.B)
      }

      for (v <- (golden \ "fold14").children) {
        val pc = goldenHex(v, "pc")
        val folded = goldenHex(v, "result")
        val key = BigInt(0x800) ^ (folded << 4)
        load(0, 0, 0)
        m.clock.step(4096)
        load(3, 0, BigInt(0x2000 >> 1))
        historyEvent(pc.toInt, 0x2000, branchType = 2, rasAction = 2)
        probe(mask = 4, set = (key & 0x7ff).toInt, tag = (key >> 11).toInt)
      }

      for (v <- (golden \ "cam_windows").children) {
        val start = goldenHex(v, "start_pc")
        val pcs = (v \ "pcs").children.map(_.extract[String]).map(s => BigInt(s.stripPrefix("0x"), 16))
        load(0, 0, 0)
        m.clock.step(4096)
        for ((pc, i) <- pcs.zipWithIndex) {
          load(1, i, (BigInt(1) << 51) | ((pc >> 1) & ((BigInt(1) << 47) - 1)))
        }
        m.io.s1StartPc.poke(start.U)
        m.io.s1CfiPc.poke(start.U)
        prediction(false, 0x3000)
        m.io.s1Fire.poke(true.B)
        m.clock.step()
        m.io.s1Fire.poke(false.B)
        m.io.s2Fire.poke(true.B)
        m.clock.step()
        m.io.s2Fire.poke(false.B)
        val selected = v \ "selected"
        if (selected == JNull) m.io.s3Lookup.candidate.expect(false.B)
        else m.io.s3Lookup.pc.expect(goldenHex(v, "selected").U)
      }

      // Build each nonzero shared hash history through ordinary speculative
      // branch events. The expected set and tag come directly from the JSON.
      def seedCells(value: BigInt, count: Int)(emit: Int => Unit): Unit = {
        for (i <- (count - 1) to 0 by -1) emit(((value >> (2 * i)) & 3).toInt)
      }
      def queryVector(v: JValue, id: Int): Unit = {
        val pc = goldenHex(v, "pc")
        val mask = (v \ "mask").extract[Int]
        val set = (v \ "set").extract[Int]
        val tag = (v \ "tag").extract[Int]
        load(1, 0, (BigInt(1) << 51) | (BigInt(mask) << 47) |
          ((pc >> 1) & ((BigInt(1) << 47) - 1)))
        load(2, set * 2, (BigInt(1) << 24) | (BigInt(3) << 22) | BigInt(tag))
        m.io.s1StartPc.poke(pc.U)
        m.io.s1CfiPc.poke(pc.U)
        m.io.s3CfiPc.poke(pc.U)
        ptr(m.io.s1Ptr, id)
        ptr(m.io.s3Ptr, id)
        prediction(false, 0x2000)
        m.io.s1Fire.poke(true.B)
        m.clock.step()
        m.io.s1Fire.poke(false.B)
        m.io.s2Fire.poke(true.B)
        m.clock.step()
        m.io.s2Fire.poke(false.B)
        m.io.s3Lookup.candidate.expect(true.B)
        m.io.s3Lookup.pc.expect(pc.U)
        m.io.s3Lookup.set.expect(set.U)
        m.io.s3Lookup.contextHit.expect(true.B)
        m.io.s3Fire.poke(true.B)
        m.clock.step() // no taken H2P CFI, so release this read-only reservation
        m.io.s3Fire.poke(false.B)
      }
      val nonzeroHashes = (golden \ "hash").children.filter(v => (v \ "mask").extract[Int] == 15)
      for (v <- nonzeroHashes) {
        val pc = goldenHex(v, "pc")
        load(0, 0, 0)
        m.clock.step(4096)
        load(1, 0, (BigInt(1) << 51) | (BigInt(15) << 47) |
          ((pc >> 1) & ((BigInt(1) << 47) - 1)))
        load(3, 0, BigInt(0x2000 >> 1))
        var id = 0
        seedCells(goldenHex(v, "lhr_low8"), 4) { cell =>
          m.io.s1StartPc.poke(pc.U)
          m.io.s1CfiPc.poke(pc.U)
          m.io.s3CfiPc.poke(pc.U)
          ptr(m.io.s1Ptr, id)
          ptr(m.io.s3Ptr, id)
          prediction(true, cell << 1)
          m.io.s1Fire.poke(true.B)
          m.clock.step()
          m.io.s1Fire.poke(false.B)
          m.io.s2Fire.poke(true.B)
          m.clock.step()
          m.io.s2Fire.poke(false.B)
          m.io.s3Fire.poke(true.B)
          m.clock.step()
          m.io.s3Fire.poke(false.B)
          m.io.commit.valid.poke(true.B)
          ptr(m.io.commit.bits, id)
          m.clock.step()
          m.io.commit.valid.poke(false.B)
          m.clock.step(3)
          id += 1
        }
        seedCells(goldenHex(v, "ith_low64"), 32) { cell =>
          historyEvent(0x2000, cell << 1, branchType = 3, rasAction = 0)
        }
        seedCells(goldenHex(v, "call_low64"), 32) { cell =>
          historyEvent(cell << 1, 0, branchType = 2, rasAction = 2)
        }
        seedCells(goldenHex(v, "ret_low64"), 32) { cell =>
          historyEvent(cell << 1, 0, branchType = 3, rasAction = 1)
        }
        queryVector(v, id)
        // The mask-five vector uses the same PC and histories as the first
        // mask-fifteen vector, so it tests mask exclusion without reseeding.
        if (pc == BigInt("123456789a", 16)) {
          val maskFive = (golden \ "hash").children.find(x =>
            (x \ "mask").extract[Int] == 5 && goldenHex(x, "pc") == pc).get
          queryVector(maskFive, id + 1)
        }
      }
    }
  }

  it should "observe a fifth speculative LHR write without stalling and restore the four-target window" in {
    simulate(new Hint) { m =>
      def ptr(port: xiangshan.frontend.ftq.FtqPtr, value: Int): Unit = {
        port.flag.poke(false.B)
        port.value.poke(value.U)
      }
      def input(id: Int, taken: Boolean, target: Int): Unit = {
        m.io.s1StartPc.poke(0x1000.U)
        m.io.s1CfiPc.poke(0x1000.U)
        m.io.s3CfiPc.poke(0x1000.U)
        ptr(m.io.s1Ptr, id)
        ptr(m.io.s3Ptr, id)
        for (pred <- Seq(m.io.s1Prediction, m.io.s3Prediction)) {
          pred.taken.poke(taken.B)
          pred.target.addr.poke((target >> 1).U)
          pred.attribute.branchType.poke(3.U)
          pred.attribute.rasAction.poke(0.U)
          pred.cfiPosition.poke(0.U)
        }
      }
      def load(kind: Int, index: Int, data: BigInt): Unit = {
        m.io.load.bits.kind.poke(kind.U)
        m.io.load.bits.index.poke(index.U)
        m.io.load.bits.data.poke(data.U)
        m.io.load.valid.poke(true.B)
        m.io.load.ready.expect(true.B)
        m.clock.step()
        m.io.load.valid.poke(false.B)
      }
      m.io.load.valid.poke(false.B)
      m.io.enabled.poke(false.B)
      m.io.profile.poke(true.B)
      m.io.measureActive.poke(false.B)
      m.io.s3UsedHint.poke(false.B)
      m.io.s3BaselineTarget.poke(0.U)
      m.io.s1Fire.poke(false.B)
      m.io.s2Fire.poke(false.B)
      m.io.s3Fire.poke(false.B)
      m.io.s3Override.poke(false.B)
      m.io.redirect.valid.poke(false.B)
      m.io.commit.valid.poke(false.B)
      for (r <- m.io.resolve) r.valid.poke(false.B)
      input(0, true, 0x3002)
      m.clock.step(4096)
      load(1, 0, (BigInt(1) << 51) | (BigInt(2) << 47) | BigInt(0x1000 >> 1))
      load(3, 0, BigInt(0x2000 >> 1))
      load(2, 32, (BigInt(1) << 24) | (BigInt(3) << 22) | BigInt(1))
      m.io.s1Fire.poke(true.B)
      m.clock.step()
      m.io.s1Fire.poke(false.B)
      m.io.s2Fire.poke(true.B)
      m.clock.step()
      m.io.s2Fire.poke(false.B)
      val saved = m.io.s3Snapshot
      val ithHead = saved.ithHead.peek().litValue
      val callHead = saved.callHead.peek().litValue
      val retHead = saved.retHead.peek().litValue
      val ithWrites = saved.ithWrites.peek().litValue
      val callWrites = saved.callWrites.peek().litValue
      val retWrites = saved.retWrites.peek().litValue
      val callLow12 = saved.callLow12.peek().litValue
      val retLow12 = saved.retLow12.peek().litValue
      val lhrHeads = saved.lhrHead.map(_.peek().litValue)
      for (id <- 1 to 4) {
        input(id, true, 0x3002)
        m.io.canAccept.expect(true.B)
        m.io.s1Fire.poke(true.B)
        m.clock.step()
        m.io.s1Fire.poke(false.B)
      }
      m.io.canAccept.expect(true.B)
      m.io.redirect.bits.meta.hint.ithHead.poke(ithHead.U)
      m.io.redirect.bits.meta.hint.callHead.poke(callHead.U)
      m.io.redirect.bits.meta.hint.retHead.poke(retHead.U)
      m.io.redirect.bits.meta.hint.ithWrites.poke(ithWrites.U)
      m.io.redirect.bits.meta.hint.callWrites.poke(callWrites.U)
      m.io.redirect.bits.meta.hint.retWrites.poke(retWrites.U)
      m.io.redirect.bits.meta.hint.callLow12.poke(callLow12.U)
      m.io.redirect.bits.meta.hint.retLow12.poke(retLow12.U)
      m.io.redirect.bits.meta.hint.lhrHead.zip(lhrHeads).foreach {
        case (port, value) => port.poke(value.U)
      }
      m.io.redirect.bits.cfiPc.addr.poke((0x1000 >> 1).U)
      m.io.redirect.bits.target.addr.poke((0x3004 >> 1).U)
      m.io.redirect.bits.taken.poke(true.B)
      m.io.redirect.bits.attribute.branchType.poke(3.U)
      m.io.redirect.bits.attribute.rasAction.poke(0.U)
      ptr(m.io.redirect.bits.newFtqIdx, 1)
      m.io.redirect.bits.reapply.poke(true.B)
      m.io.redirect.valid.poke(true.B)
      m.clock.step()
      m.io.redirect.valid.poke(false.B)
      m.clock.step()
      m.io.profile.poke(false.B)
      m.io.enabled.poke(true.B)
      input(5, false, 0x3002)
      m.io.canAccept.expect(true.B)
      m.io.s1Fire.poke(true.B)
      m.clock.step()
      m.io.s1Fire.poke(false.B)
      m.io.s2Fire.poke(true.B)
      m.clock.step()
      m.io.s2Fire.poke(false.B)
      m.io.s3Lookup.contextHit.expect(true.B)
      m.io.s3Lookup.target.expect(0x2000.U)
    }
  }
}
