package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import top.BackendV2NoRVVNoLLCConfig
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{DebugOptionsKey, XSCoreParamsKey, XSTileKey}

/** Same finite production Hint under v1 and v2; no new DUT/test-only API. */
class HintIfuHistoryTest extends AnyFlatSpec with ChiselSim {
  private val config = new BackendV2NoRVVNoLLCConfig
  implicit val params: org.chipsalliance.cde.config.Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
    case DebugOptionsKey => config(DebugOptionsKey).copy(FPGAPlatform = true)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, true, false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.NORMAL, 0)
  }
  override implicit def commonSettingsModifications: svsim.CommonSettingsModifications =
    new svsim.CommonSettingsModifications {
      override def apply(s: svsim.CommonCompilationSettings): svsim.CommonCompilationSettings =
        s.copy(optimizationStyle = svsim.CommonCompilationSettings.OptimizationStyle.OptimizeForCompilationSpeed)
    }
  override implicit def backendSettingsModifications: svsim.BackendSettingsModifications =
    new svsim.BackendSettingsModifications {
      override def apply(s: svsim.Backend.Settings): svsim.Backend.Settings = s match {
        case v: svsim.verilator.Backend.CompilationSettings =>
          v.withOutputSplit(Some(20000)).withOutputSplitCFuncs(Some(2000))
        case x => x
      }
    }

  behavior of "Surviving IFU-identified Hint checkpoints"
  it should "retain real pre-query contexts and count every speculative LHR append" in {
    simulate(new HintSramTestTop(), firtoolOpts = Array("-O=release",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none")) { top =>
      val io = top.io.hint
      def initialize(d: Data): Unit = d match {
        case r: Record => r.elements.values.foreach(initialize)
        case v: Vec[_] => v.getElements.foreach(initialize)
        case b: Bool if DataMirror.directionOf(b) == ActualDirection.Input => b.poke(false.B)
        case u: UInt if DataMirror.directionOf(u) == ActualDirection.Input => u.poke(0.U)
        case _ => ()
      }
      def tick(n: Int = 1): Unit = for (_ <- 0 until n) {
        io.canAccept.expect(true.B); top.clock.step()
      }
      def ptr(p: xiangshan.frontend.ftq.FtqPtr, id: Int): Unit = {
        p.flag.poke(((id & 64) != 0).B); p.value.poke((id & 63).U)
      }
      def copySaved(a: Data, b: Data): Seq[() => Unit] = (a, b) match {
        case (x: Bool, y: Bool) => val v = x.peek().litToBoolean; Seq(() => y.poke(v.B))
        case (x: UInt, y: UInt) => val v = x.peek().litValue; Seq(() => y.poke(v.U))
        case (x: Record, y: Record) => x.elements.toSeq.flatMap { case (k, v) => copySaved(v, y.elements(k)) }
        case (x: Vec[_], y: Vec[_]) => x.getElements.zip(y.getElements).flatMap { case (u, v) => copySaved(u, v) }
        case _ => throw new IllegalArgumentException("snapshot field")
      }
      def load(kind: Int, index: Int, data: BigInt): Unit = {
        io.load.bits.kind.poke(kind.U); io.load.bits.index.poke(index.U); io.load.bits.data.poke(data.U)
        io.load.valid.poke(true.B); var n = 0
        while (!io.load.ready.peek().litToBoolean && n < 20000) { tick(); n += 1 }
        assert(n < 20000, "image load timeout"); tick(); io.load.valid.poke(false.B)
      }
      val predictedCfis = scala.collection.mutable.Map.empty[Int, Int]
      val predictedTaken = scala.collection.mutable.Map.empty[Int, Boolean]
      def reset(profile: Boolean, camCfi: Int = 0x1006): Unit = {
        predictedCfis.clear(); predictedTaken.clear()
        initialize(top.io); top.reset.poke(true.B); top.clock.step(3); top.reset.poke(false.B)
        var n = 0
        while (!io.tableReady.peek().litToBoolean && n < 20000) { tick(); n += 1 }
        assert(n < 20000, "production SRAM reset timeout")
        load(1, 0, (BigInt(((camCfi - 0x1000) / 2) & 31) << 52) | (BigInt(1) << 51) | (0x1000 >> 1))
        load(3, 0, 0x2000 >> 1)
        // F11(0x800)=1, tag=F17(0x800)^(F16(0x800)<<2)=0x2800.
        load(2, 2, (BigInt(1) << 24) | BigInt(0x2800)) // conf=0
        io.profile.poke(profile.B); io.enabled.poke((!profile).B)
      }
      case class Saved(id: Int, redirect: Seq[() => Unit], trace: Seq[() => Unit])
      def query(id: Int, recognized: Boolean = true, target: Int = 0x2006, cfiPc: Int = 0x1006): Saved = {
        predictedCfis(id) = cfiPc; predictedTaken(id) = recognized
        io.s1StartPc.poke(0x1000.U); io.s1CfiPc.poke(cfiPc.U); io.s3CfiPc.poke(cfiPc.U)
        ptr(io.s1Ptr, id); ptr(io.s3Ptr, id)
        for (p <- Seq(io.s1Prediction, io.s3Prediction)) {
          p.taken.poke(recognized.B); p.attribute.branchType.poke((if (recognized) 3 else 0).U)
          p.attribute.rasAction.poke(0.U); p.cfiPosition.poke((((cfiPc - 0x1000) / 2) & 31).U); p.target.addr.poke((target >> 1).U)
        }
        io.s3BaselineTarget.poke(0x1008.U)
        io.s1Fire.poke(true.B); tick(); io.s1Fire.poke(false.B)
        io.s2Fire.poke(true.B); tick(); io.s2Fire.poke(false.B)
        val saved = Saved(id, copySaved(io.s3Snapshot, io.redirect.bits.meta.hint),
          copySaved(io.s3Snapshot, io.traceCommit.bits.snapshot))
        io.s3UsedHint.poke(io.s3Lookup.usable.peek()); io.s3Fire.poke(true.B); tick()
        io.s3Fire.poke(false.B); io.s3UsedHint.poke(false.B); saved
      }
      def sample(s: Saved, valid: Boolean = true, label: String = "snapshot", actualPc: Int = 0x1004): Vector[BigInt] = {
        s.trace.foreach(_()); io.traceCommit.bits.ftqTag.poke(s.id.U)
        io.traceCommit.bits.queryStartPc.poke(0x1000.U); io.traceCommit.bits.actualPc.poke(actualPc.U)
        io.traceCommit.bits.actualTarget.poke(0x2006.U); io.traceCommit.bits.eligible.poke(true.B)
        io.traceCommit.bits.queryValid.poke(true.B); io.traceCommit.valid.poke(true.B)
        io.profileEvent.valid.expect(true.B)
        assert(io.profileEvent.bits.queryValid.peek().litToBoolean == valid, label)
        val data = Vector(io.profileEvent.bits.ith, io.profileEvent.bits.lhr,
          io.profileEvent.bits.call, io.profileEvent.bits.ret).map(_.peek().litValue)
        io.traceCommit.valid.poke(false.B); data
      }
      def setRedirect(s: Saved, target: Int, self: Boolean = false, start: Int = 0x1000, cfiPc: Int = 0x1006): Unit = {
        s.redirect.foreach(_()); ptr(io.redirect.bits.newFtqIdx, s.id + 1)
        io.redirect.bits.startPc.addr.poke((start >> 1).U)
        io.redirect.bits.cfiPc.addr.poke((cfiPc >> 1).U); io.redirect.bits.target.addr.poke((target >> 1).U)
        io.redirect.bits.taken.poke(true.B); io.redirect.bits.attribute.branchType.poke(3.U)
        io.redirect.bits.attribute.rasAction.poke(0.U); io.redirect.bits.reapply.poke(true.B)
        io.redirect.bits.flushItself.poke(self.B); io.redirect.valid.poke(true.B)
      }
      def redirect(s: Saved, target: Int, self: Boolean = false, start: Int = 0x1000, cfiPc: Int = 0x1006): Unit = {
        setRedirect(s, target, self, start, cfiPc); tick(); io.redirect.valid.poke(false.B)
      }
      def setResolve(id: Int, actual: Int, eligible: Boolean = true, cfiPc: Int = 0x1006): Unit = {
        val r = io.resolve(0); ptr(r.bits.ftqPtr, id); r.bits.cfiPc.poke(cfiPc.U)
        r.bits.target.poke(actual.U); r.bits.eligible.poke(eligible.B); r.bits.sourceBackend.poke(true.B); r.bits.fresh.poke(true.B)
        r.bits.baselineTarget.poke(0x1008.U); r.bits.baselineValid.poke((predictedTaken.getOrElse(id, false) && predictedCfis.get(id).contains(cfiPc)).B); r.valid.poke(true.B)
      }
      def hash(x: BigInt): BigInt = {
        def fold(w: Int): BigInt = { var v = x; var z = BigInt(0)
          while (v != 0) { z ^= v & ((BigInt(1) << w) - 1); v >>= w }; z }
        ((fold(17) ^ ((fold(16) << 2) & 0x1ffff)) << 11) | fold(11)
      }

      reset(profile = true)
      val a = query(0, target = 0x2002); val beforeA = sample(a)
      val b = query(1, recognized = false); val beforeB = sample(b)
      redirect(b, 0x1008) // IFU knows the jalr PC; target is still sequential.
      assert(sample(b, label = "IFU_B_CHECKPOINT_LOST: surviving decoded B lost its real S1 snapshot") == beforeB)
      assert(sample(a) == beforeA)
      io.stats.recordAllocate.expect(2.U); io.stats.confAccepted.expect(0.U)
      println("HINT_IFU_B_OWNERSHIP_AND_PREQUERY_CONTEXT_PASS")
      for (id <- 2 until 8) query(id)
      io.stats.recordAllocate.expect(8.U); io.stats.lhrWrites.expect(8.U)
      val rejected = query(8)
      io.stats.recordAllocate.expect(8.U); io.stats.lhrWrites.expect(8.U)
      sample(rejected, valid = false, label = "ninth same-CAM instance must not own a checkpoint")
      assert(sample(a) == beforeA, "IFU_UNTRACKED_APPEND: oldest real S1 history was overwritten")
      assert(sample(b) == beforeB)
      println("HINT_IFU_LHR8_NINTH_FALLBACK_OLDEST_SNAPSHOT_PASS")
      // Backend must still use B's original preHeads, replacing the IFU's
      // placeholder sample=0 with the real sample=3, rather than appending twice.
      setResolve(1, 0x2006); redirect(b, 0x2006); io.resolve(0).valid.poke(false.B)
      assert(sample(b) == beforeB)
      val afterBackend = query(2, target = 0x2000)
      assert(sample(afterBackend)(1) == hash(BigInt(7)), "backend did not restore B.preHeads then append actual target")
      println("HINT_IFU_THEN_BACKEND_TARGET_REPAIR_PASS")

      // A decoded IFU identity cannot turn an originally unrecognized/untaken
      // prediction into a qualified confidence query. Keep its checkpoint only.
      reset(profile = false); query(0, target = 0x2002); val trainB = query(1, recognized = false)
      io.measureActive.poke(true.B); redirect(trainB, 0x1008); tick(3)
      io.stats.confAccepted.expect(0.U); io.stats.confApply.expect(0.U)
      setResolve(1, 0x2006); tick(); io.resolve(0).valid.poke(false.B); tick(3)
      io.stats.confAccepted.expect(0.U); io.stats.confApply.expect(0.U)
      println("HINT_IFU_UNRECOGNIZED_IDENTITY_AND_RESOLVE_NO_TRAIN_PASS")

      // Concurrent identity/resolve and a later duplicate remain unqualified:
      // final BPU prediction had neither taken nor the indirect CFI type.
      reset(profile = false); query(0, target = 0x2002); val concurrent = query(1, recognized = false)
      io.measureActive.poke(true.B); setResolve(1, 0x2006); redirect(concurrent, 0x1008)
      io.resolve(0).valid.poke(false.B); tick(3)
      setResolve(1, 0x2006); tick(); io.resolve(0).valid.poke(false.B); tick(3)
      io.stats.confAccepted.expect(0.U); io.stats.confApply.expect(0.U)
      println("HINT_IFU_UNRECOGNIZED_CONCURRENT_AND_DUPLICATE_NO_TRAIN_PASS")

      for ((self, start) <- Seq((true, 0x1000), (false, 0x1100))) {
        reset(profile = true); val old = query(0, target = 0x2002); val oldContext = sample(old)
        val cancelled = query(1, recognized = false)
        redirect(cancelled, 0x1008, self, start)
        // Snapshot ownership belongs to the surviving FTQ window, not this
        // branch's trace eligibility. The real FTQ filters killed CFI commits.
        sample(cancelled, valid = true, label = "surviving window checkpoint must remain valid")
        io.stats.lhrWrites.expect(1.U); assert(sample(old) == oldContext)
      }
      reset(profile = true); val oldest = query(0, target = 0x2002)
      val younger = query(1, recognized = false)
      // Only the winning older redirect reaches Hint; a younger IFU resolve
      // cannot identify B through the backend-only resolve side channel.
      val ifu = io.resolve(0); setResolve(1, 0x1008); ifu.bits.sourceBackend.poke(false.B)
      redirect(oldest, 0x2002); ifu.valid.poke(false.B)
      sample(younger, valid = false, label = "older winning redirect must cancel younger B")
      println("HINT_IFU_WINDOW_SNAPSHOT_FLUSH_SELF_STARTPC_OLDER_WINNER_PASS")

      // Prediction type is speculative: a record confirmed by S3 can later
      // resolve as a direct branch or return. Such records must drain without
      // accepting training absent from an actual eligible result.
      for (eligible <- Seq(false, true)) {
        reset(profile = false); query(0)
        io.measureActive.poke(true.B)
        io.load.ready.expect(false.B) // The production metadata record is live.
        setResolve(0, 0x2006, eligible); tick(); io.resolve(0).valid.poke(false.B)
        tick(3)
        io.stats.confAccepted.expect((if (eligible) 1 else 0).U)
        io.stats.confApply.expect((if (eligible) 1 else 0).U)
        // A duplicate backend result must not enqueue another training event.
        setResolve(0, 0x2006, eligible); io.resolve(0).bits.fresh.poke(false.B); tick(); io.resolve(0).valid.poke(false.B)
        ptr(io.commit.bits, 0); io.commit.valid.poke(true.B)
        tick(); io.commit.valid.poke(false.B)
        tick(4)
        io.load.ready.expect(true.B) // Commit and normal drain released all metadata.
        io.stats.confAccepted.expect((if (eligible) 1 else 0).U)
        io.stats.confApply.expect((if (eligible) 1 else 0).U)
        println(if (eligible) "HINT_ELIGIBLE_ACTUAL_TRAIN_ONCE_RELEASE_PASS"
          else "HINT_INELIGIBLE_ACTUAL_NO_TRAIN_RELEASE_PASS")
      }

      // Hint's contract is already the internal final-halfword CFI coordinate,
      // not the architectural first-halfword PC. Exercise 32-bit instructions
      // at both alignments and a boundary where the high offset bit matters.
      // The FTQ adapter must supply architecturalPc + 2 for these jalr cases;
      // Coordinate arithmetic itself is checked by HintResolveCoordinatesTest;
      // this check exercises metadata lifetime for those FTQ coordinates.
      for (architecturalPc <- Seq(0x1000, 0x1002, 0x103e)) {
        val internalPc = architecturalPc + 2
        reset(profile = false, camCfi = internalPc); query(0, cfiPc = internalPc)
        setResolve(0, 0x2006, cfiPc = internalPc); tick()
        io.resolve(0).valid.poke(false.B)
        ptr(io.commit.bits, 0); io.commit.valid.poke(true.B)
        tick(); io.commit.valid.poke(false.B); tick(3)
        io.load.ready.expect(true.B)
      }
      println("HINT_32BIT_CFI_METADATA_RELEASE_PASS")

      // IFU can correct not just an unrecognized CFI, but an already confirmed
      // later predicted indirect in the same CAM window. Preserve the original
      // S1 owner and its exact pre-query components through that correction.
      reset(profile = true); query(0, target = 0x2002)
      val predictedLater = query(1, cfiPc = 0x101a)
      val originalContext = sample(predictedLater, actualPc = 0x1010)
      redirect(predictedLater, 0x1008, cfiPc = 0x1010)
      assert(sample(predictedLater, actualPc = 0x1010,
        label = "IFU_EARLIER_CFI_CHECKPOINT_LOST: confirmed later CFI must retain its S1 checkpoint") == originalContext)
      io.stats.recordAllocate.expect(2.U)
      io.stats.confAccepted.expect(0.U)
      setResolve(1, 0x2006, cfiPc = 0x1010); tick(); io.resolve(0).valid.poke(false.B)
      assert(sample(predictedLater, actualPc = 0x1010) == originalContext)
      println("HINT_IFU_EARLIER_CFI_PROFILE_CHECKPOINT_PASS")

      for (oldReady <- Seq(false, true); concurrent <- Seq(false, true)) {
        reset(profile = false, camCfi = 0x101a)
        val later = query(0, cfiPc = 0x101a)
        io.measureActive.poke(true.B)
        if (oldReady) {
          setResolve(0, 0x2030, cfiPc = 0x101a); tick()
          io.resolve(0).valid.poke(false.B); tick(3)
        }
        io.stats.confAccepted.expect((if (oldReady) 1 else 0).U)
        if (concurrent) setResolve(0, 0x2006, cfiPc = 0x1010)
        redirect(later, 0x1008, cfiPc = 0x1010)
        io.resolve(0).valid.poke(false.B)
        if (!concurrent) {
          // Identification alone never trains or claims an actual target.
          tick(3); io.stats.confAccepted.expect((if (oldReady) 1 else 0).U)
          setResolve(0, 0x2006, cfiPc = 0x1010); tick(); io.resolve(0).valid.poke(false.B)
        }
        tick(3)
        // Repeated IFU identification retains the checkpoint; the earlier CFI
        // remains unqualified for training against the original later prediction.
        redirect(later, 0x1008, cfiPc = 0x1010)
        setResolve(0, 0x2030, cfiPc = 0x1010); tick(); io.resolve(0).valid.poke(false.B)
        tick(3)
        val expectedTraining = if (oldReady) 1 else 0 // only original matching later CFI trains
        io.stats.confAccepted.expect(expectedTraining.U)
        io.stats.confApply.expect(expectedTraining.U)
        ptr(io.commit.bits, 0); io.commit.valid.poke(true.B)
        tick(); io.commit.valid.poke(false.B); tick(3)
        io.load.ready.expect(true.B)
      }
      println("HINT_IFU_EARLIER_CFI_NO_TRAIN_CHECKPOINT_RELEASE_PASS")
    }
  }
}
