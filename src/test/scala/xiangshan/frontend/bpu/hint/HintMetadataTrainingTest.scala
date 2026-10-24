package xiangshan.frontend.bpu.hint

import chisel3._
import java.nio.file.Files
import chisel3.reflect.DataMirror
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import top.BackendV2NoRVVNoLLCConfig
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{DebugOptionsKey, XSCoreParamsKey, XSTileKey}

/** Drives the production Hint and normal SRAMs, with the shared finite FTQ harness. */
class HintMetadataTrainingTest extends AnyFlatSpec with ChiselSim {
  private val config = new BackendV2NoRVVNoLLCConfig
  implicit val params: org.chipsalliance.cde.config.Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, true, false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.NORMAL, 0)
  }
  private lazy val observerSource: String = {
    val directory = Files.createTempDirectory("hint-observer-source-")
    val source = directory.resolve("observer_only_stubs.cpp")
    directory.toFile.deleteOnExit()
    source.toFile.deleteOnExit()
    val input = getClass.getResourceAsStream("/hint/observer_only_stubs.cpp")
    require(input != null, "missing Hint observer test resource")
    try Files.copy(input, source)
    finally input.close()
    source.toAbsolutePath.toString
  }

  // Test-only low C++ optimization keeps compilation practical. The hardware,
  // cycle schedule and assertions remain identical to the production DUT.
  // Link only no-return observers; actual loader has a separate test.
  override implicit def commonSettingsModifications: svsim.CommonSettingsModifications =
    new svsim.CommonSettingsModifications {
      override def apply(settings: svsim.CommonCompilationSettings): svsim.CommonCompilationSettings =
        settings.copy(
          optimizationStyle = svsim.CommonCompilationSettings.OptimizationStyle.OptimizeForCompilationSpeed,
          linkLibraryPaths = settings.linkLibraryPaths :+ observerSource)
    }

  override implicit def backendSettingsModifications: svsim.BackendSettingsModifications =
    new svsim.BackendSettingsModifications {
      override def apply(s: svsim.Backend.Settings): svsim.Backend.Settings = s match {
        case v: svsim.verilator.Backend.CompilationSettings =>
          v.withOutputSplit(Some(20000)).withOutputSplitCFuncs(Some(2000))
        case x => x
      }
    }

  behavior of "Minimal Hint metadata production training"
  it should "train final eligible CFIs including conf0 and survive simultaneous FTQ lifetime events" in {
    simulate(new HintSramTestTop(classifyFinal = true), firtoolOpts = Array("-O=release",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none")) { top =>
      val io = top.io.hint
      val classification = top.io.classification.get
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
        case (x: Bool, y: Bool) => val value = x.peek().litToBoolean; Seq(() => y.poke(value.B))
        case (x: UInt, y: UInt) => val value = x.peek().litValue; Seq(() => y.poke(value.U))
        case (x: Record, y: Record) => x.elements.toSeq.flatMap { case (k, v) => copySaved(v, y.elements(k)) }
        case (x: Vec[_], y: Vec[_]) => x.getElements.zip(y.getElements).flatMap { case (u, v) => copySaved(u, v) }
        case _ => throw new IllegalArgumentException("snapshot")
      }
      def load(kind: Int, index: Int, data: BigInt): Unit = {
        io.load.bits.kind.poke(kind.U); io.load.bits.index.poke(index.U); io.load.bits.data.poke(data.U)
        io.load.valid.poke(true.B); var waited = 0
        while (!io.load.ready.peek().litToBoolean && waited < 20000) { tick(); waited += 1 }
        assert(waited < 20000); tick(); io.load.valid.poke(false.B)
      }
      def reset(conf: Int): Unit = {
        initialize(top.io); top.reset.poke(true.B); top.clock.step(3); top.reset.poke(false.B)
        var waited = 0
        while (!io.tableReady.peek().litToBoolean && waited < 20000) { tick(); waited += 1 }
        assert(waited < 20000)
        load(1, 0, (BigInt(3) << 52) | (BigInt(1) << 51) | BigInt(0x1000 >> 1))
        load(3, 0, 0x2000 >> 1)
        load(2, 2, (BigInt(1) << 24) | (BigInt(conf) << 22) | BigInt(0x2800))
        io.enabled.poke(true.B); io.measureActive.poke(true.B)
        classification.fallThroughTarget.addr.poke((0x3000 >> 1).U)
      }
      case class Query(id: Int, cfi: Int, taken: Boolean, finalTarget: BigInt,
          snapshot: Seq[() => Unit], trace: Seq[() => Unit])
      def query(id: Int, kind: Int = 3, taken: Boolean = true, cfi: Int = 0x1006, baselineTarget: Int = 0x3000, leaveS3Pending: Boolean = false): Query = {
        ptr(io.s1Ptr, id); ptr(io.s3Ptr, id)
        io.s1StartPc.poke(0x1000.U); io.s1CfiPc.poke(cfi.U); io.s3CfiPc.poke(cfi.U)
        for (p <- Seq(io.s1Prediction, io.s3Prediction)) {
          p.taken.poke(taken.B); p.attribute.branchType.poke(kind.U)
          p.attribute.rasAction.poke(0.U); p.target.addr.poke((baselineTarget >> 1).U)
          p.cfiPosition.poke((((cfi - 0x1000) / 2) & 31).U)
        }
        io.s3BaselineTarget.poke(baselineTarget.U)
        io.s1Fire.poke(true.B); tick(); io.s1Fire.poke(false.B)
        io.s2Fire.poke(true.B); tick(); io.s2Fire.poke(false.B)
        val used = classification.usedHint.peek().litToBoolean
        val finalTarget = classification.prediction.target.addr.peek().litValue << 1
        val finalCfi = 0x1000 + classification.prediction.cfiPosition.peek().litValue.toInt * 2
        val finalTaken = classification.prediction.taken.peek().litToBoolean
        val result = Query(id, finalCfi, finalTaken, finalTarget,
          copySaved(io.s3Snapshot, io.redirect.bits.meta.hint),
          copySaved(io.s3Snapshot, io.traceCommit.bits.snapshot))
        if (!leaveS3Pending) {
          io.s3UsedHint.poke(used.B); io.s3Fire.poke(true.B); tick()
          io.s3Fire.poke(false.B); io.s3UsedHint.poke(false.B)
        }
        result
      }
      def resolve(q: Query, actual: Int, actualCfi: Int = 0x1006,
          eligible: Boolean = true, fresh: Boolean = true, lane: Int = 0): Unit = {
        val r = io.resolve(lane); ptr(r.bits.ftqPtr, q.id)
        r.bits.cfiPc.poke(actualCfi.U); r.bits.target.poke(actual.U)
        r.bits.eligible.poke(eligible.B); r.bits.sourceBackend.poke(true.B); r.bits.fresh.poke(fresh.B)
        r.bits.offset.poke(((actualCfi - 0x1000) / 2).U)
        r.bits.baselineTarget.poke(q.finalTarget.U)
        r.bits.baselineValid.poke((q.taken && q.cfi == actualCfi).B); r.valid.poke(true.B)
      }
      def clearResolve(): Unit = io.resolve.foreach(_.valid.poke(false.B))
      def commit(q: Query): Unit = {
        ptr(io.commit.bits, q.id); io.commit.valid.poke(true.B); tick(); io.commit.valid.poke(false.B)
      }
      def probe(id: Int, expected: Int): Unit = {
        val q = query(id); io.debugSelectedConf.get.expect(expected.U); commit(q)
      }
      // The production selector repairs unknown/direct/conditional/untaken
      // baseline identities before real Hint training. Actual position mismatches
      // still cannot train against an unrelated resident FTQ target.
      for (initial <- Seq(0, 2); (kind, taken, actualCfi) <- Seq(
        (0, true, 0x1006), (1, true, 0x1006), (2, true, 0x1006),
        (3, false, 0x1006), (3, true, 0x1004), (3, true, 0x100a))) {
        reset(initial); val q = query(0, kind, taken)
        io.stats.recordAllocate.expect(1.U)
        resolve(q, 0x2000, actualCfi); tick(); clearResolve(); tick(3)
        val matches = actualCfi == 0x1006
        io.stats.confAccepted.expect((if (matches) 1 else 0).U)
        io.stats.confApply.expect((if (matches) 1 else 0).U)
        val expected = if (!matches) initial else if (initial == 0) 1 else 3
        commit(q); probe(1, expected)
      }
      println("HINT_CAM58_TYPE_TAKEN_REPAIR_AND_FINAL_POSITION_TRAINING_PASS")

      for ((initial, actual, expected, up, down) <- Seq(
        (0, 0x3000, 0, 0, 0), (0, 0x2000, 1, 1, 0),
        (2, 0x2000, 3, 1, 0), (2, 0x3000, 1, 0, 1),
        (3, 0x2000, 3, 0, 0))) {
        reset(initial); val q = query(0)
        resolve(q, actual); tick(); clearResolve(); tick(3)
        io.stats.confAccepted.expect(1.U); io.stats.confApply.expect(1.U)
        io.stats.confUp.expect(up.U); io.stats.confDown.expect(down.U)
        commit(q); probe(1, expected)
      }
      println("HINT_METADATA_FTQ_TARGET_CONF0_SATURATION_ACCEPTED_COUNTERS_PASS")

      // Context miss retains CAM identity while selecting baseline targets.
      reset(2)
      io.enabled.poke(false.B)
      load(2, 2, 0)
      io.enabled.poke(true.B)
      classification.ittageHit.poke(true.B)
      classification.ittageTarget.addr.poke((0x4440 >> 1).U)
      val contextMiss = query(0, kind = 0, taken = false)
      assert(contextMiss.taken && contextMiss.cfi == 0x1006 && contextMiss.finalTarget == 0x4440)
      io.s3Lookup.contextHit.expect(false.B)
      resolve(contextMiss, 0x2000); tick(); clearResolve(); tick(3)
      io.stats.confAccepted.expect(0.U); commit(contextMiss)
      println("HINT_CAM58_CONTEXT_MISS_CLASSIFIES_WITH_ITTAGE_TARGET_NO_TRAIN_PASS")

      // A second S1 arrives while the previous lookup occupies S3. It retains
      // an admitted checkpoint but cannot inherit the old Context response.
      reset(2)
      val firstBusy = query(0, baselineTarget = 0x2000, leaveS3Pending = true)
      ptr(io.s1Ptr, 1)
      io.s1Prediction.taken.poke(false.B); io.s1Prediction.attribute.branchType.poke(0.U)
      io.s1Prediction.target.addr.poke((0x3000 >> 1).U)
      io.s1Fire.poke(true.B); io.s3Fire.poke(true.B); tick()
      io.s1Fire.poke(false.B); io.s3Fire.poke(false.B)
      ptr(io.s3Ptr, 1)
      io.s2Fire.poke(true.B); tick(); io.s2Fire.poke(false.B)
      io.s3Prediction.taken.poke(false.B); io.s3Prediction.attribute.branchType.poke(0.U)
      io.s3Prediction.target.addr.poke((0x3000 >> 1).U)
      classification.prediction.taken.expect(true.B)
      classification.prediction.attribute.branchType.expect(3.U)
      classification.prediction.target.addr.expect((0x3000 >> 1).U)
      classification.camSelected.expect(true.B); classification.usedHint.expect(false.B)
      io.s3Lookup.admitted.expect(true.B); io.s3Lookup.contextHit.expect(false.B)
      val busy = Query(1, 0x1006, true, BigInt(0x3000),
        copySaved(io.s3Snapshot, io.redirect.bits.meta.hint),
        copySaved(io.s3Snapshot, io.traceCommit.bits.snapshot))
      io.s3Fire.poke(true.B); tick(); io.s3Fire.poke(false.B)
      resolve(busy, 0x2000); tick(); clearResolve(); tick(3)
      io.stats.confAccepted.expect(0.U); io.stats.recordAllocate.expect(2.U)
      io.stats.lhrWrites.expect(2.U)
      commit(firstBusy); commit(busy)
      println("HINT_CAM58_BUSY_CLASSIFIES_WITH_CHECKPOINT_NO_STALE_CONTEXT_TRAIN_PASS")

      // Detail remains an observation of the original predictor, including an
      // unrecognized branch and its original fall-through control fields.
      reset(2); io.enabled.poke(false.B); io.profile.poke(true.B)
      val detail = query(0, kind = 0, taken = false, cfi = 0x1010)
      assert(!detail.taken && detail.cfi == 0x1010 && detail.finalTarget == 0x3000)
      classification.camSelected.expect(false.B); classification.usedHint.expect(false.B)
      io.stats.lhrWrites.expect(0.U); commit(detail)
      println("HINT_CAM58_DETAIL_PRESERVES_ORIGINAL_BPU_PASS")

      // A type-only difference must trigger correction even with identical
      // taken/position/target. The production Hint observer counts this override.
      reset(2); val typeOnly = query(0, kind = 2, baselineTarget = 0x2000)
      assert(typeOnly.finalTarget == 0x2000 && typeOnly.cfi == 0x1006)
      io.stats.overrides.expect(1.U)
      commit(typeOnly)
      println("HINT_CAM58_TYPE_ONLY_OVERRIDE_PASS")

      // Earlier taken indirects belong to the ordinary BPU, not this CAM CFI.
      reset(2); val earlier = query(0, cfi = 0x1004)
      assert(earlier.cfi == 0x1004 && earlier.finalTarget == 0x3000)
      classification.camSelected.expect(false.B); classification.usedHint.expect(false.B)
      io.stats.lhrWrites.expect(0.U)
      resolve(earlier, 0x2000, actualCfi = 0x1004); tick(); clearResolve(); tick(3)
      io.stats.confAccepted.expect(0.U); commit(earlier)
      println("HINT_CAM58_EARLIER_TAKEN_NO_CAM_LHR_OR_TRAINING_PASS")

      // The ninth same-CAM window still gains its identity, but has no local
      // checkpoint and cannot use/train Hint. The global ITH must still advance.
      reset(2)
      val occupied = (0 until 8).map(i => query(i, baselineTarget = 0x2000))
      io.stats.lhrWrites.expect(8.U)
      classification.fallThroughTarget.addr.poke((0x3002 >> 1).U)
      val unowned = query(8, kind = 0, taken = false)
      assert(unowned.cfi == 0x1006 && unowned.taken && unowned.finalTarget == 0x3002)
      classification.camSelected.expect(true.B); classification.usedHint.expect(false.B)
      io.s3Lookup.admitted.expect(false.B); io.stats.recordAllocate.expect(8.U)
      io.stats.lhrWrites.expect(8.U)
      // Snapshot precedes the ninth correction. Restore it onto the trace input
      // and compare to the next window's snapshot after the global append.
      def folded(q: Query): BigInt = {
        q.trace.foreach(_()); io.traceCommit.bits.ftqTag.poke(q.id.U)
        io.traceCommit.bits.queryStartPc.poke(0x1000.U)
        io.traceCommit.bits.actualPc.poke(0x1004.U); io.traceCommit.bits.actualTarget.poke(0x2000.U)
        io.traceCommit.bits.eligible.poke(true.B); io.traceCommit.bits.queryValid.poke(true.B)
        io.traceCommit.valid.poke(true.B)
        val value = io.profileEvent.bits.ith.peek().litValue
        io.traceCommit.valid.poke(false.B); value
      }
      val beforeGlobal = folded(unowned)
      resolve(unowned, 0x2000); tick(); clearResolve(); tick(3)
      io.stats.confAccepted.expect(0.U)
      val nextUnowned = query(9, kind = 0, taken = false)
      val afterGlobal = folded(nextUnowned)
      assert(beforeGlobal == 0 && afterGlobal != 0,
        "Unowned CAM classification must still append the fallback target to global ITH")
      occupied.foreach(commit); commit(unowned); commit(nextUnowned)
      println("HINT_CAM58_LHR_FULL_CLASSIFIES_WITHOUT_LOCAL_UPDATE_OR_TRAINING_PASS")

      // Resolve and commit share the old payload on one edge. No ready/commit
      // record or post-commit drain may be needed for either training or reuse.
      reset(1); val sameEdge = query(0)
      resolve(sameEdge, 0x2000); ptr(io.commit.bits, 0); io.commit.valid.poke(true.B)
      tick(); clearResolve(); io.commit.valid.poke(false.B); tick(3)
      io.stats.confAccepted.expect(1.U); io.stats.confApply.expect(1.U)
      io.debugMetaHead.get.expect(1.U); io.debugMetaTail.get.expect(1.U)
      val reused = query(64) // same physical FTQ slot, opposite generation
      io.debugSelectedConf.get.expect(2.U)
      resolve(sameEdge, 0x3000); tick(); clearResolve(); tick(2)
      io.stats.confAccepted.expect(1.U) // stale generation cannot touch new payload
      resolve(reused, 0x2000); tick(); clearResolve(); tick(2); commit(reused)
      io.stats.confAccepted.expect(2.U)
      println("HINT_METADATA_RESOLVE_COMMIT_SAME_EDGE_AND_FTQ_GENERATION_REUSE_PASS")

      // Three owned windows, then a selected redirect and an older commit on
      // the same edge. The surviving payload must retain its original index.
      reset(1); val oldest = query(0); val boundary = query(1); query(2)
      boundary.snapshot.foreach(_()); ptr(io.redirect.bits.newFtqIdx, 2)
      io.redirect.bits.startPc.addr.poke((0x1000 >> 1).U)
      io.redirect.bits.cfiPc.addr.poke((0x1006 >> 1).U)
      io.redirect.bits.target.addr.poke((0x2002 >> 1).U)
      io.redirect.bits.taken.poke(true.B); io.redirect.bits.attribute.branchType.poke(3.U)
      io.redirect.bits.attribute.rasAction.poke(0.U); io.redirect.bits.reapply.poke(true.B)
      io.redirect.bits.flushItself.poke(false.B); io.redirect.valid.poke(true.B)
      resolve(boundary, 0x2000); ptr(io.commit.bits, oldest.id); io.commit.valid.poke(true.B)
      tick(); clearResolve(); io.commit.valid.poke(false.B); io.redirect.valid.poke(false.B); tick(3)
      io.debugMetaHead.get.expect(1.U); io.debugMetaTail.get.expect(2.U)
      io.stats.confAccepted.expect(1.U)
      resolve(boundary, 0x3000, fresh = false); tick(); clearResolve()
      io.stats.confAccepted.expect(1.U)
      commit(boundary); probe(2, 2)
      io.tableReady.expect(true.B)
      println("HINT_METADATA_REDIRECT_RESOLVE_OLDER_COMMIT_COMPRESSION_PASS")

      // An equal inclusive flush kills a same-edge resolve, but keeps the
      // window checkpoint until the existing FTQ commit releases it.
      reset(1); val killed = query(0); killed.snapshot.foreach(_())
      ptr(io.redirect.bits.newFtqIdx, 1); io.redirect.bits.reapply.poke(true.B)
      io.redirect.bits.flushItself.poke(true.B); io.redirect.bits.taken.poke(true.B)
      io.redirect.bits.attribute.branchType.poke(3.U); io.redirect.bits.attribute.rasAction.poke(0.U)
      io.redirect.bits.startPc.addr.poke((0x1000 >> 1).U)
      io.redirect.bits.cfiPc.addr.poke((0x1006 >> 1).U)
      io.redirect.bits.target.addr.poke((0x2002 >> 1).U); io.redirect.valid.poke(true.B)
      resolve(killed, 0x2000); tick(); clearResolve(); io.redirect.valid.poke(false.B); tick(3)
      io.stats.confAccepted.expect(0.U); commit(killed); probe(1, 1)
      println("HINT_METADATA_INCLUSIVE_REDIRECT_SAME_EDGE_RESOLVE_NO_TRAIN_PASS")
    }
  }
}
