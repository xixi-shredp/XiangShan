package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import top.BackendV2NoRVVNoLLCConfig
import xiangshan.{DebugOptionsKey, XSCoreParamsKey, XSTileKey}
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}

/** Real finite Hint registers/queues; FPGAPlatform removes only external DPI sinks. */
class HintRedirectLevelTest extends AnyFlatSpec with ChiselSim {
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
        s.copy(optimizationStyle = svsim.CommonCompilationSettings.OptimizationStyle.OptimizeForSimulationSpeed)
    }
  override implicit def backendSettingsModifications: svsim.BackendSettingsModifications =
    new svsim.BackendSettingsModifications {
      override def apply(s: svsim.Backend.Settings): svsim.Backend.Settings = s match {
        case v: svsim.verilator.Backend.CompilationSettings =>
          v.withOutputSplit(Some(20000)).withOutputSplitCFuncs(Some(2000))
        case x => x
      }
    }
  behavior of "Hint redirect coordinates and flush level"
  it should "retain earlier branches and cancel inclusive, late and concurrent wrong-path state" in {
    simulate(new Hint, firtoolOpts = Array("-O=release",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none")) { m =>
      var checks = 0
      def zero(d: Data): Unit = d match {
        case x: Bool => x.poke(false.B)
        case x: UInt => x.poke(0.U)
        case x: Vec[_] => x.getElements.foreach(zero)
        case x: Record => x.elements.values.foreach(zero)
        case _ => throw new IllegalArgumentException("unexpected input field")
      }
      def ptr(p: xiangshan.frontend.ftq.FtqPtr, id: Int): Unit = {
        p.flag.poke(((id & 64) != 0).B); p.value.poke((id & 63).U)
      }
      def tick(n: Int = 1): Unit = for (_ <- 0 until n) {
        m.io.canAccept.expect(true.B); m.io.confDropped.expect(0.U)
        m.clock.step(); checks += 2
      }
      def load(kind: Int, index: Int, value: BigInt): Unit = {
        m.io.load.bits.kind.poke(kind.U); m.io.load.bits.index.poke(index.U)
        m.io.load.bits.data.poke(value.U); m.io.load.valid.poke(true.B)
        m.io.load.ready.expect(true.B); tick(); m.io.load.valid.poke(false.B)
      }
      def reset(profile: Boolean, hardware: Boolean = false): Unit = {
        zero(m.io.load.bits); zero(m.io.redirect.bits); zero(m.io.commit.bits)
        m.io.resolve.foreach(r => { r.valid.poke(false.B); zero(r.bits) })
        zero(m.io.s1Prediction); zero(m.io.s3Prediction)
        m.io.load.valid.poke(false.B); m.io.enabled.poke(false.B); m.io.profile.poke(false.B)
        m.io.measureActive.poke(false.B); m.io.s1Fire.poke(false.B); m.io.s2Fire.poke(false.B)
        m.io.s3Fire.poke(false.B); m.io.s3Override.poke(false.B); m.io.s3UsedHint.poke(false.B)
        m.io.s1StartPc.poke(0xfe0.U); m.io.s1CfiPc.poke(0x1000.U); m.io.s3CfiPc.poke(0x1000.U)
        m.io.s3BaselineTarget.poke(0x2000.U); ptr(m.io.s1Ptr, 10); ptr(m.io.s3Ptr, 10)
        m.io.redirect.valid.poke(false.B); m.io.commit.valid.poke(false.B)
        m.reset.poke(true.B); m.clock.step(3); m.reset.poke(false.B); tick(4096)
        load(1, 0, (BigInt(1) << 51) | BigInt(0x1000 >> 1))
        if(hardware) {
          load(3, 0, BigInt(0x2000 >> 1))
          load(2, 0, (BigInt(1) << 24) | (BigInt(1) << 22) | BigInt(1))
        }
        m.io.profile.poke(profile.B); m.io.enabled.poke(hardware.B)
      }
      def copies(a: Data, b: Data): Seq[() => Unit] = (a,b) match {
        case (x: Bool,y: Bool) => val v=x.peek().litToBoolean; Seq(() => y.poke(v.B))
        case (x: UInt,y: UInt) => val v=x.peek().litValue; Seq(() => y.poke(v.U))
        case (x: Vec[_],y: Vec[_]) => x.getElements.zip(y.getElements).flatMap { case (u,v) => copies(u,v) }
        case (x: Record,y: Record) => x.elements.toSeq.flatMap { case(k,v) => copies(v,y.elements(k)) }
        case _ => throw new IllegalArgumentException("snapshot type")
      }
      def query(id: Int = 10): Seq[() => Unit] = {
        ptr(m.io.s1Ptr,id); ptr(m.io.s3Ptr,id)
        for(p <- Seq(m.io.s1Prediction,m.io.s3Prediction)) {
          p.taken.poke(true.B); p.target.addr.poke((0x2000 >> 1).U)
          p.attribute.branchType.poke(3.U); p.attribute.rasAction.poke(0.U); p.cfiPosition.poke(0.U)
        }
        m.io.s1Fire.poke(true.B); tick(); m.io.s1Fire.poke(false.B)
        m.io.s2Fire.poke(true.B); tick(); m.io.s2Fire.poke(false.B)
        copies(m.io.s3Snapshot,m.io.redirect.bits.meta.hint)
      }
      def setResolve(pc: Int, id: Int = 10, actual: Int = 0x3000): Unit = {
        val r=m.io.resolve(0); r.valid.poke(true.B); ptr(r.bits.ftqPtr,id)
        r.bits.cfiPc.poke(pc.U); r.bits.target.poke(actual.U); r.bits.eligible.poke(true.B)
        r.bits.sourceBackend.poke(true.B); r.bits.baselinePc.poke(pc.U)
        r.bits.baselineTarget.poke(0x2000.U); r.bits.baselineValid.poke(true.B)
        r.bits.blockStart.poke(0xfe0.U); r.bits.offset.poke(((pc-0xfe0)/2).U)
        r.bits.mispredict.poke(true.B)
      }
      def resolve(pc: Int = 0x1000, id: Int = 10): Unit = {
        setResolve(pc,id); m.io.measureActive.poke(true.B); tick()
        m.io.resolve(0).valid.poke(false.B); m.io.measureActive.poke(false.B)
      }
      def setRedirect(saved: Seq[() => Unit], pc: Int, self: Boolean, id: Int = 10): Unit = {
        saved.foreach(_()); ptr(m.io.redirect.bits.newFtqIdx,id+1)
        m.io.redirect.bits.cfiPc.addr.poke((pc >> 1).U)
        m.io.redirect.bits.target.addr.poke((0x4000 >> 1).U)
        m.io.redirect.bits.taken.poke(false.B); m.io.redirect.bits.attribute.branchType.poke(1.U)
        m.io.redirect.bits.attribute.rasAction.poke(0.U)
        m.io.redirect.bits.reapply.poke(true.B); m.io.redirect.bits.flushItself.poke(self.B)
        m.io.redirect.valid.poke(true.B)
      }
      def redirect(saved: Seq[() => Unit], pc: Int, self: Boolean): Unit = {
        setRedirect(saved,pc,self); tick(); m.io.redirect.valid.poke(false.B)
      }
      def event(valid: Boolean, pc: Int = 0x1000, id: Int = 10): Unit = {
        ptr(m.io.commit.bits,id); m.io.commit.valid.poke(true.B)
        m.io.profileEvent.valid.expect(valid.B)
        if(valid) m.io.profileEvent.bits.pc.expect(pc.U)
        m.io.commit.valid.poke(false.B); checks += 1
      }
      // All six ordering/level outcomes and persistent late-resolution behavior.
      for(self <- Seq(false,true); cutoff <- Seq(0xffe,0x1000,0x1002)) {
        reset(profile=true); val s=query(); resolve(); redirect(s,cutoff,self); tick(2)
        val survives = cutoff>0x1000 || cutoff==0x1000 && !self
        event(survives); resolve(); tick(2); event(survives)
      }
      println("HINT_REDIRECT_SIX_ORDERING_AND_LATE_RESOLVE_PASS")
      // At the same position inclusive becomes stronger and cannot later weaken.
      reset(profile=true); val equal=query(); resolve()
      redirect(equal,0x1000,false); event(true)
      redirect(equal,0x1000,true); event(false)
      redirect(equal,0x1000,false); resolve(); event(false)
      // A smaller exclusive cutoff is stronger than a larger inclusive cutoff.
      reset(profile=true); val smaller=query(); resolve()
      redirect(smaller,0x1002,true); redirect(smaller,0x1000,false); event(true)
      println("HINT_REDIRECT_PERSISTENT_INCLUSIVE_MERGE_PASS")
      // Exact backend redirect cycle: replace a younger resolved event with an earlier survivor.
      reset(profile=true); val replacement=query(); resolve()
      setResolve(0xffc); setRedirect(replacement,0xffe,true); tick()
      m.io.redirect.valid.poke(false.B); m.io.resolve(0).valid.poke(false.B)
      event(true,0xffc)
      // Equal-position simultaneous resolve under flushSelf must never become resolved.
      reset(profile=true); val concurrent=query(); setResolve(0x1000)
      setRedirect(concurrent,0x1000,true); tick()
      m.io.redirect.valid.poke(false.B); m.io.resolve(0).valid.poke(false.B)
      event(false); resolve(); event(false)
      println("HINT_REDIRECT_SAME_CYCLE_EARLIER_REPLACEMENT_PASS")
      def confirm(): Unit = {
        m.io.s3UsedHint.poke(true.B); m.io.s3Fire.poke(true.B); tick()
        m.io.s3Fire.poke(false.B); m.io.s3UsedHint.poke(false.B)
      }
      // Actual finite confidence SRAM: 1 -> 0 wrong resolve, inclusive undo -> 1.
      reset(profile=false,hardware=true); val applied=query(); confirm(); resolve(); tick(5)
      m.io.stats.confApply.expect(1.U)
      query(11); m.io.s3Lookup.contextHit.expect(true.B); m.io.s3Lookup.usable.expect(false.B)
      // Exclusive first trigger, immediately strengthened while rollback is active.
      redirect(applied,0x1000,false); redirect(applied,0x1000,true); tick(8)
      m.io.stats.confUndo.expect(1.U); query(11); m.io.s3Lookup.usable.expect(true.B)
      val accepted=m.io.stats.confAccepted.peek().litValue
      resolve(); tick(5); m.io.stats.confAccepted.expect(accepted.U)
      println("HINT_REDIRECT_CONFIDENCE_EQUAL_STRENGTH_UNDO_PASS")
      // A genuinely earlier record and its applied update survive flushSelf at a later PC.
      reset(profile=false,hardware=true); val earlier=query(); confirm(); resolve(); tick(5)
      redirect(earlier,0x1002,true); tick(8); m.io.stats.confUndo.expect(0.U)
      query(11); m.io.s3Lookup.usable.expect(false.B)
      println("HINT_REDIRECT_ACTUAL_EARLIER_SURVIVOR_PASS")
      println(s"HINT_REDIRECT_LEVEL_REAL_RTL_PASS checks=$checks forced_state=0")
    }
  }
}
