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
    simulate(new HintSramTestTop(), firtoolOpts = Array("-O=release",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none")) { top =>
      val m=new {val io=top.io.hint;val clock=top.clock;val reset=top.reset}
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
      def reset(profile: Boolean, hardware: Boolean = false, conf: Int = 1): Unit = {
        zero(m.io.load.bits); zero(m.io.redirect.bits); zero(m.io.commit.bits)
        zero(m.io.traceCommit.bits);m.io.traceCommit.valid.poke(false.B)
        m.io.resolve.foreach(r => { r.valid.poke(false.B); zero(r.bits) })
        zero(m.io.s1Prediction); zero(m.io.s3Prediction)
        m.io.load.valid.poke(false.B); m.io.enabled.poke(false.B); m.io.profile.poke(false.B)
        m.io.measureActive.poke(false.B); m.io.s1Fire.poke(false.B); m.io.s2Fire.poke(false.B)
        m.io.s3Fire.poke(false.B); m.io.s3Override.poke(false.B); m.io.s3UsedHint.poke(false.B)
        m.io.s1StartPc.poke(0x1000.U); m.io.s1CfiPc.poke(0x1006.U); m.io.s3CfiPc.poke(0x1006.U)
        m.io.s3BaselineTarget.poke(0x2000.U); ptr(m.io.s1Ptr, 10); ptr(m.io.s3Ptr, 10)
        m.io.redirect.valid.poke(false.B); m.io.commit.valid.poke(false.B)
        m.reset.poke(true.B); m.clock.step(3); m.reset.poke(false.B); tick(4096)
        load(1, 0, (BigInt(3) << 52) | (BigInt(1) << 51) | BigInt(0x1000 >> 1))
        if(hardware) {
          load(3, 0, BigInt(0x2000 >> 1))
          load(2, 2, (BigInt(1) << 24) | (BigInt(conf) << 22) | BigInt(0x2800))
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
      def query(id: Int = 10, recognized: Boolean = true): Seq[() => Unit] = {
        ptr(m.io.s1Ptr,id); ptr(m.io.s3Ptr,id)
        for(p <- Seq(m.io.s1Prediction,m.io.s3Prediction)) {
          p.taken.poke(recognized.B); p.target.addr.poke((0x2000 >> 1).U)
          p.attribute.branchType.poke((if(recognized)3 else 0).U); p.attribute.rasAction.poke(0.U); p.cfiPosition.poke(3.U)
        }
        m.io.s1Fire.poke(true.B); tick(); m.io.s1Fire.poke(false.B)
        m.io.s2Fire.poke(true.B); tick(); m.io.s2Fire.poke(false.B)
        copies(m.io.s3Snapshot,m.io.redirect.bits.meta.hint)
      }
      def setResolve(pc: Int, id: Int = 10, actual: Int = 0x3000): Unit = {
        val r=m.io.resolve(0); r.valid.poke(true.B); ptr(r.bits.ftqPtr,id)
        r.bits.cfiPc.poke(pc.U); r.bits.target.poke(actual.U); r.bits.eligible.poke(true.B)
        r.bits.sourceBackend.poke(true.B); r.bits.fresh.poke(true.B); r.bits.baselineTarget.poke(0x2000.U); r.bits.baselineValid.poke(true.B)
        r.bits.offset.poke(((pc-0x1000)/2).U)
        }
      def resolve(pc: Int = 0x1006, id: Int = 10): Unit = {
        setResolve(pc,id); m.io.measureActive.poke(true.B); tick()
        m.io.resolve(0).valid.poke(false.B); m.io.measureActive.poke(false.B)
      }
      def setRedirect(saved: Seq[() => Unit], pc: Int, self: Boolean, id: Int = 10): Unit = {
        saved.foreach(_()); ptr(m.io.redirect.bits.newFtqIdx,id+1)
        m.io.redirect.bits.startPc.addr.poke((0x1000>>1).U)
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
      def confirm():Unit={
        m.io.s3UsedHint.poke(m.io.s3Lookup.usable.peek());m.io.s3Fire.poke(true.B);tick()
        m.io.s3Fire.poke(false.B);m.io.s3UsedHint.poke(false.B)
      }
      def commit(id:Int=10,expectReplay:Boolean=true):Unit={
        ptr(m.io.commit.bits,id);m.io.measureActive.poke(true.B)
        m.io.commit.valid.poke(true.B)
        tick();m.io.commit.valid.poke(false.B)
        m.io.measureActive.poke(false.B)
      }
      // Resolved writes survive later wrong-path cancellation. Metadata still
      // follows the actual CFI coordinate and redirect inclusivity.
      for(self<-Seq(false,true);cutoff<-Seq(0x1004,0x1006,0x1008)){
        reset(profile=false,hardware=true);val saved=query();confirm();resolve();tick(4)
        m.io.stats.confAccepted.expect(1.U);m.io.stats.confApply.expect(1.U)
        redirect(saved,cutoff,self);tick(4)
        val survives=cutoff>0x1006 || cutoff==0x1006 && !self
        commit(expectReplay=survives);tick(4);m.io.stats.confApply.expect(1.U)
        m.io.stats.confUndo.expect(0.U);m.io.stats.confCanceled.expect(0.U)
        query(11);m.io.s3Lookup.contextHit.expect(true.B)
        m.io.s3Lookup.usable.expect(false.B)
      }
      println("HINT_REDIRECT_SIX_ORDERING_RESOLVE_APPROX_PASS retained_written_training=1 no_undo=1")
      // Equal inclusive same-edge resolve never qualifies a later commit.
      reset(profile=false,hardware=true);val concurrent=query();confirm();setResolve(0x1006)
      m.io.measureActive.poke(true.B);setRedirect(concurrent,0x1006,true);tick()
      m.io.redirect.valid.poke(false.B);m.io.resolve(0).valid.poke(false.B)
      m.io.measureActive.poke(false.B);tick(12)
      m.io.stats.confAccepted.expect(0.U);m.io.stats.confApply.expect(0.U)
      query(11);m.io.s3Lookup.usable.expect(true.B)
      println("HINT_REDIRECT_CONCURRENT_KILL_RESOLVE_PASS")
      // Unknown windows acquire their true CFI from the same backend event
      // that determines whether this redirect preserves or cancels them.
      for(self<-Seq(false,true);cutoff<-Seq(0x1004,0x1006,0x1008)){
        reset(profile=false,hardware=true,conf=0);val unknown=query(recognized=false);confirm()
        setResolve(0x1006);m.io.resolve(0).bits.baselineValid.poke(false.B)
        m.io.measureActive.poke(true.B);setRedirect(unknown,cutoff,self);tick()
        m.io.redirect.valid.poke(false.B);m.io.resolve(0).valid.poke(false.B)
        m.io.measureActive.poke(false.B);tick(4)
        val survives=cutoff>0x1006 || cutoff==0x1006 && !self
        m.io.stats.confAccepted.expect(0.U)
        commit(expectReplay=survives);tick(3)
        query(11);m.io.s3Lookup.usable.expect(false.B)
      }
      reset(profile=false,hardware=true);val unknown=query(recognized=false);confirm()
      redirect(unknown,0x1008,false);commit(expectReplay=false)
      m.io.stats.confAccepted.expect(0.U)
      println("HINT_UNKNOWN_CFI_REDIRECT_CHECKPOINT_RELEASE_PASS unrecognized_no_training=1")
      println(s"HINT_REDIRECT_LEVEL_REAL_RTL_PASS checks=$checks forced_state=0")
    }
  }
}
