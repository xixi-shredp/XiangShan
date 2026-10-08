package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import java.nio.file.{Files, Paths}
import org.json4s.{DefaultFormats, JNull, JValue}
import org.json4s.jackson.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec
import top.BackendV2NoRVVNoLLCConfig
import xiangshan.{XSCoreParamsKey, XSTileKey}

class HintTest extends AnyFlatSpec with ChiselSim {
  private val config = new BackendV2NoRVVNoLLCConfig
  implicit val params: org.chipsalliance.cde.config.Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
  }

  // Match the production emulator graph optimization; the default leaves an
  // enormous expanded confidence-log expression in one C++ function.
  override implicit def commonSettingsModifications: svsim.CommonSettingsModifications =
    new svsim.CommonSettingsModifications {
      override def apply(settings: svsim.CommonCompilationSettings): svsim.CommonCompilationSettings =
        settings.copy(optimizationStyle = svsim.CommonCompilationSettings.OptimizationStyle.OptimizeForSimulationSpeed)
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

  behavior of "Continuous Hint with finite resources"
  it should "preserve independent histories and ordered confidence under wrap pressure and redirects" in {
    simulate(new Hint, firtoolOpts = Array("-O=release",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none")) { m =>
      val pcs = Vector.tabulate(16)(i => 0x1000 + i * 0x40)
      var recent = Vector.fill(16)(Vector.fill(4)(0)) // newest first; independent of physical ring
      var heads = Vector.fill(16)(0)
      var checks = 0L
      def ptr(p: xiangshan.frontend.ftq.FtqPtr, id: Int): Unit = {
        p.flag.poke(((id / 64) % 2 != 0).B); p.value.poke((id % 64).U)
      }
      def tick(n: Int = 1): Unit = for (_ <- 0 until n) {
        m.io.canAccept.expect(true.B); m.io.confDropped.expect(0.U)
        val h = m.io.debugConfHead.get.peek().litValue
        val t = m.io.debugConfTail.get.peek().litValue
        val w = m.io.debugConfWrite.get.peek().litValue
        assert((t-h+64)%64 <= 17 && (t-w+64)%64 <= 1)
        val accepted = m.io.stats.confAccepted.peek().litValue
        val retired = m.io.stats.confRetired.peek().litValue
        val canceled = m.io.stats.confCanceled.peek().litValue
        val undone = m.io.stats.confUndo.peek().litValue
        assert(accepted == retired + canceled + undone + (t-h+64)%64,
          s"accepted=$accepted retired=$retired canceled=$canceled undo=$undone occupancy=${(t-h+64)%64}")
        checks += 1; m.clock.step()
      }
      def prediction(p: xiangshan.frontend.bpu.Prediction, taken: Boolean, target: Int,
                     kind: Int = 3): Unit = {
        p.taken.poke(taken.B); p.target.addr.poke((target >> 1).U)
        p.attribute.branchType.poke(kind.U); p.attribute.rasAction.poke(0.U); p.cfiPosition.poke(0.U)
      }
      def input(pc: Int, id: Int, target: Int, taken: Boolean, start: Int = -1): Unit = {
        m.io.s1StartPc.poke((if(start<0) pc else start).U)
        m.io.s1CfiPc.poke(pc.U); m.io.s3CfiPc.poke(pc.U)
        ptr(m.io.s1Ptr,id); ptr(m.io.s3Ptr,id)
        prediction(m.io.s1Prediction,taken,target); prediction(m.io.s3Prediction,taken,target)
      }
      def reset(): Unit = {
        m.io.load.valid.poke(false.B); m.io.enabled.poke(false.B); m.io.profile.poke(false.B)
        m.io.measureActive.poke(false.B); m.io.s1Fire.poke(false.B); m.io.s2Fire.poke(false.B)
        m.io.s3Fire.poke(false.B); m.io.s3Override.poke(false.B); m.io.s3UsedHint.poke(false.B)
        m.io.s3BaselineTarget.poke(0x3000.U); m.io.redirect.valid.poke(false.B)
        m.io.commit.valid.poke(false.B); for(r<-m.io.resolve) { r.valid.poke(false.B); r.bits.offset.poke(0.U); r.bits.mispredict.poke(false.B); r.bits.sourceBackend.poke(true.B) }
        input(0x1000,0,0x2000,false)
        m.reset.poke(true.B); m.clock.step(3); m.reset.poke(false.B)
        recent=Vector.fill(16)(Vector.fill(4)(0)); heads=Vector.fill(16)(0); tick(4096)
      }
      def load(kind: Int,index: Int,value: BigInt): Unit = {
        m.io.load.bits.kind.poke(kind.U); m.io.load.bits.index.poke(index.U)
        m.io.load.bits.data.poke(value.U); m.io.load.valid.poke(true.B)
        m.io.load.ready.expect(true.B); tick(); m.io.load.valid.poke(false.B)
      }
      def cam(index: Int,pc: Int,mask: Int): Unit =
        load(1,index,(BigInt(1)<<51)|(BigInt(mask)<<47)|BigInt(pc>>1))
      def context(conf: Int): Unit = {
        load(3,0,BigInt(0x2000>>1)); load(2,0,(BigInt(1)<<24)|(BigInt(conf)<<22)|BigInt(1))
      }
      def localWrite(pc: Int,target: Int): Unit = {
        val i=pcs.indexOf(pc)
        if(i>=0){recent=recent.updated(i,((target>>1)&3)+:recent(i).take(3));heads=heads.updated(i,(heads(i)+1)&15)}
      }
      def expectSnapshot(values: Vector[Vector[Int]], pointers: Vector[Int]): Unit = {
        for(i<-0 until 16){
          val low=values(i).zipWithIndex.map{case(v,a)=>v<<(2*a)}.sum
          m.io.s3Snapshot.lhrLow8(i).expect(low.U);m.io.s3Snapshot.lhrHead(i).expect(pointers(i).U)
        }; checks+=32
      }
      def copies(a: Data,b: Data): Seq[()=>Unit] = (a,b) match {
        case(x:Bool,y:Bool)=>val v=x.peek().litToBoolean;Seq(()=>y.poke(v.B))
        case(x:UInt,y:UInt)=>val v=x.peek().litValue;Seq(()=>y.poke(v.U))
        case(x:Vec[_],y:Vec[_])=>x.getElements.zip(y.getElements).flatMap{case(u,v)=>copies(u,v)}
        case(x:Record,y:Record)=>x.elements.toSeq.flatMap{case(k,v)=>copies(v,y.elements(k))}
        case _=>throw new IllegalArgumentException("snapshot field")
      }
      case class Saved(values: Vector[Vector[Int]],pointers: Vector[Int],poke:Seq[()=>Unit])
      def query(pc:Int,id:Int,target:Int=0x2000,taken:Boolean=true,start:Int = -1): Saved = {
        val old=recent;val oldHeads=heads;input(pc,id,target,taken,start)
        m.io.s1Fire.poke(true.B);tick();m.io.s1Fire.poke(false.B)
        if(taken)localWrite(pc,target)
        m.io.s2Fire.poke(true.B);tick();m.io.s2Fire.poke(false.B)
        expectSnapshot(old,oldHeads)
        Saved(old,oldHeads,copies(m.io.s3Snapshot,m.io.redirect.bits.meta.hint))
      }
      def confirm(): Unit = {
        m.io.s3UsedHint.poke(m.io.s3Lookup.usable.peek().litToBoolean.B)
        m.io.s3Fire.poke(true.B);tick();m.io.s3Fire.poke(false.B);m.io.s3UsedHint.poke(false.B)
      }
      // Commit denotes the already fully retired FTQ reclamation watermark.
      def commit(id:Int): Unit = {ptr(m.io.commit.bits,id);m.io.commit.valid.poke(true.B);tick();m.io.commit.valid.poke(false.B)}
      def resolve(id:Int,actual:Int,measure:Boolean=true): Unit = {
        val r=m.io.resolve(0);r.valid.poke(true.B);ptr(r.bits.ftqPtr,id)
        r.bits.cfiPc.poke(0x1000.U);r.bits.target.poke(actual.U);r.bits.eligible.poke(true.B)
        r.bits.baselinePc.poke(0x1000.U);r.bits.baselineTarget.poke(0x3000.U)
        r.bits.baselineValid.poke(true.B);r.bits.blockStart.poke(0x1000.U)
        m.io.measureActive.poke(measure.B);tick();r.valid.poke(false.B);m.io.measureActive.poke(false.B)
      }
      def restore(s:Saved,id:Int,pc:Int,target:Int,reapply:Boolean,taken:Boolean=true,kind:Int=3): Unit = {
        s.poke.foreach(_());ptr(m.io.redirect.bits.newFtqIdx,id)
        m.io.redirect.bits.cfiPc.addr.poke((pc>>1).U);m.io.redirect.bits.target.addr.poke((target>>1).U)
        m.io.redirect.bits.taken.poke(taken.B);m.io.redirect.bits.attribute.branchType.poke(kind.U)
        m.io.redirect.bits.attribute.rasAction.poke(0.U);m.io.redirect.bits.reapply.poke(reapply.B)
        m.io.redirect.valid.poke(true.B);tick();m.io.redirect.valid.poke(false.B)
        recent=s.values;heads=s.pointers;if(reapply&&taken&&kind==3)localWrite(pc,target)
      }
      def probe(id:Int): Unit = {query(0x1800,id,taken=false);()}
      def confProbe(id:Int,expected:Int): Unit = {
        query(0x1000,id);m.io.s3Lookup.contextHit.expect(true.B)
        m.io.debugSelectedConf.get.expect(expected.U);m.io.s3Lookup.usable.expect((expected!=0).B)
      }

      // Actual same-FTQ positional counterexample: redirect retains FTQ 10
      // at its earlier ordinary branch (0xffe), so H2P 0x1000 must be killed.
      reset();cam(0,0x1000,0);context(1);m.io.enabled.poke(true.B)
      val sameFtqLater=query(0x1000,10,start=0xfe0);confirm()
      resolve(10,0x2000);tick(5);m.io.stats.confApply.expect(1.U)
      restore(sameFtqLater,11,0xffe,0x4000,reapply=true,kind=1)
      tick(8);m.io.stats.confUndo.expect(1.U);confProbe(11,1)
      println("HINT_SAME_FTQ_EARLIER_REAPPLY_APPLIED_UNDO_PASS")

      // The same positional boundary must cancel an accepted, unapplied log
      // item while a genuinely older FTQ retires on the redirect cycle.
      reset();cam(0,0x1000,0);context(1);m.io.enabled.poke(true.B)
      query(0x1000,9);confirm();resolve(9,0x2000);tick(4)
      val pendingSame=query(0x1000,10,start=0xfe0);confirm();resolve(10,0x3000)
      tick();m.io.stats.confAccepted.expect(2.U);m.io.stats.confApply.expect(1.U)
      ptr(m.io.commit.bits,9);m.io.commit.valid.poke(true.B)
      val pendingAgain=m.io.resolve(0);pendingAgain.valid.poke(true.B)
      ptr(pendingAgain.bits.ftqPtr,10);pendingAgain.bits.cfiPc.poke(0x1000.U)
      pendingAgain.bits.target.poke(0x3000.U);pendingAgain.bits.eligible.poke(true.B)
      m.io.measureActive.poke(true.B)
      restore(pendingSame,11,0xffe,0x4000,reapply=true,kind=1)
      pendingAgain.valid.poke(false.B);m.io.commit.valid.poke(false.B)
      m.io.measureActive.poke(false.B);tick(8);confProbe(11,2)
      m.io.stats.confCanceled.expect(1.U);m.io.stats.confUndo.expect(0.U)
      m.io.stats.confRetired.expect(1.U);m.io.stats.confAccepted.expect(2.U)
      println("HINT_SAME_FTQ_EARLIER_REAPPLY_UNAPPLIED_CANCEL_PASS")

      // Equal-position reapply preserves the branch; a nested earlier cutoff
      // subsequently kills that same record and its applied confidence log.
      reset();cam(0,0x1000,0);context(1);m.io.enabled.poke(true.B)
      val nestedSame=query(0x1000,20,start=0xfe0);confirm()
      resolve(20,0x2000);tick(5)
      restore(nestedSame,21,0x1000,0x2000,reapply=true);tick(4)
      m.io.stats.confUndo.expect(0.U)
      restore(nestedSame,21,0xffe,0x4000,reapply=true,kind=1);tick(8)
      m.io.stats.confUndo.expect(1.U);confProbe(21,1)
      // Late resolve from the killed CFI cannot allocate a new update.
      val acceptedAfterKill=m.io.stats.confAccepted.peek().litValue
      resolve(20,0x2000);tick(5)
      m.io.stats.confAccepted.expect(acceptedAfterKill.U)
      println("HINT_SAME_FTQ_NESTED_CUTOFF_LATE_RESOLVE_PASS")

      // Resolve on the kill cycle has lower priority than the earlier CFI.
      reset();cam(0,0x1000,0);context(1);m.io.enabled.poke(true.B)
      val unresolvedSame=query(0x1000,30,start=0xfe0);confirm()
      val killedResolve=m.io.resolve(0);killedResolve.valid.poke(true.B)
      ptr(killedResolve.bits.ftqPtr,30);killedResolve.bits.cfiPc.poke(0x1000.U)
      killedResolve.bits.target.poke(0x2000.U);killedResolve.bits.eligible.poke(true.B)
      m.io.measureActive.poke(true.B)
      restore(unresolvedSame,31,0xffe,0x4000,reapply=true,kind=1)
      killedResolve.valid.poke(false.B);m.io.measureActive.poke(false.B);tick(6)
      m.io.stats.confAccepted.expect(0.U);m.io.stats.confApply.expect(0.U)
      resolve(30,0x2000);tick(5);m.io.stats.confAccepted.expect(0.U)
      println("HINT_SAME_FTQ_KILL_PRIORITY_UNRESOLVED_PASS")

      def expectProfileCommit(id:Int,valid:Boolean,actual:Int=0x2000):Unit={
        ptr(m.io.commit.bits,id);m.io.commit.valid.poke(true.B)
        m.io.profileEvent.valid.expect(valid.B)
        if(valid){m.io.profileEvent.bits.pc.expect(0x1000.U)
          m.io.profileEvent.bits.actual.expect(actual.U)
          m.io.profileEvent.bits.ftqTag.expect((id%128).U)}
        // A killed FTQ is never legally reclaimed as a retired branch. Probe
        // its combinational event suppression without clocking fake commit.
        if(valid)tick()
        m.io.commit.valid.poke(false.B)
      }
      // These observe the actual production profileRows/profileEvent registers.
      reset();cam(0,0x1000,0);m.io.profile.poke(true.B)
      val profileKilled=query(0x1000,40,start=0xfe0);resolve(40,0x2000)
      restore(profileKilled,41,0xffe,0x4000,reapply=true,kind=1);tick(3)
      expectProfileCommit(40,false)
      resolve(40,0x2000);tick(2);expectProfileCommit(40,false)

      reset();cam(0,0x1000,0);m.io.profile.poke(true.B)
      val profileNested=query(0x1000,42,start=0xfe0);resolve(42,0x2000)
      restore(profileNested,43,0x1002,0x4000,reapply=true,kind=1);tick(2)
      restore(profileNested,43,0xffe,0x4000,reapply=true,kind=1);tick(2)
      expectProfileCommit(42,false)

      reset();cam(0,0x1000,0);m.io.profile.poke(true.B)
      val profileEqual=query(0x1000,44,start=0xfe0);resolve(44,0x2000)
      restore(profileEqual,45,0x1000,0x2000,reapply=true);tick(2)
      expectProfileCommit(44,true)

      reset();cam(0,0x1000,0);m.io.profile.poke(true.B)
      val profileDead=query(0x1000,46,start=0xfe0)
      restore(profileDead,46,0xffe,0x4000,reapply=false,kind=1);tick(2)
      resolve(46,0x2000);tick(2);expectProfileCommit(46,false)
      query(0x1000,110,start=0xfe0)
      resolve(46,0x3000);tick(2)
      // Combinational observation only: do not retire an unresolved H2P.
      ptr(m.io.commit.bits,110);m.io.commit.valid.poke(true.B)
      m.io.profileEvent.valid.expect(false.B);m.io.commit.valid.poke(false.B)
      // The live new flag-generation emits only its own resolved target.
      resolve(110,0x2006);tick(2)
      expectProfileCommit(110,true,0x2006)
      // A surviving earlier backend resolve on the exact S3 override cycle
      // replaces an already-resolved later event in this same live FTQ.
      reset();cam(0,0x1000,0);m.io.profile.poke(true.B)
      query(0x1000,50,start=0xfe0);resolve(50,0x2000)
      input(0xffe,50,0x3004,true,start=0xfe0)
      m.io.s3Override.poke(true.B)
      val s3Replacing=m.io.resolve(0);s3Replacing.valid.poke(true.B)
      ptr(s3Replacing.bits.ftqPtr,50);s3Replacing.bits.cfiPc.poke(0xffe.U)
      s3Replacing.bits.target.poke(0x3004.U);s3Replacing.bits.eligible.poke(true.B)
      s3Replacing.bits.sourceBackend.poke(true.B);s3Replacing.bits.offset.poke(15.U)
      s3Replacing.bits.baselinePc.poke(0xffe.U);s3Replacing.bits.blockStart.poke(0xfe0.U)
      tick();m.io.s3Override.poke(false.B);s3Replacing.valid.poke(false.B);tick(2)
      ptr(m.io.commit.bits,50);m.io.commit.valid.poke(true.B)
      m.io.profileEvent.valid.expect(true.B);m.io.profileEvent.bits.pc.expect(0xffe.U)
      m.io.profileEvent.bits.actual.expect(0x3004.U);tick();m.io.commit.valid.poke(false.B)
      println("HINT_PROFILE_S3_SAME_CYCLE_EARLIER_SURVIVOR_PASS")
      println("HINT_PROFILE_REAL_LIVE_CUTOFF_NEW_GENERATION_PASS")

      for(mask<-Seq(2,0)){
        reset();for(i<-0 until 16)cam(i,pcs(i),mask);m.io.enabled.poke(true.B)
        var at64:Option[Saved]=None
        for(i<-0 until 128){
          if(i>=64)commit(i-64)
          val s=query(pcs(0),i,0x2000+((i*3+i/7)&3)*2)
          if(i==64)at64=Some(s);confirm()
          if(Set(5,8,16,64,128).contains(i+1))probe(i+1)
        }
        restore(at64.get,64,pcs(0),0x2006,reapply=true);tick(2);probe(65)
        for(a<-0 until 4;i<-0 until 16)query(pcs(i),a*16+i,0x2000+((i+a)&3)*2)
        val all=query(0x1800,0,taken=false)
        for(a<-0 until 5;i<-0 until 16)query(pcs(i),a*16+i,0x2000+((i*3+a)&3)*2)
        restore(all,16,0x1800,0x4000,reapply=false,taken=false,kind=1);tick(2);probe(17)
        restore(all,16,pcs(15),0x2006,reapply=true);tick(2);probe(17)
        val s3=query(pcs(0),18,0x2002)
        m.io.s3CfiPc.poke(pcs(4).U);prediction(m.io.s3Prediction,true,0x2006)
        m.io.s3Override.poke(true.B);m.io.s3Fire.poke(true.B);tick()
        m.io.s3Override.poke(false.B);m.io.s3Fire.poke(false.B)
        recent=s3.values;heads=s3.pointers;localWrite(pcs(4),0x2006);probe(19)
        val both=query(pcs(0),20,0x2002);m.io.s3Override.poke(true.B)
        restore(both,20,pcs(8),0x2004,reapply=true);m.io.s3Override.poke(false.B);tick(2);probe(21)
      }
      println("HINT_CONTINUOUS_HISTORY_PASS milestones=5,8,16,64,128 all_pc=16")

      // Exact S1 CFI updates its ring; window candidate only decides the lookup.
      reset();cam(0,0x1000,0);cam(1,0x1010,0);context(3);m.io.enabled.poke(true.B)
      input(0x1010,0,0x2006,true,0x1000);m.io.s1Fire.poke(true.B);tick();m.io.s1Fire.poke(false.B)
      recent=recent.updated(1,3+:recent(1).take(3));heads=heads.updated(1,1)
      m.io.s2Fire.poke(true.B);tick();m.io.s2Fire.poke(false.B)
      m.io.s3Lookup.pc.expect(0x1000.U);m.io.s3Lookup.usable.expect(false.B)
      probe(1);m.io.s3Snapshot.lhrLow8(0).expect(0.U);m.io.s3Snapshot.lhrLow8(1).expect(3.U)

      reset();cam(0,0x1000,0);context(3);m.io.enabled.poke(true.B)
      for(i<-0 until 16){query(0x1000,i,0x2000+(i&3)*2);m.io.s3Lookup.usable.expect(true.B);confirm()}
      m.io.stats.recordAllocate.expect(16.U)
      query(0x1000,16,0x2006);m.io.s3Lookup.usable.expect(false.B);confirm()
      m.io.stats.recordAllocate.expect(16.U);m.io.stats.metadataFallback.expect(1.U);probe(17)
      resolve(0,0x2000);commit(0);tick(8)
      query(0x1000,17);m.io.s3Lookup.usable.expect(true.B);confirm();m.io.stats.recordAllocate.expect(17.U)
      println("HINT_CONTINUOUS_METADATA_PASS capacity=16 normal_s1_blocked=0")

      reset();cam(0,0x1000,0);context(1);m.io.enabled.poke(true.B)
      var logicalConf=1
      for(i<-0 until 130){
        confProbe(i,logicalConf);confirm();val correct=i%7!=0
        resolve(i,if(correct)0x2000 else 0x3000);commit(i);tick(5)
        logicalConf=if(correct)math.min(3,logicalConf+1)else math.max(0,logicalConf-1)
      }
      confProbe(130,logicalConf);m.io.stats.confApply.expect(130.U)
      assert(m.io.debugConfHead.get.peek().litValue==m.io.debugConfTail.get.peek().litValue)
      confirm();resolve(130,0x3000,measure=false);commit(130);tick(5)
      confProbe(131,logicalConf);m.io.stats.confApply.expect(130.U)
      println("HINT_CONTINUOUS_WRAP_PASS applied=130 warmup_no_training=1")

      reset();cam(0,0x1000,0);context(1);m.io.enabled.poke(true.B)
      confProbe(0,1);confirm();confProbe(1,1);confirm()
      resolve(1,0x2000);tick(3);m.io.stats.confApply.expect(0.U)
      resolve(0,0x3000);tick(5);m.io.debugSelectedConf.get.expect(1.U);m.io.stats.confApply.expect(2.U)
      commit(0);commit(1);tick(5);m.io.debugSelectedConf.get.expect(1.U);confProbe(2,1)

      reset();cam(0,0x1000,0);context(1);m.io.enabled.poke(true.B)
      confProbe(0,1);confirm();resolve(0,0x3000);commit(0);tick(6)
      m.io.debugSelectedConf.get.expect(0.U);m.io.s3Lookup.usable.expect(false.B)
      confProbe(1,0);confirm();resolve(1,0x2000);commit(1);tick(6);m.io.debugSelectedConf.get.expect(1.U)
      println("HINT_CONTINUOUS_BROADCAST_PASS conf0_rise=1 stalled_row_retired_log=1")

      reset();cam(0,0x1000,0);context(1);m.io.enabled.poke(true.B)
      confProbe(0,1);confirm();resolve(0,0x3000)
      // Every cycle issues S1, unlike query()'s two-cycle staged driver.
      for(i<-1 to 12){
        input(0x1000,i,0x2000,true)
        m.io.s1Fire.poke(true.B);m.io.s2Fire.poke(true.B)
        tick();localWrite(0x1000,0x2000)
      }
      m.io.s1Fire.poke(false.B);m.io.s2Fire.poke(false.B)
      confProbe(13,0)
      m.io.stats.confApply.expect(1.U);m.io.debugSelectedConf.get.expect(0.U)
      assert(m.io.stats.lookupPortFallback.peek().litValue>0)

      reset();cam(0,0x1000,0);context(1);m.io.enabled.poke(true.B)
      var saved11:Option[Saved]=None;var saved12:Option[Saved]=None
      for(i<-10 to 12){
        val s=query(0x1000,i);if(i==11)saved11=Some(s);if(i==12)saved12=Some(s)
        confirm();resolve(i,if(i==11)0x2000 else 0x3000);tick(4)
      }
      m.io.stats.confApply.expect(3.U)
      restore(saved12.get,12,0x1800,0x4000,reapply=false,taken=false,kind=1)
      restore(saved11.get,11,0x1800,0x4000,reapply=false,taken=false,kind=1)
      tick(8);confProbe(13,0);m.io.stats.confUndo.expect(2.U);commit(10);tick(6)
      println("HINT_CONTINUOUS_NESTED_ROLLBACK_PASS undo=2 oldest_preserved=1")

      // S3 override and S3 record allocation in the same cycle retain the current branch.
      reset();cam(0,0x1000,0);context(1);m.io.enabled.poke(true.B)
      val own=query(0x1000,20,0x3000)
      prediction(m.io.s3Prediction,true,0x2000);m.io.s3Override.poke(true.B)
      confirm();m.io.s3Override.poke(false.B)
      recent=own.values;heads=own.pointers;localWrite(0x1000,0x2000)
      m.io.stats.recordAllocate.expect(1.U)
      resolve(20,0x2000);commit(20);tick(5);confProbe(21,2)
      m.io.stats.confUndo.expect(0.U)
      // Earlier non-H2P CFI in this same FTQ kills the later speculative H2P; no illegal commit.
      val later=query(0x1000,22);confirm()
      restore(later,22,0x1800,0x4000,reapply=false,taken=false,kind=1);tick(5)
      resolve(22,0x3000);tick(5);confProbe(23,2)
      m.io.stats.confApply.expect(1.U)
      m.io.stats.metaStall.expect(0.U);m.io.stats.lhrStall.expect(0.U)
      // Cancel an accepted but unapplied younger suffix. Older commit is legal
      // on the redirect cycle; committing the killed younger FTQ would not be.
      reset();cam(0,0x1000,0);context(1);m.io.enabled.poke(true.B)
      query(0x1000,20);confirm();resolve(20,0x2000);tick(4)
      val pendingYoung=query(0x1000,21);confirm();resolve(21,0x3000)
      tick() // enqueue the young update, but do not give it an apply cycle
      m.io.stats.confAccepted.expect(2.U);m.io.stats.confApply.expect(1.U)
      ptr(m.io.commit.bits,20);m.io.commit.valid.poke(true.B)
      val concurrent=m.io.resolve(0);concurrent.valid.poke(true.B);ptr(concurrent.bits.ftqPtr,21)
      concurrent.bits.cfiPc.poke(0x1000.U);concurrent.bits.target.poke(0x3000.U)
      concurrent.bits.eligible.poke(true.B);m.io.measureActive.poke(true.B)
      restore(pendingYoung,21,0x1800,0x4000,reapply=false,taken=false,kind=1)
      concurrent.valid.poke(false.B);m.io.commit.valid.poke(false.B);m.io.measureActive.poke(false.B)
      tick(8);confProbe(22,2)
      m.io.stats.confAccepted.expect(2.U);m.io.stats.confApply.expect(1.U)
      m.io.stats.confCanceled.expect(1.U);m.io.stats.confUndo.expect(0.U);m.io.stats.confRetired.expect(1.U)
      println("HINT_CONTINUOUS_UNAPPLIED_CANCEL_PASS legal_old_commit_young_resolve_redirect=1")

      // Cache raw confidence in S2, then update/retire the log before S2 moves.
      // Repeat with apply exactly on the stage-transfer cycle.
      for(transferWithApply<-Seq(false,true)){
        reset();cam(0,0x1000,0);context(1);m.io.enabled.poke(true.B)
        query(0x1000,0);confirm();resolve(0,0x3000)
        val before=recent;val beforeHeads=heads
        input(0x1000,1,0x2000,true)
        m.io.s1Fire.poke(true.B);tick();m.io.s1Fire.poke(false.B);localWrite(0x1000,0x2000)
        // S2 holds the raw row read while enqueue occurred, before apply.
        m.io.stats.confAccepted.expect(1.U);m.io.stats.confApply.expect(0.U)
        if(transferWithApply)m.io.s2Fire.poke(true.B)
        tick();m.io.s2Fire.poke(false.B)
        commit(0);tick(5)
        if(!transferWithApply){m.io.s2Fire.poke(true.B);tick();m.io.s2Fire.poke(false.B)}
        expectSnapshot(before,beforeHeads)
        m.io.debugSelectedConf.get.expect(0.U);m.io.s3Lookup.usable.expect(false.B)
        assert(m.io.debugConfHead.get.peek().litValue==m.io.debugConfTail.get.peek().litValue)
      }
      println("HINT_CONTINUOUS_S2_BROADCAST_PASS stalled_retired_log=1 apply_samecycle_transfer=1")

      // Shared nonzero component goldens are expanded to all sixteen masks by
      // the separately frozen Python hash contract. Actual RTL histories are
      // seeded through normal events; no private register forcing is used.
      implicit val formats: DefaultFormats.type = DefaultFormats
      val vectorsPath=Iterator.iterate(Paths.get("").toAbsolutePath)(_.getParent)
        .takeWhile(_ != null).map(_.resolve("results/v2config-norvv-nol3-1m1m/20260930/stage7-continuous-hint/tests/all-mask-golden.json"))
        .find(path=>Files.isRegularFile(path)).get
      val allMasks=(parse(Files.readString(vectorsPath)) \ "vectors").children
      def hex(v:JValue,name:String):BigInt=BigInt((v \ name).extract[String].stripPrefix("0x"),16)
      def emitHistory(pc:BigInt,cell:Int,kind:Int,ras:Int):Unit={
        m.io.s1StartPc.poke(pc.U);m.io.s1CfiPc.poke(pc.U);ptr(m.io.s1Ptr,0)
        prediction(m.io.s1Prediction,true,cell<<1,kind)
        m.io.s1Prediction.attribute.rasAction.poke(ras.U)
        m.io.s1Fire.poke(true.B);tick();m.io.s1Fire.poke(false.B)
      }
      def seed(value:BigInt,count:Int)(emit:Int=>Unit):Unit=
        for(i<-(count-1) to 0 by -1)emit(((value>>(2*i))&3).toInt)
      for(group<-allMasks.groupBy(v=>hex(v,"pc")).toSeq.sortBy(_._1)){
        val (pc,vs)=group;val sample=vs.head
        reset();load(1,0,(BigInt(1)<<51)|((pc>>1)&((BigInt(1)<<47)-1)));load(3,0,BigInt(0x2000>>1))
        m.io.profile.poke(true.B)
        seed(hex(sample,"lhr_low8"),4){cell=>emitHistory(pc,cell,3,0)}
        seed(hex(sample,"ith_low64"),32){cell=>emitHistory(BigInt(0x2000),cell,3,0)}
        seed(hex(sample,"call_low64"),32){cell=>emitHistory(BigInt(cell<<1),0,2,2)}
        seed(hex(sample,"ret_low64"),32){cell=>emitHistory(BigInt(cell<<1),0,3,1)}
        m.io.profile.poke(false.B)
        for(v<-vs.sortBy(x=>(x \ "mask").extract[Int])){
          m.io.enabled.poke(false.B)
          val mask=(v \ "mask").extract[Int];val set=(v \ "set").extract[Int];val tag=(v \ "tag").extract[Int]
          load(1,0,(BigInt(1)<<51)|(BigInt(mask)<<47)|((pc>>1)&((BigInt(1)<<47)-1)))
          load(2,set*2,(BigInt(1)<<24)|(BigInt(3)<<22)|BigInt(tag))
          m.io.enabled.poke(true.B)
          m.io.s1StartPc.poke(pc.U);m.io.s1CfiPc.poke(pc.U);m.io.s3CfiPc.poke(pc.U)
          ptr(m.io.s1Ptr,0);ptr(m.io.s3Ptr,0)
          prediction(m.io.s1Prediction,false,0x2000);prediction(m.io.s3Prediction,true,0x2000)
          m.io.s1Fire.poke(true.B);tick();m.io.s1Fire.poke(false.B)
          m.io.s2Fire.poke(true.B);tick();m.io.s2Fire.poke(false.B)
          m.io.s3Lookup.pc.expect(pc.U);m.io.s3Lookup.set.expect(set.U)
          m.io.s3Lookup.contextHit.expect(true.B);m.io.s3Lookup.usable.expect(true.B)
          m.io.s3Lookup.target.expect(0x2000.U)
          m.io.s3Snapshot.lhrLow8(0).expect(hex(sample,"lhr_low8").U)
          checks+=6
        }
      }
      println(s"HINT_CONTINUOUS_ALL_MASK_GOLDEN_PASS vectors=${allMasks.size} masks=16 nonzero_history=1")
      println(s"HINT_CONTINUOUS_ACTUAL_RTL_PASS checks=$checks forced_state=0")
    }
  }
}
