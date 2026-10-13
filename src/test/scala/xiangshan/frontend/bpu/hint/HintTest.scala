package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import java.nio.file.{Files, Paths}
import org.json4s.{DefaultFormats, JNull, JValue}
import org.json4s.jackson.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec
import top.BackendV2NoRVVNoLLCConfig
import xiangshan.{XSCoreParamsKey, XSTileKey}

// One elaboration preserves the production SRAM Definition identity and
// connects the same normal-mode global control distribution used by Frontend.
class HintSramTestTop(implicit p: org.chipsalliance.cde.config.Parameters) extends Module {
  val predictor=Module(new Hint)
  val contextTable=Module(new HintContextTable)
  val slotTable=Module(new HintSlotTable)
  val io=IO(new Bundle {
    val hint=predictor.io.cloneType
    val context=contextTable.io.cloneType
    val slot=slotTable.io.cloneType
  })
  io.hint <> predictor.io
  io.context <> contextTable.io
  io.slot <> slotTable.io
  val normalBroadcast=utility.sram.SramHelper.genBroadCastBundleTop()
  normalBroadcast := 0.U.asTypeOf(normalBroadcast)
}

class HintObserverLinkPreflight extends Module {
  val io=IO(new Bundle { val input=Input(UInt(8.W));val output=Output(UInt(8.W)) })
  io.output := RegNext(io.input,0.U)
  val observer=Module(new xiangshan.frontend.ftq.HintPendingDpi)
  observer.io.clock := clock
  observer.io.pending := io.input
  observer.io.outstanding := 0.U
  observer.io.meta := 0.U
  val traceObserver=Module(new HintTraceDpi)
  traceObserver.io.clock := clock
  traceObserver.io.valid := false.B
  traceObserver.io.data := io.input
  traceObserver.io.discovery := false.B
  traceObserver.io.ftqTag := 0.U
  traceObserver.io.queryCycle := 0.U
  traceObserver.io.overflow := false.B
  traceObserver.io.occupancy := 0.U
}

class HintTest extends AnyFlatSpec with ChiselSim {
  private val config = new BackendV2NoRVVNoLLCConfig
  implicit val params: org.chipsalliance.cde.config.Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
  }

  // Keep the observer implementation in the test classpath, rather than
  // depending on a precompiled object in an experiment results directory.
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
  // Link only the three no-return observers; actual loader has a separate test.
  override implicit def commonSettingsModifications: svsim.CommonSettingsModifications =
    new svsim.CommonSettingsModifications {
      override def apply(settings: svsim.CommonCompilationSettings): svsim.CommonCompilationSettings =
        settings.copy(
          optimizationStyle = svsim.CommonCompilationSettings.OptimizationStyle.OptimizeForCompilationSpeed,
          linkLibraryPaths = settings.linkLibraryPaths :+ observerSource)
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
  it should "preflight the real observer ABI and test compiler linkage" in {
    simulate(new HintObserverLinkPreflight) { m =>
      m.io.input.poke(0.U);m.reset.poke(true.B);m.clock.step(2);m.reset.poke(false.B)
      m.io.input.poke(7.U);m.clock.step();m.io.output.expect(7.U)
      println("HINT_OBSERVER_PUBLIC_LINK_PREFLIGHT_PASS")
    }
  }

  it should "preserve histories and finite queues using the actual synchronous SRAM ports" in {
    simulate(new HintSramTestTop, firtoolOpts=Array("-O=release",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none")) { top =>
      {
        val m=new { val io=top.io.hint;val clock=top.clock;val reset=top.reset }
      val pcs = Vector.tabulate(16)(i => 0x1000 + i * 0x40)
      var recent = Vector.fill(16)(Vector.fill(4)(0)) // newest first; independent of physical ring
      var heads = Vector.fill(16)(0)
      var checks = 0L
      def ptr(p: xiangshan.frontend.ftq.FtqPtr, id: Int): Unit = {
        p.flag.poke(((id / 64) % 2 != 0).B); p.value.poke((id % 64).U)
      }
      def tick(n: Int = 1): Unit = for (_ <- 0 until n) {
        m.io.canAccept.expect(true.B)
        val h = m.io.debugConfHead.get.peek().litValue
        val t = m.io.debugConfTail.get.peek().litValue
        val occupancy = (t-h+32)%32
        assert(occupancy <= 16, s"FIFO capacity exceeded: $occupancy")
        val accepted = m.io.stats.confAccepted.peek().litValue
        val retired = m.io.stats.confRetired.peek().litValue
        m.io.stats.confUndo.expect(0.U);m.io.stats.confCanceled.expect(0.U)
        assert(accepted == retired + occupancy,
          s"accepted=$accepted retired=$retired occupancy=$occupancy")
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
        m.io.commit.valid.poke(false.B);m.io.traceCommit.valid.poke(false.B); for(r<-m.io.resolve) { r.valid.poke(false.B); r.bits.offset.poke(0.U); r.bits.mispredict.poke(false.B); r.bits.sourceBackend.poke(true.B) }
        input(0x1000,0,0x2000,false)
        m.reset.poke(true.B); m.clock.step(3); m.reset.poke(false.B)
        recent=Vector.fill(16)(Vector.fill(4)(0)); heads=Vector.fill(16)(0)
        var waited=0
        while(!m.io.tableReady.peek().litToBoolean && waited<20000){tick();waited+=1}
        assert(m.io.tableReady.peek().litToBoolean,"actual SRAM reset completion timeout")
      }
      def load(kind: Int,index: Int,value: BigInt): Unit = {
        // Offline loading starts only after inactive prediction tokens drain.
        // This drives normal stage consumption rather than forcing private state.
        m.io.enabled.expect(false.B);m.io.profile.expect(false.B)
        if(!m.io.tableReady.peek().litToBoolean){
          m.io.s2Fire.poke(true.B);m.io.s3Fire.poke(true.B);tick(2)
          m.io.s2Fire.poke(false.B);m.io.s3Fire.poke(false.B)
        }
        m.io.load.bits.kind.poke(kind.U); m.io.load.bits.index.poke(index.U)
        m.io.load.bits.data.poke(value.U); m.io.load.valid.poke(true.B)
        var waited=0
        while(!m.io.load.ready.peek().litToBoolean && waited<20000){tick();waited+=1}
        assert(m.io.load.ready.peek().litToBoolean,"actual SRAM load acceptance timeout")
        m.io.tableReady.expect(false.B) // No old resetDone acknowledgement on a load edge.
        tick(); m.io.load.valid.poke(false.B)
        if(kind==0)m.io.tableReady.expect(false.B) // Clear must cross the real reset edge.
        while(!m.io.tableReady.peek().litToBoolean && waited<20000){tick();waited+=1}
        assert(m.io.tableReady.peek().litToBoolean,"actual SRAM load completion timeout")
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
      def commit(id:Int,measure:Boolean=true): Unit = {
        ptr(m.io.commit.bits,id);m.io.measureActive.poke(measure.B)
        m.io.commit.valid.poke(true.B);tick();m.io.commit.valid.poke(false.B)
        m.io.measureActive.poke(false.B)
      }
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

      // Clear-only is the exact empty-image hardware sequence. Old raw
      // resetDone is high before acceptance, but completion must wait for reset.
      reset();load(0,0,BigInt(0));m.io.tableReady.expect(true.B)
      println("HINT_SRAM_CLEAR_ONLY_REAL_DONE_PASS old_done_edge_rejected=1 real_sram_reset_complete=1")

      // Resolve-only and wrong-path records never reach the committed FIFO.
      reset();cam(0,0x1000,0);context(1);m.io.enabled.poke(true.B)
      val killed=query(0x1000,10,start=0xfe0);confirm();resolve(10,0x2000);tick(8)
      m.io.stats.confAccepted.expect(0.U);m.io.stats.confApply.expect(0.U)
      restore(killed,11,0xffe,0x4000,reapply=true,kind=1);tick(6)
      resolve(10,0x2000);tick(4);confProbe(11,1)
      m.io.stats.confAccepted.expect(0.U);m.io.stats.confUndo.expect(0.U)
      println("HINT_CONF16_COMMIT_ONLY_KILL_PASS")

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
      m.io.stats.recordAllocate.expect(16.U);m.io.canAccept.expect(true.B);probe(17)
      resolve(0,0x2000);commit(0);tick(8)
      query(0x1000,17);m.io.s3Lookup.usable.expect(true.B);confirm();m.io.stats.recordAllocate.expect(17.U)
      println("HINT_CONTINUOUS_METADATA_PASS capacity=16 normal_s1_blocked=0")

      // Same-address RMW uses the actual SRAM value, not a stale prediction.
      reset();cam(0,0x1000,0);context(3);m.io.enabled.poke(true.B)
      var logicalConf=3
      for((correct,i)<-Vector(false,false,false,true,true,true,true,false).zipWithIndex){
        confProbe(i,logicalConf);confirm();resolve(i,if(correct)0x2000 else 0x3000)
        m.io.stats.confAccepted.expect(i.U) // Resolve did not enqueue.
        commit(i);tick(12)
        logicalConf=if(correct)math.min(3,logicalConf+1)else math.max(0,logicalConf-1)
        confProbe(i+40,logicalConf)
      }
      m.io.stats.confApply.expect(8.U)
      println("HINT_CONF16_SERIAL_SATURATION_PASS sequence=3,2,1,0,1,2,3,3,2")

      // A qualified backend resolve and FTQ commit on one edge train exactly once.
      // IFU/noneligible resolves must skip and release the candidate metadata.
      for((backend,eligible,expected)<-Seq((true,true,1),(false,true,0),(true,false,0))){
        reset();cam(0,0x1000,0);context(1);m.io.enabled.poke(true.B)
        query(0x1000,0);confirm()
        val r=m.io.resolve(0);r.valid.poke(true.B);ptr(r.bits.ftqPtr,0)
        r.bits.cfiPc.poke(0x1000.U);r.bits.target.poke(0x2000.U)
        r.bits.eligible.poke(eligible.B);r.bits.sourceBackend.poke(backend.B)
        r.bits.baselinePc.poke(0x1000.U);r.bits.baselineTarget.poke(0x3000.U)
        r.bits.baselineValid.poke(true.B);r.bits.blockStart.poke(0x1000.U)
        commit(0);r.valid.poke(false.B);tick(12)
        m.io.stats.confAccepted.expect(expected.U);m.io.stats.confApply.expect(expected.U)
        confProbe(1,1+expected)
      }
      println("HINT_CONF16_QUALIFIED_SAMEEDGE_COMMIT_PASS backend=1 ifu_skipped=1 noneligible_skipped=1")

      // Warmup/restore resolve or commit cannot qualify a training event.
      for((resolveMeasure,commitMeasure)<-Seq((false,true),(true,false),(false,false))){
        reset();cam(0,0x1000,0);context(1);m.io.enabled.poke(true.B)
        query(0x1000,0);confirm();resolve(0,0x3000,resolveMeasure);commit(0,commitMeasure);tick(12)
        m.io.stats.confAccepted.expect(0.U);m.io.stats.confApply.expect(0.U);confProbe(1,1)
      }
      println("HINT_CONF16_PHASE_QUALIFICATION_PASS")

      // A stalled prediction owns its raw SRAM conf even after training changes it.
      reset();cam(0,0x1000,0);context(1);m.io.enabled.poke(true.B)
      query(0x1000,0);confirm();resolve(0,0x3000)
      val before=recent;val beforeHeads=heads
      input(0x1000,1,0x2000,true)
      m.io.s1Fire.poke(true.B);tick();m.io.s1Fire.poke(false.B);localWrite(0x1000,0x2000)
      commit(0);tick(12) // train read/write occurs while S2 is held.
      m.io.stats.confApply.expect(1.U)
      m.io.s2Fire.poke(true.B);tick();m.io.s2Fire.poke(false.B)
      expectSnapshot(before,beforeHeads)
      m.io.debugSelectedConf.get.expect(1.U);m.io.s3Lookup.usable.expect(true.B)
      // A later S1 sees the newly written zero; correct shadow target can recover it.
      confirm();commit(1,measure=false);tick(4)
      confProbe(2,0);confirm();resolve(2,0x2000);commit(2);tick(12);confProbe(3,1)
      println("HINT_CONF16_RAW_STALLED_PREDICTION_PASS old_query=1 new_query=0 conf0_recovered=1")

      // At least two complete FIFO pointer wraps with real sequential SRAM RMW.
      reset();cam(0,0x1000,0);context(1);m.io.enabled.poke(true.B)
      logicalConf=1
      for(i<-0 until 70){
        confProbe(i,logicalConf);confirm();val correct=i%7!=0
        resolve(i,if(correct)0x2000 else 0x3000);commit(i);tick(12)
        logicalConf=if(correct)math.min(3,logicalConf+1)else math.max(0,logicalConf-1)
      }
      confProbe(71,logicalConf);m.io.stats.confApply.expect(70.U)
      assert(m.io.debugConfHead.get.peek().litValue==m.io.debugConfTail.get.peek().litValue)
      println("HINT_CONF16_WRAP_PASS events=70")

      // Back-to-back independent set/way/slot identities expose a one-stage
      // alignment mistake that repeated-PC tests cannot detect. Empty Context
      // and absent CAM windows may carry held Slot bits, but cannot reuse them.
      reset();cam(0,0x1000,0);cam(1,0x1040,0);cam(2,0x1080,0)
      load(3,0,BigInt(0x2000>>1));load(3,1,BigInt(0x4000>>1))
      load(2,0,(BigInt(1)<<24)|(BigInt(1)<<22)|BigInt(1))
      load(2,65,(BigInt(1)<<24)|(BigInt(2)<<22)|(BigInt(1)<<17)|BigInt(1))
      m.io.enabled.poke(true.B)
      val streamPcs=Vector(0x1000,0x1040,0x1000,0x1080,0x1000,0x1800,0x1040)
      for(cycle<-0 until streamPcs.size+2){
        val pc=if(cycle<streamPcs.size)streamPcs(cycle) else 0x1800
        input(pc,cycle,0x3000,false)
        val s3Id=cycle-2
        m.io.s1Fire.poke((cycle<streamPcs.size).B)
        m.io.s2Fire.poke((cycle>=1 && cycle<streamPcs.size+1).B)
        m.io.s3Fire.poke((s3Id>=0).B)
        if(s3Id>=0){
          val retiredPc=streamPcs(s3Id)
          ptr(m.io.s3Ptr,s3Id);m.io.s3CfiPc.poke(retiredPc.U)
          prediction(m.io.s3Prediction,true,0x3000)
          val hasTarget=retiredPc==0x1000 || retiredPc==0x1040
          m.io.s3Lookup.usable.expect(hasTarget.B)
          m.io.s3Lookup.contextHit.expect(hasTarget.B)
          if(hasTarget){
            m.io.s3Lookup.pc.expect(retiredPc.U)
            m.io.s3Lookup.way.expect((if(retiredPc==0x1040)1 else 0).U)
            m.io.s3Lookup.target.expect((if(retiredPc==0x1040)0x4000 else 0x2000).U)
            m.io.debugSelectedConf.get.expect((if(retiredPc==0x1040)2 else 1).U)
          }
        }
        m.io.s3UsedHint.poke(m.io.s3Lookup.usable.peek().litToBoolean.B);tick()
      }
      m.io.s1Fire.poke(false.B);m.io.s2Fire.poke(false.B);m.io.s3Fire.poke(false.B)
      m.io.s3UsedHint.poke(false.B)
      println("HINT_SRAM_PIPELINE_IDENTITY_PASS independent_sets=2 independent_ways=2 stale_slot_miss_rejected=1 back_to_back=1")

      // A real continuous S1/S2/S3 stream starves physical writes until the
      // 16-entry commit FIFO fills. Full drops training, never a stalled
      // ordinary S1 and never a Hint lookup fallback.
      reset();cam(0,0x1000,0);context(1);m.io.enabled.poke(true.B)
      m.io.measureActive.poke(true.B)
      var maxOutstanding=BigInt(0)
      val streamLength=120
      for(cycle<-0 until streamLength+48){
        val querying=cycle<streamLength
        input(0x1000,cycle,0x2000,true)
        ptr(m.io.s3Ptr,math.max(0,cycle-2))
        m.io.s1Fire.poke(querying.B)
        m.io.s2Fire.poke((cycle>=1 && cycle<streamLength+1).B)
        m.io.s3Fire.poke((cycle>=2 && cycle<streamLength+2).B)
        m.io.s3UsedHint.poke(m.io.s3Lookup.usable.peek().litToBoolean.B)
        val resolveId=cycle-5;val retiringId=cycle-9
        val r=m.io.resolve(0)
        r.valid.poke((resolveId>=0 && resolveId<streamLength).B)
        if(resolveId>=0 && resolveId<streamLength){
          ptr(r.bits.ftqPtr,resolveId);r.bits.cfiPc.poke(0x1000.U)
          r.bits.target.poke(0x2000.U);r.bits.eligible.poke(true.B)
          r.bits.baselinePc.poke(0x1000.U);r.bits.baselineTarget.poke(0x3000.U)
          r.bits.baselineValid.poke(true.B);r.bits.blockStart.poke(0x1000.U)
        }
        m.io.commit.valid.poke((retiringId>=0 && retiringId<streamLength).B)
        if(retiringId>=0 && retiringId<streamLength)ptr(m.io.commit.bits,retiringId)
        val h=m.io.debugConfHead.get.peek().litValue
        val t=m.io.debugConfTail.get.peek().litValue
        maxOutstanding=maxOutstanding.max((t-h+32)%32)
        tick();if(querying)localWrite(0x1000,0x2000)
      }
      m.io.s1Fire.poke(false.B);m.io.s2Fire.poke(false.B);m.io.s3Fire.poke(false.B)
      m.io.s3UsedHint.poke(false.B);m.io.resolve(0).valid.poke(false.B)
      m.io.commit.valid.poke(false.B);m.io.measureActive.poke(false.B)
      var drainCycles=0
      while(m.io.debugConfHead.get.peek().litValue!=m.io.debugConfTail.get.peek().litValue && drainCycles<128){
        tick();drainCycles+=1
      }
      assert(drainCycles<128,"finite RMW FIFO failed to drain without prediction reads")
      assert(maxOutstanding==16,s"did not exercise full FIFO capacity: $maxOutstanding")
      val acceptedStream=m.io.stats.confAccepted.peek().litValue
      val drops=m.io.stats.confDropped.peek().litValue
      assert(drops>0,s"full FIFO did not drop training: $drops")
      m.io.stats.confRetired.expect(acceptedStream.U)
      m.io.stats.confApply.expect(acceptedStream.U)
      m.io.stats.lookupPortFallback.expect(0.U)
      assert(m.io.debugConfHead.get.peek().litValue==m.io.debugConfTail.get.peek().litValue)
      confProbe(121,3)
      println(s"HINT_CONF16_FULL_DROP_PASS maxOutstanding=$maxOutstanding accepted=$acceptedStream dropped=$drops ordinary_s1_always_ready=1 hint_read_continues=1")

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
        // Exact FTQ snapshot-to-commit fold: no whole-history profile shadow.
        m.io.s1StartPc.poke(pc.U);m.io.s1CfiPc.poke(pc.U);ptr(m.io.s1Ptr,0)
        prediction(m.io.s1Prediction,false,0x2000)
        m.io.s1Fire.poke(true.B);tick();m.io.s1Fire.poke(false.B)
        m.io.s2Fire.poke(true.B);tick();m.io.s2Fire.poke(false.B)
        val commitSnapshot=copies(m.io.s3Snapshot,m.io.traceCommit.bits.snapshot)
        val restoreSnapshot=copies(m.io.s3Snapshot,m.io.redirect.bits.meta.hint)
        commitSnapshot.foreach(_())
        m.io.traceCommit.bits.actualPc.poke(pc.U);m.io.traceCommit.bits.actualTarget.poke(0x2000.U)
        m.io.traceCommit.bits.queryStartPc.poke(pc.U);m.io.traceCommit.bits.eligible.poke(true.B)
        m.io.traceCommit.bits.queryValid.poke(true.B);m.io.traceCommit.bits.baselineMiss.poke(true.B)
        m.io.traceCommit.bits.pcId.poke(0.U);m.io.traceCommit.bits.ftqTag.poke(0.U)
        m.io.traceCommit.bits.queryCycle.foreach(_.poke(0.U))
        m.io.traceCommit.valid.poke(true.B)
        def expectFold():Unit={
          val cs=(sample \ "components").children.map(v=>BigInt(v.extract[String].stripPrefix("0x"),16))
          m.io.profileEvent.valid.expect(true.B);m.io.profileEvent.bits.queryValid.expect(true.B)
          m.io.profileEvent.bits.ith.expect(cs(0).U);m.io.profileEvent.bits.lhr.expect(cs(1).U)
          m.io.profileEvent.bits.call.expect(cs(2).U);m.io.profileEvent.bits.ret.expect(cs(3).U)
        }
        expectFold();m.io.traceCommit.valid.poke(false.B)
        // A fifth same-PC LHR write and compressed Call/Ret rewrite cannot
        // change the old snapshot. Global ITH advances to exactly the 96-cell limit.
        for(i<-0 until 5)emitHistory(pc,(i+1)&3,3,0)
        for(i<-0 until 91)emitHistory(BigInt(0x2000),(i*3)&3,3,0)
        emitHistory(BigInt(0x135a),0,2,2);emitHistory(BigInt(0x246c),0,3,1)
        m.io.traceCommit.valid.poke(true.B);expectFold();m.io.traceCommit.valid.poke(false.B)
        restoreSnapshot.foreach(_());ptr(m.io.redirect.bits.newFtqIdx,0)
        m.io.redirect.bits.cfiPc.addr.poke(((pc>>1)&((BigInt(1)<<47)-1)).U);m.io.redirect.bits.target.addr.poke(0x1000.U)
        m.io.redirect.bits.taken.poke(false.B);m.io.redirect.bits.attribute.branchType.poke(1.U)
        m.io.redirect.bits.attribute.rasAction.poke(0.U);m.io.redirect.bits.reapply.poke(false.B)
        m.io.redirect.valid.poke(true.B);tick();m.io.redirect.valid.poke(false.B)
        println("HINT_TRACE_FTQ_SNAPSHOT_FOLD_PASS ith96=1 lhr_fifth=1 call_low12_patch=1 ret_low12_patch=1 full_history_shadow=0")
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
      // All children share the top reset. Leaf testing resets an inactive Hint,
      // just as the real loader holds enabled/profile low through SRAM clear.
      m.io.enabled.poke(false.B);m.io.profile.poke(false.B);m.io.load.valid.poke(false.B)
      m.io.s1Fire.poke(false.B);m.io.s2Fire.poke(false.B);m.io.s3Fire.poke(false.B)

      }
      {
        val m=new { val io=top.io.context;val clock=top.clock;val reset=top.reset }
      var portChecks=0
      def tick():Unit={
        // Both logical operations must be excluded at the public hardware ports.
        val readFire=m.io.read.valid.peek().litToBoolean && m.io.read.ready.peek().litToBoolean
        val trainFire=m.io.trainRead.valid.peek().litToBoolean && m.io.trainRead.ready.peek().litToBoolean
        val loadFire=m.io.load.valid.peek().litToBoolean && m.io.load.ready.peek().litToBoolean
        val writeFire=m.io.writeConf.valid.peek().litToBoolean && m.io.writeConf.ready.peek().litToBoolean
        assert(Seq(readFire,trainFire,loadFire,writeFire).count(identity)<=1,"Context logical operation conflict")
        assert(!(loadFire && writeFire),"loader and dynamic conf write conflict")
        portChecks+=1;m.clock.step()
      }
      def ready():Unit={
        var waited=0
        while(!m.io.resetDone.peek().litToBoolean && waited<20000){tick();waited+=1}
        assert(m.io.resetDone.peek().litToBoolean,"Context macro reset never completes")
      }
      def row(set:Int,way:Int,conf:Int):BigInt=
        (BigInt(1)<<24)|(BigInt(conf)<<22)|(BigInt((set*3+way)&31)<<17)|BigInt(0x100+set*2+way)
      var expected=Vector.tabulate(8,2)((set,way)=>row(set,way,(set+way)&3))
      m.io.clear.poke(false.B);m.io.read.valid.poke(false.B);m.io.read.bits.poke(0.U)
      m.io.trainRead.valid.poke(false.B);m.io.trainRead.bits.poke(0.U)
      m.io.load.valid.poke(false.B);m.io.load.bits.index.poke(0.U);m.io.load.bits.data.poke(0.U)
      m.io.writeConf.valid.poke(false.B);m.io.writeConf.bits.index.poke(0.U);m.io.writeConf.bits.conf.poke(0.U)
      m.reset.poke(true.B);m.clock.step(3);m.reset.poke(false.B)
      // The conf macro finishes much earlier than meta. A pending paired load
      // must not be half-accepted in that interval.
      m.io.load.bits.index.poke(15.U);m.io.load.bits.data.poke(expected(7)(1).U)
      m.io.load.valid.poke(true.B)
      var resetCycles=0
      while(!m.io.resetDone.peek().litToBoolean && resetCycles<20000){
        m.io.load.ready.expect(false.B);m.io.read.ready.expect(false.B)
        m.io.writeConf.ready.expect(false.B);tick();resetCycles+=1
      }
      assert(m.io.resetDone.peek().litToBoolean,"paired SRAM reset timeout")
      m.io.load.ready.expect(true.B);tick();m.io.load.valid.poke(false.B)
      def load(index:Int,data:BigInt):Unit={
        m.io.load.bits.index.poke(index.U);m.io.load.bits.data.poke(data.U)
        m.io.load.valid.poke(true.B);m.io.load.ready.expect(true.B);tick();m.io.load.valid.poke(false.B)
      }
      for(set<-0 until 8;way<-0 until 2)load(set*2+way,expected(set)(way))
      def read(set:Int):Unit={
        m.io.read.bits.poke(set.U);m.io.read.valid.poke(true.B);m.io.read.ready.expect(true.B)
        tick();m.io.respValid.expect(true.B);m.io.readSet.expect(set.U)
        for(way<-0 until 2)m.io.rows(way).expect(expected(set)(way).U)
      }
      // All eight logical sets occupy one folded physical row.
      for(set<-0 until 8)read(set)
      m.io.writeConf.bits.index.poke(0.U);m.io.writeConf.bits.conf.poke(3.U)
      m.io.writeConf.valid.poke(true.B)
      for(set<-0 until 8){
        m.io.read.bits.poke(set.U);m.io.read.valid.poke(true.B)
        m.io.read.ready.expect(true.B);m.io.writeConf.ready.expect(false.B)
        tick();m.io.respValid.expect(true.B);m.io.readSet.expect(set.U)
        for(way<-0 until 2)m.io.rows(way).expect(expected(set)(way).U)
      }
      m.io.read.valid.poke(false.B);m.io.writeConf.ready.expect(true.B)
      tick();m.io.writeConf.valid.poke(false.B)
      expected=expected.updated(0,expected(0).updated(0,(expected(0)(0) & (~(BigInt(3)<<22)))|(BigInt(3)<<22)))
      for(set<-0 until 8)read(set)
      // Training reads share one logical port and must not replace a held prediction row.
      read(7);m.io.read.valid.poke(false.B)
      val held=expected(7)
      for(set<-0 until 8;way<-0 until 2){
        m.io.trainRead.bits.poke((set*2+way).U);m.io.trainRead.valid.poke(true.B)
        m.io.trainRead.ready.expect(true.B);tick();m.io.trainRespValid.expect(true.B)
        m.io.trainConf.expect(((expected(set)(way)>>22)&3).U)
        m.io.respValid.expect(false.B)
        for(w<-0 until 2)m.io.rows(w).expect(held(w).U)
      }
      m.io.read.bits.poke(0.U);m.io.read.valid.poke(true.B)
      m.io.trainRead.ready.expect(false.B);tick();m.io.respValid.expect(true.B)
      m.io.trainRespValid.expect(false.B)
      m.io.trainRead.valid.poke(false.B);m.io.read.valid.poke(false.B)
      // Update each neighbor and each way independently; metadata is immutable.
      for(set<-0 until 8;way<-0 until 2){
        m.io.read.valid.poke(false.B)
        val previous=expected(set)(way);val value=((set*3+way+2)&3)
        m.io.writeConf.bits.index.poke((set*2+way).U);m.io.writeConf.bits.conf.poke(value.U)
        m.io.writeConf.valid.poke(true.B);m.io.writeConf.ready.expect(true.B);tick();m.io.writeConf.valid.poke(false.B)
        expected=expected.updated(set,expected(set).updated(way,(previous & (~(BigInt(3)<<22)))|(BigInt(value)<<22)))
        for(neighbor<-0 until 8)read(neighbor)
        m.io.read.valid.poke(false.B)
        m.io.writeConf.bits.conf.poke(((previous>>22)&3).U);m.io.writeConf.valid.poke(true.B)
        m.io.writeConf.ready.expect(true.B);tick();m.io.writeConf.valid.poke(false.B)
        expected=expected.updated(set,expected(set).updated(way,previous))
        for(neighbor<-0 until 8)read(neighbor)
      }
      // An invalid response carries old held bits but must never claim a query.
      m.io.read.valid.poke(false.B);tick();m.io.respValid.expect(false.B)
      for(way<-0 until 2)m.io.rows(way).expect(expected(7)(way).U)
      for(clearRound<-0 until 2){
        m.io.clear.poke(true.B);m.io.read.valid.poke(true.B);m.io.read.bits.poke(7.U)
        m.io.writeConf.valid.poke(true.B)
        m.io.read.ready.expect(false.B);m.io.writeConf.ready.expect(false.B)
        tick();m.io.respValid.expect(false.B)
        m.io.clear.poke(false.B);m.io.read.valid.poke(false.B);m.io.writeConf.valid.poke(false.B)
        ready();expected=Vector.fill(8)(Vector.fill(2)(BigInt(0)))
        for(set<-0 until 8)read(set)
        m.io.read.valid.poke(false.B)
      }
      println(s"HINT_SRAM_CONTEXT_PORTS_PASS checks=$portChecks folded_neighbor_masks=16 true_read_priority=1 paired_reset=1")
      }
      {
        val m=new { val io=top.io.slot;val clock=top.clock;val reset=top.reset }
      m.io.clear.poke(false.B);m.io.read.valid.poke(false.B);m.io.read.bits.poke(0.U)
      m.io.load.valid.poke(false.B);m.io.load.bits.index.poke(0.U);m.io.load.bits.data.poke(0.U)
      m.reset.poke(true.B);m.clock.step(3);m.reset.poke(false.B)
      def ready():Unit={var n=0;while(!m.io.resetDone.peek().litToBoolean && n<20000){m.clock.step();n+=1};assert(m.io.resetDone.peek().litToBoolean)}
      ready()
      val targets=Vector.tabulate(32)(i=>(BigInt(1)<<46)|BigInt(0x1000+i*8))
      for(index<-0 until 32){
        m.io.load.bits.index.poke(index.U);m.io.load.bits.data.poke(targets(index).U)
        m.io.load.valid.poke(true.B);m.io.load.ready.expect(true.B);m.clock.step();m.io.load.valid.poke(false.B)
      }
      for(index<-Seq(0,31,1,30,0,7,8,16,31)){
        m.io.read.bits.poke(index.U);m.io.read.valid.poke(true.B);m.io.read.ready.expect(true.B)
        m.io.load.bits.index.poke(31.U);m.io.load.bits.data.poke(0x1234.U);m.io.load.valid.poke(true.B)
        m.io.load.ready.expect(false.B);m.clock.step()
        m.io.respValid.expect(true.B);m.io.readSlot.expect(index.U);m.io.target.expect(targets(index).U)
      }
      m.io.read.valid.poke(false.B);m.io.load.valid.poke(false.B)
      m.clock.step(3);m.io.respValid.expect(false.B);m.io.target.expect(targets(31).U)
      m.io.clear.poke(true.B);m.io.read.valid.poke(true.B)
      m.io.read.ready.expect(false.B);m.clock.step();m.io.respValid.expect(false.B)
      m.io.clear.poke(false.B);m.io.read.valid.poke(false.B);ready()
      for(index<-0 until 32){m.io.read.bits.poke(index.U);m.io.read.valid.poke(true.B);m.clock.step();m.io.readSlot.expect(index.U);m.io.target.expect(0.U)}
      println("HINT_SRAM_SLOT_PORTS_PASS width47=1 synchronous_identity=1 held_bits_invalid=1 cleared_all32=1")
      }
    }
  }
}
