package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.util.RegEnable
import chisel3.simulator.scalatest.ChiselSim
import java.nio.file.Files
import org.scalatest.flatspec.AnyFlatSpec
import top.BackendV2NoRVVNoLLCConfig
import xiangshan.{XSCoreParamsKey, XSTileKey}

// One elaboration preserves the production SRAM Definition identity and
// connects the same normal-mode global control distribution used by Frontend.
class HintSramTestTop(classifyFinal: Boolean = false)(implicit p: org.chipsalliance.cde.config.Parameters) extends Module {
  val predictor=Module(new Hint)
  val contextTable=Module(new HintContextTable)
  val slotTable=Module(new HintSlotTable)
  val io=IO(new Bundle {
    val classification = Option.when(classifyFinal)(new Bundle {
      val ittageHit = Input(Bool())
      val ittageTarget = Input(xiangshan.frontend.GuardedPc())
      val btbTarget = Input(chisel3.util.Valid(xiangshan.frontend.GuardedPc()))
      val fallThroughTarget = Input(xiangshan.frontend.GuardedPc())
      val prediction = Output(new xiangshan.frontend.bpu.Prediction)
      val camSelected = Output(Bool())
      val usedHint = Output(Bool())
    })
    val hint=predictor.io.cloneType
    val context=contextTable.io.cloneType
    val slot=slotTable.io.cloneType
  })
  io.hint <> predictor.io
  if (classifyFinal) {
    val selection = Module(new HintCfiSelection)
    val c = io.classification.get
    selection.io.original := io.hint.s3Prediction
    selection.io.classify := predictor.io.s3Lookup.classify
    selection.io.position := predictor.io.s3Lookup.position
    selection.io.isCall := predictor.io.s3Lookup.isCall
    selection.io.ittageHit := c.ittageHit
    selection.io.ittageTarget := c.ittageTarget
    selection.io.btbTarget := c.btbTarget
    selection.io.fallThroughTarget := c.fallThroughTarget
    selection.io.hintUsable := predictor.io.s3Lookup.usable
    selection.io.hintTarget := xiangshan.frontend.GuardedPcInit(predictor.io.s3Lookup.target(predictor.GuardedVAddrBits - 1, 0))
    predictor.io.s3Prediction := selection.io.prediction
    predictor.io.s3UsedHint := selection.io.usedHint
    predictor.io.s3Override := io.hint.s3Override || (io.hint.s3Fire && HintPredictionDiff(selection.io.prediction, io.hint.s3Prediction).orR)
    val s2Start = RegEnable(io.hint.s1StartPc, io.hint.s1Fire)
    val s3Start = RegEnable(s2Start, io.hint.s2Fire)
    val position = selection.io.prediction.cfiPosition
    predictor.io.s3CfiPc := ((s3Start & ~31.U(64.W)) + (position << 1))
    c.prediction := selection.io.prediction
    c.camSelected := selection.io.camSelected
    c.usedHint := selection.io.usedHint
  }
  // Finite testbench stand-in for the FTQ association storage. All flush
  // boundaries use the production helper; no identity is placed in Hint.
  val owners = RegInit(VecInit(Seq.fill(predictor.FtqSize)(0.U.asTypeOf(new HintOwner))))
  predictor.io.owners := owners
  for (i <- 0 until predictor.FtqSize) {
    val owner = owners(i)
    when(owner.valid && (xiangshan.frontend.ftq.HintOwnership.killed(owner.ptr,
      io.hint.redirect.valid, io.hint.redirect.bits.newFtqIdx,
      predictor.io.s3Override, io.hint.s3Ptr) ||
      (io.hint.commit.valid && owner.ptr === io.hint.commit.bits))) {
      owner.valid := false.B
    }
  }
  when(io.hint.s1Fire) {
    val owner = owners(io.hint.s1Ptr.value)
    owner.valid := predictor.io.allocation.valid
    owner.ptr := io.hint.s1Ptr
    owner.index := predictor.io.allocation.bits
  }
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
  traceObserver.io.discoveryMissingStart := 0.U
  // Include the real diagnostic import in the small link preflight so its
  // generated DPI header checks the no-op test observer ABI before HintTest.
  xiangshan.DiagnosticDpi.emit(clock, reset, 0.U(64.W), 8, 0, true.B, Seq(io.input))
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
  // Link only no-return observers; actual loader has a separate test.
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

  private def fold(x: BigInt, width: Int): BigInt = {
    var value=x;var out=BigInt(0);val mask=(BigInt(1)<<width)-1
    while(value!=0){out^=value&mask;value>>=width};out
  }
  private def hash(x: BigInt): BigInt =
    (((fold(x,17)^((fold(x,16)<<2)&0x1ffff))<<11)|fold(x,11))

  behavior of "SPEC Hint exact-window predictor"
  it should "preserve exact CAM ownership, finite histories and resolve snapshot training" in {
    simulate(new HintSramTestTop(), firtoolOpts=Array("-O=release",
      "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none")) { top =>
      val io=top.io.hint
      def initialize(d:Data):Unit=d match {
        case r:Record=>r.elements.values.foreach(initialize)
        case v:Vec[_]=>v.getElements.foreach(initialize)
        case b:Bool if chisel3.reflect.DataMirror.directionOf(b)==ActualDirection.Input=>b.poke(false.B)
        case u:UInt if chisel3.reflect.DataMirror.directionOf(u)==ActualDirection.Input=>u.poke(0.U)
        case _=>()
      }
      val finalTargets = scala.collection.mutable.Map.empty[Int, BigInt]
      var currentId = 0
      def tick(n:Int=1):Unit=for(_<-0 until n){io.canAccept.expect(true.B);top.clock.step()}
      def ptr(p:xiangshan.frontend.ftq.FtqPtr,id:Int):Unit={p.flag.poke(((id&64)!=0).B);p.value.poke((id&63).U)}
      def copies(a:Data,b:Data):Seq[()=>Unit]=(a,b) match {
        case(x:Bool,y:Bool)=>val v=x.peek().litToBoolean;Seq(()=>y.poke(v.B))
        case(x:UInt,y:UInt)=>val v=x.peek().litValue;Seq(()=>y.poke(v.U))
        case(x:Record,y:Record)=>x.elements.toSeq.flatMap{case(k,v)=>copies(v,y.elements(k))}
        case(x:Vec[_],y:Vec[_])=>x.getElements.zip(y.getElements).flatMap{case(u,v)=>copies(u,v)}
        case _=>throw new IllegalArgumentException("snapshot field")
      }
      def reset():Unit={
        finalTargets.clear();initialize(top.io);top.reset.poke(true.B);top.clock.step(3);top.reset.poke(false.B)
        var n=0;while(!io.tableReady.peek().litToBoolean&&n<20000){tick();n+=1}
        assert(n<20000,"SRAM reset timeout")
      }
      def load(kind:Int,index:Int,data:BigInt):Unit={
        io.load.bits.kind.poke(kind.U);io.load.bits.index.poke(index.U);io.load.bits.data.poke(data.U)
        io.load.valid.poke(true.B);var n=0
        while(!io.load.ready.peek().litToBoolean&&n<20000){tick();n+=1}
        assert(n<20000,"image load timeout");tick();io.load.valid.poke(false.B)
        while(!io.tableReady.peek().litToBoolean&&n<20000){tick();n+=1}
        assert(n<20000,"image completion timeout")
      }
      def cam(index:Int,start:Int,mask:Int=0,position:Int=3):Unit=load(1,index,(BigInt(position)<<52)|(BigInt(1)<<51)|(BigInt(mask)<<47)|(start>>1))
      def row(start:Int,conf:Int,slot:Int=0):Unit={
        val key=hash(BigInt(start>>1));val set=(key&2047).toInt
        load(2,set*2,(BigInt(1)<<24)|(BigInt(conf)<<22)|(BigInt(slot)<<17)|(key>>11))
      }
      def image(conf:Int):Unit={reset();cam(0,0x1000);load(3,0,0x2000>>1);row(0x1000,conf);io.enabled.poke(true.B)}
      def prediction(p:xiangshan.frontend.bpu.Prediction,target:Int,taken:Boolean=true,kind:Int=3,ras:Int=0):Unit={
        p.taken.poke(taken.B);p.target.addr.poke((target>>1).U);p.attribute.branchType.poke(kind.U)
        p.attribute.rasAction.poke(ras.U);p.cfiPosition.poke(0.U)
      }
      case class Saved(redirect:Seq[()=>Unit],trace:Seq[()=>Unit],id:Int)
      def inputs(id:Int,start:Int,branch:Int,target:Int,kind:Int=3,ras:Int=0,taken:Boolean=true):Unit={
        currentId = id
        io.s1StartPc.poke(start.U);io.s1CfiPc.poke(branch.U);io.s3CfiPc.poke(branch.U)
        ptr(io.s1Ptr,id);ptr(io.s3Ptr,id)
        prediction(io.s1Prediction,target,taken,kind,ras);prediction(io.s3Prediction,target,taken,kind,ras)
        val position = ((branch - (start & ~31)) / 2) & 31
        io.s1Prediction.cfiPosition.poke(position.U);io.s3Prediction.cfiPosition.poke(position.U)
        io.s3BaselineTarget.poke(0x3000.U)
      }
      def query(id:Int,start:Int=0x1000,branch:Int=0x1006,target:Int=0x2000,
                kind:Int=3,ras:Int=0,taken:Boolean=true):Saved={
        inputs(id,start,branch,target,kind,ras,taken)
        io.s1Fire.poke(true.B);tick();io.s1Fire.poke(false.B)
        io.s2Fire.poke(true.B);tick();io.s2Fire.poke(false.B)
        Saved(copies(io.s3Snapshot,io.redirect.bits.meta.hint),copies(io.s3Snapshot,io.traceCommit.bits.snapshot),id)
      }
      def confirm():Unit={
        finalTargets(currentId) = if(io.s3Lookup.usable.peek().litToBoolean)
          io.s3Lookup.target.peek().litValue else io.s3BaselineTarget.peek().litValue
        io.s3UsedHint.poke(io.s3Lookup.usable.peek());io.s3Fire.poke(true.B);tick();io.s3Fire.poke(false.B);io.s3UsedHint.poke(false.B)}
      def commit(id:Int):Unit={ptr(io.commit.bits,id);io.commit.valid.poke(true.B);tick();io.commit.valid.poke(false.B);tick()}
      def resolve(id:Int,actual:Int,lane:Int=0,branch:Int=0x1006):Unit={
        val r=io.resolve(lane);ptr(r.bits.ftqPtr,id);r.bits.cfiPc.poke(branch.U);r.bits.target.poke(actual.U)
        r.bits.eligible.poke(true.B);r.bits.sourceBackend.poke(true.B);r.bits.fresh.poke(true.B)
        r.bits.baselineValid.poke(true.B);r.bits.baselineTarget.poke(finalTargets.getOrElse(id,BigInt(0x3000)).U);r.valid.poke(true.B)
      }
      def clearResolve():Unit=io.resolve.foreach(_.valid.poke(false.B))
      def train(id:Int,actual:Int):Unit={io.measureActive.poke(true.B);resolve(id,actual);tick();clearResolve();io.measureActive.poke(false.B);tick(3)}
      def confProbe(id:Int,expected:Int):Unit={for(old<-0 until id)commit(old);query(id);io.s3Lookup.contextHit.expect(true.B);io.debugSelectedConf.get.expect(expected.U);confirm();commit(id)}
      def profile(saved:Saved,start:Int,actualPc:Int):Vector[BigInt]={
        saved.trace.foreach(_());io.traceCommit.bits.ftqTag.poke(saved.id.U);io.traceCommit.bits.queryStartPc.poke(start.U)
        io.traceCommit.bits.actualPc.poke(actualPc.U);io.traceCommit.bits.actualTarget.poke(0x2000.U)
        io.traceCommit.bits.eligible.poke(true.B);io.traceCommit.bits.queryValid.poke(true.B)
        io.traceCommit.valid.poke(true.B);io.profileEvent.valid.expect(true.B)
        io.profileEvent.bits.queryValid.expect(true.B)
        val out=Vector(io.profileEvent.bits.ith,io.profileEvent.bits.lhr,io.profileEvent.bits.call,io.profileEvent.bits.ret).map(_.peek().litValue)
        io.traceCommit.valid.poke(false.B);out
      }

      // A 32b instruction's internal CFI is actual source+2. CAM uses neither.
      image(1);query(0,start=0xfe0);io.s3Lookup.candidate.expect(false.B);io.s3Lookup.usable.expect(false.B);confirm()
      query(1);io.s3Lookup.candidate.expect(true.B);io.s3Lookup.pc.expect(0x1006.U)
      io.s3Lookup.usable.expect(true.B);confirm();train(1,0x2000)
      io.stats.confAccepted.expect(1.U);io.stats.confApply.expect(1.U) // no commit yet
      confProbe(2,2);commit(1)
      println("HINT_EXACT_START_BRANCH_COORDINATES_RESOLVE_PASS")
      image(0);query(0,kind=0,taken=false);io.s3Lookup.contextHit.expect(true.B)
      io.s3Lookup.usable.expect(false.B);confirm();io.stats.recordAllocate.expect(1.U)
      io.measureActive.poke(true.B);resolve(0,0x3000);io.resolve(0).bits.baselineValid.poke(false.B)
      tick();clearResolve();io.measureActive.poke(false.B);tick(3)
      io.stats.confApply.expect(0.U);confProbe(1,0);commit(0)
      image(1);query(0,kind=1,taken=false);confirm();commit(0)
      io.stats.recordAllocate.expect(1.U);io.stats.confAccepted.expect(0.U)
      io.stats.lhrWrites.expect(0.U);io.tableReady.expect(true.B)
      println("HINT_LATE_IDENTIFIED_CFI_AND_EMPTY_WINDOW_RELEASE_PASS")

      // Both in-flight events use the SRAM snapshot=1. Last write is 2, not 3.
      image(1);query(10);confirm();query(11);confirm()
      io.measureActive.poke(true.B);resolve(10,0x2000,0);resolve(11,0x2000,1,0x1006);tick()
      clearResolve();io.measureActive.poke(false.B);tick(4)
      io.stats.confAccepted.expect(2.U);io.stats.confUp.expect(2.U);io.stats.confDown.expect(0.U)
      confProbe(12,2);commit(10);commit(11)
      println("HINT_MULTI_RESOLVE_SNAPSHOT_NO_RMW_PASS")
      for((initial,actual,expected)<-Seq((0,0x3000,0),(0,0x4000,1),(1,0x3000,0),(3,0x2000,3))){
        image(initial);query(0);io.s3Lookup.usable.expect((initial>0).B);confirm();train(0,actual);confProbe(1,expected);commit(0)
      }
      // Measure gates training only; warmup prediction still advances history.
      image(1);query(0);confirm();resolve(0,0x2000);tick();clearResolve();tick(3)
      io.stats.confAccepted.expect(0.U);confProbe(1,1);commit(0)
      println("HINT_CONF0_BASELINE_AND_SATURATION_MEASURE_PASS")
      image(1)
      for(id<-0 until 40){query(id);confirm();train(id,0x2000);commit(id)}
      io.stats.confAccepted.expect(40.U);io.stats.confApply.expect(40.U)
      io.debugConfHead.get.expect(40.U);io.debugConfTail.get.expect(40.U);confProbe(41,3)
      println("HINT_CONF32_PHYSICAL_POINTER_WRAP_PASS")

      // Context misses still allocate metadata and protect LHR. Ninth same CAM
      // falls back without touching LHR; every S1 remains acceptable.
      reset();cam(0,0x1000);cam(1,0x1100,position=8);cam(2,0x1200,position=8);io.enabled.poke(true.B)
      for(id<-0 until 8){query(id);io.s3Lookup.contextHit.expect(false.B);confirm()}
      io.stats.recordAllocate.expect(8.U);val writes=io.stats.lhrWrites.peek().litValue
      query(8);io.s3Lookup.admitted.expect(false.B);io.s3Lookup.usable.expect(false.B);io.s3Lookup.classify.expect(true.B);confirm()
      io.stats.lhrWrites.expect(writes.U);io.stats.recordAllocate.expect(8.U)
      for(id<-9 until 17){query(id,start=0x1100,branch=0x1110);confirm()}
      io.stats.recordAllocate.expect(16.U)
      query(17,start=0x1200,branch=0x1210);io.s3Lookup.admitted.expect(false.B);io.s3Lookup.classify.expect(true.B);confirm()
      io.stats.recordAllocate.expect(16.U)
      // Load waits for old records; disabling Hint cannot hide old transactions.
      io.enabled.poke(false.B);io.load.valid.poke(true.B);io.load.bits.kind.poke(1.U)
      io.load.bits.index.poke(0.U);io.load.bits.data.poke(0.U);io.load.ready.expect(false.B)
      for(id<-(0 until 8)++(9 until 17))commit(id)
      io.load.ready.expect(true.B);tick();io.load.valid.poke(false.B);io.tableReady.expect(true.B)
      println("HINT_CONTEXT_MISS_META16_LHR8_LOAD_DRAIN_PASS")

      // Back-to-back pipeline traffic: only first transaction reads SRAM;
      // second still gets an admitted LHR snapshot and metadata.
      image(1);inputs(0,0x1000,0x1006,0x2000);io.s1Fire.poke(true.B);tick()
      inputs(1,0x1000,0x1006,0x2000);io.s2Fire.poke(true.B);tick();io.s1Fire.poke(false.B)
      io.s3Lookup.contextHit.expect(true.B);ptr(io.s3Ptr,0);io.s3Fire.poke(true.B)
      tick();io.s2Fire.poke(false.B);ptr(io.s3Ptr,1)
      io.s3Lookup.contextHit.expect(false.B);io.s3Lookup.admitted.expect(true.B);tick();io.s3Fire.poke(false.B)
      io.stats.recordAllocate.expect(2.U);io.stats.lookupPortFallback.expect(1.U)
      println("HINT_S1_S3_BUSY_FALLBACK_PASS")
      image(1);query(0)
      // S3 is stalled while a younger S1 reservation reaches S2.
      inputs(1,0x1000,0x1006,0x2000);io.s1Fire.poke(true.B);tick();io.s1Fire.poke(false.B)
      ptr(io.s3Ptr,0);io.s2Fire.poke(true.B);io.s3Fire.poke(true.B);io.s3Override.poke(true.B)
      tick();io.s2Fire.poke(false.B);io.s3Fire.poke(false.B);io.s3Override.poke(false.B)
      commit(0);io.enabled.poke(false.B);io.tableReady.expect(true.B)
      load(1,0,(BigInt(3)<<52)|(BigInt(1)<<51)|(0x1000>>1))
      println("HINT_S3_FLUSH_YOUNGER_S2_RESERVATION_DRAIN_PASS")

      // Independent shift-register golden, exercising high history bits,
      // circular wrap, local 16b reads and Call/Ret final-halfword hashing.
      reset();cam(0,0x1000);io.profile.poke(true.B)
      var ith=BigInt(0);var call=BigInt(0);var ret=BigInt(0);var local=BigInt(0)
      val mask128=(BigInt(1)<<128)-1
      for(n<-0 until 192){
        val target=0x2000+2*(n%4);val branch=if(n%2==0)0x1046 else 0x1044
        val kind=if(n%3==1)2 else 3
        val ras=if(n%3==1)2 else if(n%3==2)1 else 0
        query(n,start=0x1800,branch=branch,target=target,kind=kind,ras=ras);confirm()
        if(kind==3&&ras==0)ith=((ith<<2)|((target>>1)&3))&mask128
        if(ras==2)call=((call<<2)^fold(BigInt(branch>>1),14))&mask128
        if(ras==1)ret=((ret<<2)^fold(BigInt(branch>>1),14))&mask128
      }
      for(n<-0 until 24){
        val target=0x2000+2*(n%4);val before=Vector(hash(ith),hash(local),hash(call),hash(ret))
        val saved=query(n,start=0x1000,branch=0x1006,target=target)
        assert(profile(saved,0x1000,0x1004)==before)
        confirm();commit(n)
        ith=((ith<<2)|((target>>1)&3))&mask128;local=((local<<2)|((target>>1)&3))&0xffff
      }
      // Snapshot before a call must reconstruct the pre-XOR low twelve bits.
      val before=Vector(hash(ith),hash(local),hash(call),hash(ret))
      val callSnapshot=query(30,branch=0x1006,target=0x2000,kind=2,ras=2)
      assert(profile(callSnapshot,0x1000,0x1004)==before)
      confirm();commit(30)
      call=((call<<2)^fold(BigInt(0x1006>>1),14))&mask128
      val frozen=Vector(hash(ith),hash(local),hash(call),hash(ret))
      val protectedSnapshot=query(31);confirm()
      for(n<-0 until 63){
        query(n+32,start=0x1800,branch=if(n%2==0)0x1846 else 0x1844,kind=3,ras=3);confirm()
      }
      assert(profile(protectedSnapshot,0x1000,0x1004)==frozen,
        "63 live younger Call/Ret windows corrupted the old snapshot")
      protectedSnapshot.redirect.foreach(_());ptr(io.redirect.bits.newFtqIdx,31)
      io.redirect.bits.startPc.addr.poke((0x1000>>1).U);io.redirect.bits.cfiPc.addr.poke((0x1006>>1).U)
      io.redirect.bits.target.addr.poke((0x2000>>1).U);io.redirect.bits.taken.poke(false.B)
      io.redirect.bits.attribute.branchType.poke(1.U);io.redirect.bits.attribute.rasAction.poke(0.U)
      io.redirect.bits.reapply.poke(false.B);io.redirect.bits.flushItself.poke(false.B)
      io.redirect.valid.poke(true.B);tick();io.redirect.valid.poke(false.B)
      val restored=query(32);assert(profile(restored,0x1000,0x1004)==frozen)
      confirm();commit(32);ith=(ith<<2)&mask128;local=(local<<2)&0xffff
      println("HINT_CALL_RET_LIVE_FTQ_WINDOW_PROTECTION_AND_RESTORE_PASS")
      io.profile.poke(false.B)
      for(mask<-0 until 16){
        cam(0,0x1000,mask);io.enabled.poke(true.B)
        val expected=hash(BigInt(0x1000>>1)) ^
          (if((mask&1)!=0)hash(ith) else BigInt(0)) ^
          (if((mask&2)!=0)hash(call) else BigInt(0)) ^
          (if((mask&4)!=0)hash(ret) else BigInt(0)) ^
          (if((mask&8)!=0)hash(local) else BigInt(0))
        query(mask,target=0x2006)
        // Lookup set is a functional signal derived from the exact S1 key;
        // replay/hash copies no longer exist in the production metadata.
        io.s3Lookup.set.expect((expected & 2047).U)
        confirm();resolve(mask,0x2006);tick();clearResolve()
        ptr(io.commit.bits,mask);io.commit.valid.poke(true.B)
        tick();io.commit.valid.poke(false.B);tick();io.enabled.poke(false.B)
        ith=((ith<<2)|3)&mask128;local=((local<<2)|3)&0xffff
      }
      println("HINT_GLOBAL128_LOCAL16_RING_PHR_COORDINATES_ALL_MASK_PASS")
      load(0,0,0);cam(0,0x1000);io.profile.poke(true.B)
      val reloaded=query(40)
      assert(profile(reloaded,0x1000,0x1004)==Vector(hash(ith),BigInt(0),hash(call),hash(ret)),
        "table Clear must retain global histories and reset only CAM/LHR")
      confirm();commit(40)
      println("HINT_LOAD_PRESERVE_GLOBAL_RESET_LOCAL_PASS")

      // LHR checkpoints live only in the sixteen-entry S1 queue. A redirect
      // at a non-CAM FTQ selects the earliest younger checkpoint, including a
      // branch whose S1 has not yet reached S3. No successor means no local
      // append occurred and the current heads are already the correct state.
      reset();cam(0,0x1000);io.profile.poke(true.B)
      def rewind(saved:Saved,id:Int):Unit={
        saved.redirect.foreach(_());ptr(io.redirect.bits.newFtqIdx,id)
        io.redirect.bits.startPc.addr.poke((0x1800>>1).U)
        io.redirect.bits.cfiPc.addr.poke((0x1800>>1).U);io.redirect.bits.target.addr.poke((0x1800>>1).U)
        io.redirect.bits.taken.poke(false.B);io.redirect.bits.attribute.branchType.poke(1.U)
        io.redirect.bits.attribute.rasAction.poke(0.U);io.redirect.bits.reapply.poke(false.B)
        io.redirect.bits.flushItself.poke(false.B);io.redirect.valid.poke(true.B);tick();io.redirect.valid.poke(false.B)
      }
      query(0,target=0x2002);confirm();commit(0) // Local history = 1.
      val anchor=query(1,start=0x1800,branch=0x1800,kind=1,taken=false);confirm()
      query(2,target=0x2004);confirm();query(3,target=0x2006);confirm()
      rewind(anchor,1)
      val first=query(2);assert(profile(first,0x1000,0x1004)(1)==hash(BigInt(1)));confirm();commit(2)
      // The previous probe appended zero: Local history = 4.
      val noSuccessor=query(3,start=0x1800,branch=0x1800,kind=1,taken=false);confirm();rewind(noSuccessor,3)
      val second=query(4);assert(profile(second,0x1000,0x1004)(1)==hash(BigInt(4)));confirm();commit(4)
      // Current local history = 16. Only S1 of the younger H2P is allowed to run.
      val early=query(5,start=0x1800,branch=0x1800,kind=1,taken=false);confirm()
      inputs(6,0x1000,0x1006,0x2006);io.s1Fire.poke(true.B);tick();io.s1Fire.poke(false.B)
      rewind(early,5)
      val third=query(6);assert(profile(third,0x1000,0x1004)(1)==hash(BigInt(16)));confirm();commit(6)
      val onCommit=query(7,target=0x2004);confirm()
      onCommit.trace.foreach(_());io.traceCommit.bits.queryStartPc.poke(0x1000.U)
      io.traceCommit.bits.actualPc.poke(0x1004.U);io.traceCommit.bits.actualTarget.poke(0x2004.U)
      io.traceCommit.bits.eligible.poke(true.B);io.traceCommit.bits.queryValid.poke(true.B)
      // Trace FTQ tag is the queue lookup identity, including the generation bit.
      io.traceCommit.bits.ftqTag.poke(7.U);io.traceCommit.valid.poke(true.B)
      ptr(io.commit.bits,7);io.commit.valid.poke(true.B)
      io.profileEvent.bits.queryValid.expect(true.B);io.profileEvent.bits.lhr.expect(hash(BigInt(64)).U)
      tick();io.commit.valid.poke(false.B);io.traceCommit.valid.poke(false.B);tick()
      io.tableReady.expect(true.B)
      println("HINT_META16_EARLIEST_YOUNGER_CURRENT_HEAD_S1_ONLY_COMMIT_TRACE_PASS")
      // Put the earliest younger checkpoint in physical slot 15 and the next
      // one in slot 0: a physical-index priority encoder would choose wrongly.
      for(id<-8 until 18){query(id,target=0x2002);confirm();commit(id)}
      val wrapAnchor=query(18,start=0x1800,branch=0x1800,kind=1,taken=false);confirm()
      query(19,target=0x2004);confirm();query(20,target=0x2006);confirm();rewind(wrapAnchor,18)
      val wrapped=query(19);assert(profile(wrapped,0x1000,0x1004)(1)==hash(BigInt(0x5555)))
      confirm();commit(19)
      println("HINT_META16_EARLIEST_YOUNGER_PHYSICAL_WRAP_PASS")
    }
  }
}
