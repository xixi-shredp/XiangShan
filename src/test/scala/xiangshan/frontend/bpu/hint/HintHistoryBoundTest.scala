package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import top.BackendV2NoRVVNoLLCConfig
import xiangshan.{DebugOptionsKey,XSCoreParamsKey,XSTileKey}

/** Preserve the oldest supported snapshot without hardware lifetime counters. */
class HintHistoryBoundTest extends AnyFlatSpec with ChiselSim {
  private val config=new BackendV2NoRVVNoLLCConfig
  implicit val params:org.chipsalliance.cde.config.Parameters=config.alterPartial {
    case XSCoreParamsKey=>config(XSTileKey).head
    case DebugOptionsKey=>config(DebugOptionsKey).copy(FPGAPlatform=true)
  }
  override implicit def commonSettingsModifications:svsim.CommonSettingsModifications=
    new svsim.CommonSettingsModifications {
      override def apply(s:svsim.CommonCompilationSettings):svsim.CommonCompilationSettings=
        s.copy(optimizationStyle=svsim.CommonCompilationSettings.OptimizationStyle.OptimizeForCompilationSpeed)
    }
  override implicit def backendSettingsModifications:svsim.BackendSettingsModifications=
    new svsim.BackendSettingsModifications {
      override def apply(s:svsim.Backend.Settings):svsim.Backend.Settings=s match {
        case v:svsim.verilator.Backend.CompilationSettings=>v.withOutputSplit(Some(20000)).withOutputSplitCFuncs(Some(2000))
        case x=>x
      }
    }
  private def initialize(d:Data):Unit=d match {
    case r:Record=>r.elements.values.foreach(initialize)
    case v:Vec[_]=>v.getElements.foreach(initialize)
    case b:Bool if DataMirror.directionOf(b)==ActualDirection.Input=>b.poke(false.B)
    case u:UInt if DataMirror.directionOf(u)==ActualDirection.Input=>u.poke(0.U)
    case _=>()
  }
  private def copyNow(a:Data,b:Data):Unit=(a,b) match {
    case(x:Bool,y:Bool)=>y.poke(x.peek())
    case(x:UInt,y:UInt)=>y.poke(x.peek())
    case(x:Record,y:Record)=>x.elements.foreach{case(k,v)=>copyNow(v,y.elements(k))}
    case(x:Vec[_],y:Vec[_])=>x.getElements.zip(y.getElements).foreach{case(u,v)=>copyNow(u,v)}
    case _=>throw new IllegalArgumentException("history snapshot field")
  }
  behavior of "Production ITH snapshot capacity"
  it should "retain the original 128-bit context across 64 later appends using only a 45-bit snapshot" in {
      simulate(new HintSramTestTop(),firtoolOpts=Array("-O=release",
        "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none")) { top=>
        initialize(top.io);top.reset.poke(true.B);top.clock.step(3);top.reset.poke(false.B)
        val io=top.io.hint
        var waited=0
        while(!io.tableReady.peek().litToBoolean && waited<20000){top.clock.step();waited+=1}
        assert(waited<20000,"actual SRAM initialization timeout")
        assert(io.s3Snapshot.getWidth==45,"global snapshot must contain only heads and Call/Ret low windows")
        assert(io.s3Snapshot.elements.keySet==Set("ithHead","callHead","retHead","callLow12","retLow12"),
          "global snapshot must not retain lifetime counters or duplicate targets")
        io.profile.poke(true.B)
        io.s1Prediction.taken.poke(true.B);io.s1Prediction.attribute.branchType.poke(3.U)
        // Seed a nonzero context, including the oldest protected sample. The
        // second group writes the disjoint half of the physical 128-cell ring.
        for(i<-0 until 64){
          io.s1Prediction.target.addr.poke((if(i==0) 1 else 0).U)
          io.s1Fire.poke(true.B);top.clock.step();io.s1Fire.poke(false.B)
        }
        io.s1Prediction.taken.poke(false.B)
        io.s1Fire.poke(true.B);top.clock.step();io.s1Fire.poke(false.B)
        io.s2Fire.poke(true.B);top.clock.step();io.s2Fire.poke(false.B)
        copyNow(io.s3Snapshot,io.traceCommit.bits.snapshot)
        val originalContext=io.profileEvent.bits.ith.peek().litValue
        assert(originalContext!=0,"test seed must produce a nonzero protected context")
        io.s1Prediction.taken.poke(true.B)
        for(i<-0 until 64){
          io.s1Prediction.target.addr.poke(0.U)
          io.s1Fire.poke(true.B);top.clock.step();io.s1Fire.poke(false.B)
        }
        io.profileEvent.bits.ith.expect(originalContext.U)
        // A 65th unretired append is outside the FTQ capacity contract. This
        // component test deliberately does not invent an extra hardware guard.
      }
    println("HINT_ITH64_CAPACITY_PASS snapshot_bits=45 later_appends=64 lifetime_counters=0")
  }
}
