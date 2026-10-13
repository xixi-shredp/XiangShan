package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import top.BackendV2NoRVVNoLLCConfig
import xiangshan.{DebugOptionsKey,XSCoreParamsKey,XSTileKey}

/** Negative gate on the production 128-cell ring's 32-cell snapshot lifetime. */
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
  behavior of "Production ITH snapshot lifetime assertion"
  it should "accept 96 writes and reject the 97th instead of silently corrupting context" in {
    var reached96=false
    val error=intercept[Exception] {
      simulate(new HintSramTestTop,firtoolOpts=Array("-O=release",
        "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none")) { top=>
        initialize(top.io);top.reset.poke(true.B);top.clock.step(3);top.reset.poke(false.B)
        val io=top.io.hint
        var waited=0
        while(!io.tableReady.peek().litToBoolean && waited<20000){top.clock.step();waited+=1}
        assert(waited<20000,"actual SRAM initialization timeout")
        io.profile.poke(true.B)
        io.s1Fire.poke(true.B);top.clock.step();io.s1Fire.poke(false.B)
        io.s2Fire.poke(true.B);top.clock.step();io.s2Fire.poke(false.B)
        copyNow(io.s3Snapshot,io.traceCommit.bits.snapshot)
        io.s1Prediction.taken.poke(true.B);io.s1Prediction.attribute.branchType.poke(3.U)
        for(i<-0 until 96){
          io.s1Prediction.target.addr.poke((i&3).U)
          io.s1Fire.poke(true.B);top.clock.step();io.s1Fire.poke(false.B)
        }
        io.traceCommit.valid.poke(true.B);top.clock.step();io.traceCommit.valid.poke(false.B)
        reached96=true
        io.s1Fire.poke(true.B);top.clock.step();io.s1Fire.poke(false.B)
        io.traceCommit.valid.poke(true.B);top.clock.step()
      }
    }
    assert(reached96,"negative test failed before exercising the valid 96-write boundary")
    assert(error.getMessage.toLowerCase.contains("assert"),"expected production assertion failure, got: "+error)
    println("HINT_ITH96_97_PRODUCTION_ASSERT_PASS valid96=1 rejected97=1 hardware_assert_kept=1")
  }
}
