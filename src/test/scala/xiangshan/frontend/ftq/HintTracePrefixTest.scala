package xiangshan.frontend.ftq

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan.frontend.bpu.hint.HintTraceResolved

class HintTracePrefixLane extends Bundle {
  val candidate=new HintTraceResolved
  val resolvedValid=Bool();val sameGeneration=Bool();val killed=Bool();val terminal=Bool()
  val knownMispredict=Bool();val cutoff=UInt(6.W)
  val candidatePosition=UInt(6.W);val previousPosition=UInt(6.W)
  val candidateCarry=Bool();val previousCarry=Bool()
}
/** Production prefix function, real 58-bit registered metadata and same-cycle lane chaining. */
class HintTracePrefixTestTop extends Module {
  val io=IO(new Bundle {
    val seed=Input(Valid(new HintTraceResolved));val applyLanes=Input(Bool())
    val lanes=Input(Vec(2,new HintTracePrefixLane))
    val meta=Output(new HintTraceResolved)
    val generationMatches=Input(Bool());val isCfi=Input(Bool());val needIttage=Input(Bool())
    val carry=Input(Bool());val knownMispredict=Input(Bool());val position=Input(UInt(6.W));val cutoff=Input(UInt(6.W))
    val diagnosticPredictedTaken=Input(Bool());val diagnosticQueryValid=Input(Bool())
    val commitAllowed=Output(Bool())
  })
  val saved=RegInit(0.U.asTypeOf(new HintTraceResolved))
  var updated:HintTraceResolved=saved
  for(l<-io.lanes){updated=HintTracePrefix.resolve(updated,l.candidate,l.resolvedValid,l.sameGeneration,
    l.killed,l.terminal,l.knownMispredict,l.cutoff,l.candidatePosition,l.previousPosition,l.candidateCarry,l.previousCarry)}
  when(io.seed.valid){saved:=io.seed.bits}.elsewhen(io.applyLanes){saved:=updated}
  io.meta:=saved
  io.commitAllowed:=HintTracePrefix.commitAllowed(saved,io.generationMatches,io.isCfi,io.needIttage,
    io.carry,io.knownMispredict,io.position,io.cutoff)
}

class HintTracePrefixTest extends AnyFlatSpec with ChiselSim {
  behavior of "Actual FTQ Hint trace prefix metadata"
  it should "update surviving target metadata before commit and reject conditional-cutoff suffixes" in {
    simulate(new HintTracePrefixTestTop){m=>
      def zero(x:Data):Unit=x match {
        case b:Bool=>b.poke(false.B)
        case u:UInt=>u.poke(0.U)
        case r:Record=>r.elements.values.foreach(zero)
        case v:Vec[_]=>v.getElements.foreach(zero)
        case _=>throw new IllegalArgumentException("prefix input")
      }
      zero(m.io.seed);zero(m.io.lanes);m.io.applyLanes.poke(false.B)
      m.io.generationMatches.poke(true.B);m.io.isCfi.poke(true.B);m.io.needIttage.poke(true.B)
      m.io.diagnosticPredictedTaken.poke(false.B);m.io.diagnosticQueryValid.poke(false.B)
      m.io.carry.poke(false.B);m.io.knownMispredict.poke(false.B);m.io.position.poke(21.U);m.io.cutoff.poke(14.U)
      m.reset.poke(true.B);m.clock.step(3);m.reset.poke(false.B)
      def seed(offset:Int=21,target:Int=0x3000,valid:Boolean=true,generation:Boolean=true):Unit={
        zero(m.io.lanes);m.io.applyLanes.poke(false.B)
        m.io.seed.valid.poke(true.B);m.io.seed.bits.valid.poke(valid.B)
        m.io.seed.bits.eligible.poke(valid.B);m.io.seed.bits.target.poke((target>>1).U)
        m.io.seed.bits.offset.poke(offset.U);m.io.seed.bits.generation.poke(generation.B)
        m.io.seed.bits.isRVC.poke(false.B);m.io.seed.bits.baselineMiss.poke(true.B)
        m.clock.step();m.io.seed.valid.poke(false.B)
      }
      def lane(i:Int,position:Int,terminal:Boolean,prefix:Boolean=true,cutoff:Int=14,
               same:Boolean=true,killed:Boolean=false,carry:Boolean=false,prevCarry:Boolean=false,
               target:Int=0x4000,previousPosition:Int=21):Unit={
        val l=m.io.lanes(i);zero(l)
        l.resolvedValid.poke(true.B);l.sameGeneration.poke(same.B);l.killed.poke(killed.B)
        l.terminal.poke(terminal.B);l.knownMispredict.poke(prefix.B);l.cutoff.poke(cutoff.U)
        l.candidatePosition.poke(position.U);l.previousPosition.poke(previousPosition.U)
        l.candidateCarry.poke(carry.B);l.previousCarry.poke(prevCarry.B)
        l.candidate.target.poke((target>>1).U);l.candidate.offset.poke(position.U)
        l.candidate.isRVC.poke(true.B);l.candidate.baselineMiss.poke(false.B)
        l.candidate.generation.poke(false.B)
      }
      def apply():Unit={m.io.applyLanes.poke(true.B);m.clock.step();m.io.applyLanes.poke(false.B)}
      def invalid():Unit={m.io.meta.valid.expect(false.B);m.io.meta.eligible.expect(false.B)
        m.io.meta.target.expect(0.U);m.io.meta.offset.expect(0.U);m.io.meta.generation.expect(true.B)}
      def retained():Unit={m.io.meta.valid.expect(true.B);m.io.meta.target.expect((0x3000>>1).U)
        m.io.meta.offset.expect(21.U);m.io.meta.baselineMiss.expect(true.B)}
      // Actual pickle/23 failure: earlier ordinary conditional resolves after a saved terminal.
      seed();lane(0,14,false);apply();invalid()
      // A later cycle's stale suffix cannot repopulate the cleared metadata.
      zero(m.io.lanes);lane(0,21,true);apply();invalid()
      zero(m.io.lanes);lane(0,22,true);apply();invalid()
      // Same-cycle ordering: suffix terminal then earlier conditional must clear target.
      seed(valid=false);lane(0,21,true,prefix=false);lane(1,14,false);apply();invalid()
      // Earlier cutoff first then suffix terminal must never repopulate metadata.
      seed();lane(0,14,false);lane(1,21,true);apply();invalid()
      // Target before or exactly at cutoff survives; metadata comes from actual candidate.
      for(position<-Seq(10,14)){
        seed();lane(0,position,true,target=0x5000);apply()
        m.io.meta.valid.expect(true.B);m.io.meta.target.expect((0x5000>>1).U)
        m.io.meta.offset.expect(position.U);m.io.meta.isRVC.expect(true.B)
        m.io.meta.baselineMiss.expect(false.B);m.io.meta.generation.expect(true.B)
      }
      // Unknown prefix retains a resolved target; wrong-generation and killed lanes cannot mutate it.
      for((same,killed)<-Seq((true,false),(false,false),(true,true))){
        seed();lane(0,14,false,prefix=false,same=same,killed=killed);apply();retained()
      }
      // A known cutoff doesn't kill an earlier saved terminal when a later event arrives.
      seed(offset=10);lane(0,21,true,previousPosition=10);apply()
      m.io.meta.valid.expect(true.B);m.io.meta.offset.expect(10.U);m.io.meta.target.expect((0x3000>>1).U)
      // Cross-block carry is not a surviving committed CFI, even with no known cutoff.
      seed();lane(0,21,true,prefix=false,carry=true,prevCarry=true);apply();invalid()
      // Recycled FTQ generation flag flips false→true→false. Old-generation
      // resolve cannot mutate the reset metadata; the current generation can.
      for(generation<-Seq(false,true,false)){
        seed(valid=false,generation=generation)
        lane(0,21,true,prefix=false,same=false,target=0x6000);apply()
        m.io.meta.valid.expect(false.B);m.io.meta.generation.expect(generation.B)
        zero(m.io.lanes);lane(0,21,true,prefix=false,same=true,target=0x7000);apply()
        m.io.meta.valid.expect(true.B);m.io.meta.target.expect((0x7000>>1).U)
        m.io.meta.generation.expect(generation.B)
      }
      // Valid CFI in the second 32B segment of a 64B block is not a carry.
      seed();m.io.knownMispredict.poke(false.B);m.io.position.poke(21.U)
      m.io.commitAllowed.expect(true.B)
      m.io.carry.poke(true.B);m.io.commitAllowed.expect(false.B);m.io.carry.poke(false.B)
      // Actual unknown frontend indirect and non-earliest CAM/query-invalid
      // contexts are legal trace events. Prediction flags are diagnostic only.
      for(predicted<-Seq(false,true);queryValid<-Seq(false,true)){
        seed(valid=false);m.io.diagnosticPredictedTaken.poke(predicted.B)
        m.io.diagnosticQueryValid.poke(queryValid.B)
        lane(0,21,true,prefix=false,target=0x8000);apply()
        m.io.meta.valid.expect(true.B);m.io.meta.target.expect((0x8000>>1).U)
        m.io.commitAllowed.expect(true.B)
      }
      // Commit gate is independently stricter than valid/target alone.
      seed();m.io.knownMispredict.poke(true.B);m.io.commitAllowed.expect(false.B)
      m.io.position.poke(14.U);m.io.commitAllowed.expect(true.B)
      for(field<-Seq(m.io.generationMatches,m.io.isCfi,m.io.needIttage)){
        field.poke(false.B);m.io.commitAllowed.expect(false.B);field.poke(true.B)
      }
      m.io.carry.poke(true.B);m.io.commitAllowed.expect(false.B)
      println("HINT_TRACE_PREFIX_REAL_RTL_PASS actual_metadata58_updated=1 both_lane_orders=1 conditional_cutoff=1 suffix_target_cleared=1 earliest_survivor_retained=1 generation_and_carry=1 late_suffix_multiple_cycles=1 flag_reuse=0,1,0 legal_second_segment=1 unknown_frontend_queryinvalid=1")
    }
  }
}
