package xiangshan.frontend.ftq

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan.frontend.bpu.hint.HintTraceResolved

class HintTracePrefixLane extends Bundle {
  val candidate = new HintTraceResolved
  val resolvedValid = Bool()
  val sameGeneration = Bool()
  val killed = Bool()
  val endsWindow = Bool()
  val terminal = Bool()
  val carry = Bool()
}

/** Existing 58-bit state; deliberately no performance metadata input. */
class HintTracePrefixTestTop extends Module {
  val io = IO(new Bundle {
    val seed = Input(Valid(new HintTraceResolved))
    val lanes = Input(Vec(2, new HintTracePrefixLane))
    val meta = Output(new HintTraceResolved)
    val commit = Output(Bool())
    val fresh = Output(Vec(2, Bool()))
    val generationMatches = Input(Bool())
    val carry = Input(Bool())
  })
  val saved = RegInit(0.U.asTypeOf(new HintTraceResolved))
  var next: HintTraceResolved = saved
  for ((lane, i) <- io.lanes.zipWithIndex) {
    val earlierBoundary = io.lanes.map(other => other.resolvedValid &&
      other.endsWindow && other.candidate.offset < lane.candidate.offset).reduce(_ || _)
    io.fresh(i) := HintTracePrefix.fresh(next, lane.candidate.offset,
      lane.sameGeneration, lane.killed, earlierBoundary)
    next = HintTracePrefix.resolve(next, lane.candidate, lane.resolvedValid,
      lane.sameGeneration, lane.killed, lane.endsWindow, lane.terminal, lane.carry)
  }
  saved := Mux(io.seed.valid, io.seed.bits, next)
  io.meta := saved
  io.commit := HintTracePrefix.commitAllowed(next, io.generationMatches, io.carry)
}

class HintTracePrefixTest extends AnyFlatSpec with ChiselSim {
  behavior of "FTQ actual prefix without performance metadata"
  it should "retain non-profiled boundaries and reject late suffixes in either lane order" in {
    simulate(new HintTracePrefixTestTop) { m =>
      def zero(x: Data): Unit = x match {
        case b: Bool => b.poke(false.B)
        case u: UInt => u.poke(0.U)
        case r: Record => r.elements.values.foreach(zero)
        case v: Vec[_] => v.getElements.foreach(zero)
        case _ => throw new IllegalArgumentException("trace prefix input")
      }
      zero(m.io.seed); zero(m.io.lanes)
      m.io.generationMatches.poke(true.B); m.io.carry.poke(false.B)
      m.reset.poke(true.B); m.clock.step(3); m.reset.poke(false.B)
      def seed(offset: Int = 21, valid: Boolean = true, eligible: Boolean = true): Unit = {
        zero(m.io.lanes); zero(m.io.seed)
        m.io.seed.valid.poke(true.B)
        m.io.seed.bits.valid.poke(valid.B); m.io.seed.bits.eligible.poke(eligible.B)
        m.io.seed.bits.offset.poke(offset.U); m.io.seed.bits.target.poke(0x3000.U)
        m.io.seed.bits.generation.poke(true.B)
        m.clock.step(); m.io.seed.valid.poke(false.B)
      }
      def lane(i: Int, offset: Int, terminal: Boolean, ends: Boolean = true): Unit = {
        val l = m.io.lanes(i); zero(l)
        l.resolvedValid.poke(true.B); l.sameGeneration.poke(true.B)
        l.endsWindow.poke(ends.B); l.terminal.poke(terminal.B)
        l.candidate.offset.poke(offset.U); l.candidate.target.poke(0x4000.U)
        l.candidate.isRVC.poke(true.B)
      }
      def boundary(offset: Int): Unit = {
        m.io.meta.valid.expect(true.B); m.io.meta.eligible.expect(false.B)
        m.io.meta.offset.expect(offset.U); m.io.meta.generation.expect(true.B)
        m.io.commit.expect(false.B)
      }
      // Earlier mispredicted conditional clears an already saved indirect.
      seed(); lane(0, 14, false); m.io.commit.expect(false.B); m.clock.step()
      zero(m.io.lanes); boundary(14)
      // Unlike a pulse or cleared-valid bit, this boundary survives many cycles.
      for (offset <- Seq(21, 22, 21)) {
        lane(0, offset, true); m.clock.step(); zero(m.io.lanes); boundary(14)
      }
      for (indirectLane <- Seq(0, 1)) {
        seed(valid = false)
        lane(indirectLane, 21, true); lane(1 - indirectLane, 14, false)
        m.io.fresh(indirectLane).expect(false.B)
        m.io.commit.expect(false.B); m.clock.step(); zero(m.io.lanes); boundary(14)
      }
      // Existing eligible terminal suppresses duplicate confidence updates.
      seed(); lane(0, 21, true); m.io.fresh(0).expect(false.B)
      // An earlier genuine terminal can supersede a conservative boundary.
      seed(14, eligible = false); lane(0, 10, true)
      m.io.commit.expect(true.B); m.clock.step(); zero(m.io.lanes)
      m.io.meta.eligible.expect(true.B); m.io.meta.offset.expect(10.U)
      m.io.meta.target.expect(0x4000.U); m.io.meta.generation.expect(true.B)
      // Exact-boundary actual indirect is allowed (non-self redirect semantics).
      seed(14, eligible = false); lane(0, 14, true); m.io.commit.expect(true.B)
      m.clock.step(); zero(m.io.lanes); m.io.meta.eligible.expect(true.B)
      // Non-ending ordinary conditional must not hide a later indirect.
      seed(); lane(0, 14, false, ends = false); m.clock.step(); zero(m.io.lanes)
      m.io.meta.offset.expect(21.U); m.io.commit.expect(true.B)
      // Later boundary cannot erase the true earlier terminal.
      seed(10); lane(0, 14, false); m.clock.step(); zero(m.io.lanes)
      m.io.meta.offset.expect(10.U); m.io.commit.expect(true.B)
      for (blocked <- Seq("generation", "killed", "carry")) {
        seed(valid = false); lane(0, 10, true)
        if (blocked == "generation") m.io.lanes(0).sameGeneration.poke(false.B)
        if (blocked == "killed") m.io.lanes(0).killed.poke(true.B)
        if (blocked == "carry") m.io.lanes(0).carry.poke(true.B)
        m.io.commit.expect(false.B); m.clock.step(); zero(m.io.lanes)
        m.io.meta.valid.expect(false.B)
      }
      seed(); m.io.generationMatches.poke(false.B); m.io.commit.expect(false.B)
      m.io.generationMatches.poke(true.B); m.io.carry.poke(true.B); m.io.commit.expect(false.B)
      println("HINT_TRACE_PREFIX_REAL_RTL_PASS perf_independent=1 both_lane_orders=1 persistent_cutoff=1 early_terminal=1 commit_bypass=1 generation_carry=1")
    }
  }
}
