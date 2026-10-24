package xiangshan.frontend.ftq

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.cde.config.Parameters
import top.BackendV2NoRVVNoLLCConfig
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.frontend.bpu.hint.HintTraceResolved

class HintTraceNextWrite(implicit p: Parameters) extends Bundle {
  val valid = Bool()
  val killed = Bool()
  val ptr = new FtqPtr
  val meta = new HintTraceResolved
}

/** The same arbitration and generation qualification used by production FTQ. */
class HintTraceNextStateTestTop(implicit p: Parameters) extends Module {
  private val entries = p(XSCoreParamsKey).frontendParameters.ftqParameters.FtqSize
  val io = IO(new Bundle {
    val s3 = Input(Valid(new HintTraceNextWrite))
    val lanes = Input(Vec(2, new HintTraceNextWrite))
    val oldest = Input(new FtqPtr)
    val newest = Input(new FtqPtr)
    val redirectValid = Input(Bool())
    val redirectNewPtr = Input(new FtqPtr)
    val redirectPtr = Input(new FtqPtr)
    val redirectOffset = Input(UInt(6.W))
    val flushItself = Input(Bool())
    val index = Input(UInt(log2Ceil(entries).W))
    val commit = Output(new HintTraceResolved)
    val saved = Output(new HintTraceResolved)
  })
  val saved = RegInit(VecInit(Seq.fill(entries)(0.U.asTypeOf(new HintTraceResolved))))
  val base = WireInit(saved)
  when(io.s3.valid) {
    base(io.s3.bits.ptr.value) := io.s3.bits.meta
    base(io.s3.bits.ptr.value).generation := io.s3.bits.ptr.flag
  }
  val writes = io.lanes.map { l =>
    val valid = l.valid && !l.killed &&
      HintTraceNextState.generationMatches(base(l.ptr.value), l.ptr, io.oldest, io.newest)
    (valid, l.ptr.value, l.meta)
  }
  val next = Wire(chiselTypeOf(saved))
  for (i <- 0 until entries) {
    next(i) := HintTraceNextState(base(i), i.U, writes,
      io.redirectValid, io.redirectNewPtr, io.redirectPtr, io.redirectOffset, io.flushItself)
  }
  saved := next
  io.commit := next(io.index)
  io.saved := saved(io.index)

}

class HintTraceNextStateTest extends AnyFlatSpec with ChiselSim {
  private val config = new BackendV2NoRVVNoLLCConfig
  implicit val params: Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
  }
  behavior of "FTQ trace storage and same-cycle commit arbitration"
  it should "apply S3 then valid lanes then redirect to the same storage and commit value" in {
    simulate(new HintTraceNextStateTestTop) { m =>
      def zero(x: Data): Unit = x match {
        case b: Bool => b.poke(false.B)
        case u: UInt => u.poke(0.U)
        case r: Record => r.elements.values.foreach(zero)
        case v: Vec[_] => v.getElements.foreach(zero)
        case _ => throw new IllegalArgumentException("trace next-state input")
      }
      def ptr(p: FtqPtr, value: Int, flag: Boolean = false): Unit = {
        p.value.poke(value.U); p.flag.poke(flag.B)
      }
      zero(m.io.s3); zero(m.io.lanes)
      ptr(m.io.oldest, 0); ptr(m.io.newest, 20)
      ptr(m.io.redirectPtr, 3); ptr(m.io.redirectNewPtr, 4)
      m.io.redirectValid.poke(false.B); m.io.redirectOffset.poke(14.U)
      m.io.flushItself.poke(false.B); m.io.index.poke(3.U)
      m.reset.poke(true.B); m.clock.step(3); m.reset.poke(false.B)
      def seed(offset: Int, flag: Boolean = false): Unit = {
        zero(m.io.lanes); m.io.redirectValid.poke(false.B)
        m.io.s3.valid.poke(true.B); ptr(m.io.s3.bits.ptr, 3, flag)
        m.io.s3.bits.meta.valid.poke(true.B); m.io.s3.bits.meta.eligible.poke(true.B)
        m.io.s3.bits.meta.offset.poke(offset.U); m.io.s3.bits.meta.target.poke(0x3000.U)
        m.clock.step(); m.io.s3.valid.poke(false.B)
      }
      def lane(i: Int, offset: Int, target: Int, flag: Boolean = false): Unit = {
        val l = m.io.lanes(i); zero(l); l.valid.poke(true.B); ptr(l.ptr, 3, flag)
        l.meta.valid.poke(true.B); l.meta.eligible.poke(true.B)
        l.meta.offset.poke(offset.U); l.meta.target.poke(target.U); l.meta.generation.poke(flag.B)
      }
      // Regression: old suffix offset21 must not invalidate the new surviving offset10.
      seed(21); lane(0, 10, 0x4000); m.io.redirectValid.poke(true.B)
      m.io.commit.valid.expect(true.B); m.io.commit.offset.expect(10.U)
      m.io.commit.target.expect(0x4000.U); m.io.saved.offset.expect(21.U)
      m.clock.step(); m.io.saved.valid.expect(true.B); m.io.saved.offset.expect(10.U)
      // New candidate, rather than old offset, also controls suffix rejection.
      seed(10); lane(0, 21, 0x5000); m.io.redirectValid.poke(true.B)
      m.io.commit.valid.expect(true.B); m.io.commit.eligible.expect(false.B)
      m.io.commit.offset.expect(14.U); m.clock.step(); m.io.saved.eligible.expect(false.B)
      // Metadata uses the final matching cumulative lane, before the clock edge.
      seed(21); lane(0, 12, 0x6000); lane(1, 10, 0x7000)
      m.io.commit.target.expect(0x7000.U)
      m.io.lanes(1).killed.poke(true.B); m.io.commit.target.expect(0x6000.U)
      m.io.lanes(1).killed.poke(false.B); ptr(m.io.lanes(1).ptr, 3, true)
      m.io.commit.target.expect(0x6000.U)
      m.io.lanes(1).valid.poke(false.B)
      // S3 generation initialization wins over old residency before resolve qualification.
      seed(21); zero(m.io.lanes)
      ptr(m.io.oldest, 0, true); ptr(m.io.newest, 20, true)
      m.io.s3.valid.poke(true.B); ptr(m.io.s3.bits.ptr, 3, true)
      m.io.s3.bits.meta.valid.poke(false.B); m.io.s3.bits.meta.generation.poke(false.B)
      lane(0, 10, 0x8000); m.io.commit.valid.expect(false.B)
      ptr(m.io.lanes(0).ptr, 3, true); m.io.lanes(0).meta.generation.poke(true.B)
      m.io.commit.valid.expect(true.B); m.io.commit.generation.expect(true.B)
      m.io.commit.target.expect(0x8000.U)
      m.clock.step(); m.io.saved.target.expect(0x8000.U)
      // Equal-offset flush-self and full younger-entry invalidation.
      m.io.s3.valid.poke(false.B); zero(m.io.lanes)
      ptr(m.io.redirectPtr, 3, true); ptr(m.io.redirectNewPtr, 4, true)
      m.io.redirectValid.poke(true.B); m.io.redirectOffset.poke(10.U)
      m.io.flushItself.poke(false.B); m.io.commit.valid.expect(true.B)
      m.io.flushItself.poke(true.B); m.io.commit.valid.expect(true.B)
      m.io.commit.eligible.expect(false.B); m.io.commit.offset.expect(9.U)
      m.io.flushItself.poke(false.B); ptr(m.io.redirectNewPtr, 3, true)
      m.io.commit.valid.expect(false.B)
      // Offset zero self-flush removes the whole FTQ: no wrapped cutoff 63.
      ptr(m.io.redirectNewPtr, 3, true); m.io.redirectOffset.poke(0.U)
      m.io.flushItself.poke(true.B); m.io.commit.valid.expect(false.B)
      // Empty target entry also retains a redirect cutoff, not just terminals.
      m.io.redirectValid.poke(false.B); m.io.s3.valid.poke(true.B)
      ptr(m.io.s3.bits.ptr, 3, true); m.io.s3.bits.meta.valid.poke(false.B)
      m.clock.step(); m.io.s3.valid.poke(false.B)
      ptr(m.io.redirectNewPtr, 4, true); m.io.redirectOffset.poke(14.U)
      m.io.flushItself.poke(false.B); m.io.redirectValid.poke(true.B)
      m.io.commit.valid.expect(true.B); m.io.commit.eligible.expect(false.B)
      m.io.commit.offset.expect(14.U)
      m.clock.step(); m.io.redirectValid.poke(false.B)
      m.io.saved.offset.expect(14.U); m.io.saved.eligible.expect(false.B)
      println("HINT_TRACE_NEXT_STATE_REAL_RTL_PASS storage_commit_same_value=1 redirect_uses_new_offset=1 last_valid_lane=1 perf_independent=1 s3_generation=1 flush_self=1")
    }
  }
}
