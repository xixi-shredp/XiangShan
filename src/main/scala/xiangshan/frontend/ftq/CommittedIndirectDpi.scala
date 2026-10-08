package xiangshan.frontend.ftq

import chisel3._
import chisel3.experimental.ExtModule
import chisel3.util.HasExtModuleInline
import chisel3.util.HasBlackBoxInline

class HintBlockedDpi extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock = Input(Clock()); val blocked = Input(Bool())
    val lhr = Input(Bool()); val meta = Input(Bool())
  })
  // Expose aliases so the caller cannot accidentally connect a prediction input.
  def clock = io.clock
  def blocked = io.blocked
  def lhr = io.lhr
  def meta = io.meta
  setInline("HintBlockedDpi.sv", """
    |import "DPI-C" function void h2p_hint_blocked(input bit blocked, lhr, meta);
    |module HintBlockedDpi(input clock, blocked, lhr, meta);
    | always @(posedge clock) h2p_hint_blocked(blocked, lhr, meta);
    |endmodule
    |""".stripMargin)
}

class HintPendingDpi extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock = Input(Clock()); val pending = Input(UInt(32.W))
    val outstanding = Input(UInt(32.W)); val meta = Input(UInt(32.W))
  })
  def clock = io.clock
  def pending = io.pending
  def outstanding = io.outstanding
  def meta = io.meta
  setInline("HintPendingDpi.sv", """
    |import "DPI-C" function void h2p_hint_pending(input int unsigned pending, outstanding, meta);
    |module HintPendingDpi(input clock, input [31:0] pending, outstanding, meta);
    | always @(negedge clock) h2p_hint_pending(pending, outstanding, meta);
    |endmodule
    |""".stripMargin)
}

// Pure simulation observer: no outputs, prediction/state updates, or guard PC.
class CommittedIndirectDpi extends ExtModule with HasExtModuleInline {
  val clock = IO(Input(Clock()))
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val pc = Input(UInt(64.W))
    val actual = Input(UInt(64.W))
    val baselineTarget = Input(UInt(64.W))
    val baselineValid = Input(Bool())
    val finalTarget = Input(UInt(64.W))
    val finalValid = Input(Bool())
    val usedHint = Input(Bool())
    val metadataValid = Input(Bool())
    val ordinal = Input(UInt(64.W))
    val instr = Input(UInt(32.W))
    val ftqTag = Input(UInt(32.W))
    val offset = Input(UInt(32.W))
    val queryCycle = Input(UInt(64.W))
  })
  val cppExtModule = """
    |void CommittedIndirectDpi(uint8_t io_valid, uint64_t io_pc, uint64_t io_actual,
    | uint64_t io_baselineTarget, uint8_t io_baselineValid, uint64_t io_finalTarget,
    | uint8_t io_finalValid, uint8_t io_usedHint, uint8_t io_metadataValid,
     | uint64_t io_ordinal, uint32_t io_instr, uint32_t io_ftqTag, uint32_t io_offset, uint64_t io_queryCycle) {
    | if (io_valid) h2p_global_commit(io_pc, io_actual, io_baselineTarget, io_baselineValid,
    |   io_finalTarget, io_finalValid, io_usedHint, io_metadataValid, io_ordinal,
     |   io_instr, io_ftqTag, io_offset, io_queryCycle);
    |}
    |""".stripMargin
  difftest.DifftestModule.createCppExtModule("CommittedIndirectDpi", cppExtModule, Some("\"hint_runtime.h\""))
  setInline(s"$desiredName.v", """
    |import "DPI-C" function void h2p_global_commit(input longint pc, actual, baselineTarget,
    | input bit baselineValid, input longint finalTarget, input bit finalValid, usedHint, metadataValid,
     | input longint ordinal, input int unsigned instr, ftqTag, offset, input longint unsigned queryCycle);
    |module CommittedIndirectDpi(input clock, io_valid, input [63:0] io_pc, io_actual,
    | io_baselineTarget, input io_baselineValid, input [63:0] io_finalTarget,
    | input io_finalValid, io_usedHint, io_metadataValid, input [63:0] io_ordinal,
     | input [31:0] io_instr, io_ftqTag, io_offset, input [63:0] io_queryCycle);
    | always @(posedge clock) if (io_valid) h2p_global_commit(io_pc, io_actual,
    |   io_baselineTarget, io_baselineValid, io_finalTarget, io_finalValid, io_usedHint,
     |   io_metadataValid, io_ordinal, io_instr, io_ftqTag, io_offset, io_queryCycle);
    |endmodule
    |""".stripMargin)
}
