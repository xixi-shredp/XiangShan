package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.util.HasBlackBoxInline

/** Simulation-only committed-event sink. */
class HintProfileDpi extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val valid = Input(Bool())
    val pc = Input(UInt(64.W))
    val actual = Input(UInt(64.W))
    val baselinePc = Input(UInt(64.W))
    val baselineTarget = Input(UInt(64.W))
    val baselineValid = Input(Bool())
    val blockStart = Input(UInt(64.W))
    val queryCycle = Input(UInt(64.W))
    val ith = Input(UInt(28.W))
    val lhr = Input(UInt(28.W))
    val call = Input(UInt(28.W))
    val ret = Input(UInt(28.W))
    val eligible = Input(Bool())
    val observed = Input(Bool())
  })
  setInline("HintProfileDpi.sv", """
module HintProfileDpi (
  input logic clock, valid,
  input logic [63:0] pc, actual, baselinePc, baselineTarget,
  input logic baselineValid,
  input logic [63:0] blockStart, queryCycle,
  input logic [27:0] ith, lhr, call, ret,
  input logic eligible, observed
);
  import "DPI-C" function void h2p_hint_record(
    input longint unsigned pc, actual, baselinePc, baselineTarget,
    input bit baselineValid,
    input longint unsigned blockStart, queryCycle,
    input int unsigned ith, lhr, call, ret,
    input bit eligible, observed);
  always @(posedge clock) if (valid)
    h2p_hint_record(pc, actual, baselinePc, baselineTarget,
                    baselineValid, blockStart, queryCycle,
                    {4'b0, ith}, {4'b0, lhr}, {4'b0, call}, {4'b0, ret},
                    eligible, observed);
endmodule
""")
}
