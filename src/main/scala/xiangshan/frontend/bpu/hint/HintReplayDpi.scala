package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.util.HasBlackBoxInline

/** Simulation-only FTQ retirement replay sink; it is not a ROB ordinal. */
class HintReplayDpi extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val valid = Input(Bool())
    val ftqTag = Input(UInt(7.W))
    val queryCycle = Input(UInt(64.W))
    val ith = Input(UInt(28.W))
    val lhr = Input(UInt(28.W))
    val call = Input(UInt(28.W))
    val ret = Input(UInt(28.W))
    val mask = Input(UInt(4.W))
    val contextHash = Input(UInt(28.W))
    val pc = Input(UInt(64.W))
    val actual = Input(UInt(64.W))
    val baselineTarget = Input(UInt(64.W))
    val finalTarget = Input(UInt(64.W))
    val usedHint = Input(Bool())
    val contextHit = Input(Bool())
    val confAtPrediction = Input(UInt(2.W))
  })
  setInline("HintReplayDpi.sv", """
module HintReplayDpi (
  input logic clock, valid,
  input logic [6:0] ftqTag,
  input logic [63:0] queryCycle,
  input logic [27:0] ith, lhr, call, ret, contextHash,
  input logic [3:0] mask,
  input logic [63:0] pc, actual, baselineTarget, finalTarget,
  input logic usedHint, contextHit,
  input logic [1:0] confAtPrediction
);
  import "DPI-C" function void h2p_hint_replay(
    input longint unsigned pc, actual, baselineTarget, finalTarget,
    input bit usedHint, contextHit, input byte unsigned confAtPrediction,
    input int unsigned ftqTag, input longint unsigned queryCycle,
    input int unsigned ith, lhr, call, ret, mask, contextHash);
  always @(posedge clock) if (valid)
    h2p_hint_replay(pc, actual, baselineTarget, finalTarget,
                    usedHint, contextHit, {6'b0, confAtPrediction},
                    {25'b0, ftqTag}, queryCycle,
                    {4'b0, ith}, {4'b0, lhr}, {4'b0, call}, {4'b0, ret},
                    {28'b0, mask}, {4'b0, contextHash});
endmodule
""")
}
