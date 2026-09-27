package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.util.HasBlackBoxInline

/** Simulation-only committed replay outcome sink. */
class HintReplayDpi extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val valid = Input(Bool())
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
  input logic [63:0] pc, actual, baselineTarget, finalTarget,
  input logic usedHint, contextHit,
  input logic [1:0] confAtPrediction
);
  import "DPI-C" function void h2p_hint_replay(
    input longint unsigned pc, actual, baselineTarget, finalTarget,
    input bit usedHint, contextHit, input byte unsigned confAtPrediction);
  always @(posedge clock) if (valid)
    h2p_hint_replay(pc, actual, baselineTarget, finalTarget,
                    usedHint, contextHit, {6'b0, confAtPrediction});
endmodule
""")
}
