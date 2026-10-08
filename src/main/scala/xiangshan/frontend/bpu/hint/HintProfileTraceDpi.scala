package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.util.HasBlackBoxInline

/** Optional, bounded host-side diagnostic stream; never a predictor input. */
class HintProfileTraceDpi extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val valid = Input(Bool())
    val eventKind = Input(UInt(3.W))
    val cycle = Input(UInt(64.W))
    val queryCycle = Input(UInt(64.W))
    val ftqTag = Input(UInt(7.W))
    val offset = Input(UInt(6.W))
    val pc = Input(UInt(64.W))
    val target = Input(UInt(64.W))
    val mispredict = Input(Bool())
    val survived = Input(Bool())
    val observed = Input(Bool())
    val resolved = Input(Bool())
    val sourceBackend = Input(Bool())
    val eligible = Input(Bool())
  })
  setInline("HintProfileTraceDpi.sv", """
module HintProfileTraceDpi (
 input logic clock, valid,
 input logic [2:0] eventKind,
 input logic [63:0] cycle, queryCycle,
 input logic [6:0] ftqTag,
 input logic [5:0] offset,
 input logic [63:0] pc, target,
 input logic mispredict, survived, observed, resolved, sourceBackend, eligible
);
 import "DPI-C" function void h2p_hint_profile_trace(
   input int unsigned eventKind,
   input longint unsigned cycle, queryCycle,
   input int unsigned ftqTag, offset,
   input longint unsigned pc, target,
   input bit mispredict, survived, observed, resolved, sourceBackend, eligible);
 always @(posedge clock) if(valid)
   h2p_hint_profile_trace({29'b0,eventKind},cycle,queryCycle,
     {25'b0,ftqTag},{26'b0,offset},pc,target,
     mispredict,survived,observed,resolved,sourceBackend,eligible);
endmodule
""")
}
