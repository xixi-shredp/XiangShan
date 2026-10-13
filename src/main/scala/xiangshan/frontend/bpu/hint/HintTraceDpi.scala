package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.util.HasBlackBoxInline

/** DPIC transports complete queue records; it never participates in prediction. */
class HintTraceDpi extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val valid = Input(Bool())
    val data = Input(UInt(176.W))
    val discovery = Input(Bool())
    val ftqTag = Input(UInt(7.W))
    val queryCycle = Input(UInt(64.W))
    val overflow = Input(Bool())
    val occupancy = Input(UInt(5.W))
  })
  setInline("HintTraceDpi.sv", """
module HintTraceDpi(
  input logic clock, valid, discovery, overflow,
  input logic [175:0] data,
  input logic [6:0] ftqTag,
  input logic [63:0] queryCycle,
  input logic [4:0] occupancy
);
  import "DPI-C" function void h2p_hint_trace_sink(
    input bit valid, discovery, overflow,
    input longint unsigned data0, data1, data2,
    input int unsigned ftqTag, occupancy,
    input longint unsigned queryCycle);
  always @(posedge clock)
    h2p_hint_trace_sink(valid, discovery, overflow,
      data[63:0], data[127:64], {16'b0,data[175:128]},
      {25'b0,ftqTag}, {27'b0,occupancy}, queryCycle);
endmodule
""")
}
