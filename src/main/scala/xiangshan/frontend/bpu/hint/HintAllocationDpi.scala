package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.util.HasBlackBoxInline

/** Simulation-only prediction generation identity. This observes S1 fire;
  * it does not claim that every generation survives to FTQ/ROB retirement.
  */
class HintAllocationDpi extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val valid = Input(Bool())
    val ftqTag = Input(UInt(7.W))
    val queryCycle = Input(UInt(64.W))
  })
  setInline("HintAllocationDpi.sv", """
module HintAllocationDpi(input logic clock, valid,
  input logic [6:0] ftqTag, input logic [63:0] queryCycle);
  import "DPI-C" function void h2p_hint_allocation(
    input int unsigned ftqTag, input longint unsigned queryCycle);
  always @(posedge clock) if (valid)
    h2p_hint_allocation({25'b0, ftqTag}, queryCycle);
endmodule
""")
}
