package xiangshan

import chisel3._
import chisel3.util.{Cat, HasBlackBoxInline}

/** Simulation sink. It has no return/control signals and no production backpressure. */
class DiagnosticDpi extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val valid = Input(Bool())
    val kind = Input(UInt(32.W))
    val lane = Input(UInt(32.W))
    val cycle = Input(UInt(64.W))
    val w0 = Input(UInt(64.W))
    val w1 = Input(UInt(64.W))
    val w2 = Input(UInt(64.W))
    val w3 = Input(UInt(64.W))
    val w4 = Input(UInt(64.W))
    val w5 = Input(UInt(64.W))
    val w6 = Input(UInt(64.W))
    val w7 = Input(UInt(64.W))
    val w8 = Input(UInt(64.W))
    val w9 = Input(UInt(64.W))
    val w10 = Input(UInt(64.W))
    val w11 = Input(UInt(64.W))
    val w12 = Input(UInt(64.W))
    val w13 = Input(UInt(64.W))
    val w14 = Input(UInt(64.W))
    val w15 = Input(UInt(64.W))
  })
  setInline("DiagnosticDpi.sv", """
module DiagnosticDpi(input logic clock, valid,
 input logic [31:0] kind, lane, input logic [63:0] cycle,
 input logic [63:0] w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15);
 import "DPI-C" function void diagnostic_event(input int unsigned kind, lane,
  input longint unsigned cycle, w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15);
 always @(posedge clock) if(valid) diagnostic_event(kind,lane,cycle,w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15);
endmodule
""")
}

object DiagnosticDpi {
  def cycle(): UInt = { val c=RegInit(0.U(64.W)); c := c+1.U; c }
  def emit(clock: Clock, reset: Reset, cycle: UInt, kind: Int, lane: Int, valid: Bool,
      fields: Seq[UInt]): Unit = {
    require(fields.length <= 16)
    fields.foreach(f => require(f.getWidth > 0 && f.getWidth <= 64,
      s"diagnostic v1 field width ${f.getWidth} exceeds its fixed uint64 payload"))
    def word(x: UInt): UInt = if (x.getWidth == 64) x else Cat(0.U((64 - x.getWidth).W), x)
    val dpi=Module(new DiagnosticDpi)
    dpi.io.clock := clock
    dpi.io.valid := valid && !reset.asBool
    dpi.io.kind := kind.U
    dpi.io.lane := lane.U
    dpi.io.cycle := cycle
    dpi.io.w0 := (if (fields.length > 0) word(fields(0)) else 0.U(64.W))
    dpi.io.w1 := (if (fields.length > 1) word(fields(1)) else 0.U(64.W))
    dpi.io.w2 := (if (fields.length > 2) word(fields(2)) else 0.U(64.W))
    dpi.io.w3 := (if (fields.length > 3) word(fields(3)) else 0.U(64.W))
    dpi.io.w4 := (if (fields.length > 4) word(fields(4)) else 0.U(64.W))
    dpi.io.w5 := (if (fields.length > 5) word(fields(5)) else 0.U(64.W))
    dpi.io.w6 := (if (fields.length > 6) word(fields(6)) else 0.U(64.W))
    dpi.io.w7 := (if (fields.length > 7) word(fields(7)) else 0.U(64.W))
    dpi.io.w8 := (if (fields.length > 8) word(fields(8)) else 0.U(64.W))
    dpi.io.w9 := (if (fields.length > 9) word(fields(9)) else 0.U(64.W))
    dpi.io.w10 := (if (fields.length > 10) word(fields(10)) else 0.U(64.W))
    dpi.io.w11 := (if (fields.length > 11) word(fields(11)) else 0.U(64.W))
    dpi.io.w12 := (if (fields.length > 12) word(fields(12)) else 0.U(64.W))
    dpi.io.w13 := (if (fields.length > 13) word(fields(13)) else 0.U(64.W))
    dpi.io.w14 := (if (fields.length > 14) word(fields(14)) else 0.U(64.W))
    dpi.io.w15 := (if (fields.length > 15) word(fields(15)) else 0.U(64.W))
  }
}
