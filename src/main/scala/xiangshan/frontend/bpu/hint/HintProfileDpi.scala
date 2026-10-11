package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.util.HasBlackBoxInline

/** Simulation-only FTQ retirement callback sink; ROB identity is audited separately. */
class HintProfileDpi extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val valid = Input(Bool())
    val ftqTag = Input(UInt(7.W))
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
  input logic [6:0] ftqTag,
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
    input bit eligible, observed, input int unsigned ftqTag);
  always @(posedge clock) if (valid)
    h2p_hint_record(pc, actual, baselinePc, baselineTarget,
                    baselineValid, blockStart, queryCycle,
                    {4'b0, ith}, {4'b0, lhr}, {4'b0, call}, {4'b0, ret},
                    eligible, observed, {25'b0, ftqTag});
endmodule
""")
}


/** Simulation-only genuine S1 snapshot / S3 admission observer. stage: 0=S1, 1=S3. */
class HintQueryV4Dpi extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val valid = Input(Bool())
    val stage = Input(UInt(1.W))
    val ftqTag = Input(UInt(7.W))
    val queryCycle = Input(UInt(64.W))
    val startPc = Input(UInt(64.W))
    val candidatePc = Input(UInt(64.W))
    val pcId = Input(UInt(4.W))
    val candidate = Input(Bool())
    val lookupFire = Input(Bool())
    val s3Eligible = Input(Bool())
    val admission = Input(Bool())
    val ith = Input(UInt(28.W))
    val lhr = Input(UInt(28.W))
    val call = Input(UInt(28.W))
    val ret = Input(UInt(28.W))
  })
  setInline("HintQueryV4Dpi.sv", """
module HintQueryV4Dpi (
  input logic clock, valid, stage,
  input logic [6:0] ftqTag,
  input logic [63:0] queryCycle, startPc, candidatePc,
  input logic [3:0] pcId,
  input logic candidate, lookupFire, s3Eligible, admission,
  input logic [27:0] ith, lhr, call, ret
);
  import "DPI-C" function void h2p_hint_query_v4(
    input int unsigned stage, ftqTag,
    input longint unsigned queryCycle, startPc, candidatePc,
    input int unsigned pcId,
    input bit candidate, lookupFire, s3Eligible, admission,
    input int unsigned ith, lhr, call, ret);
  always @(posedge clock) if (valid)
    h2p_hint_query_v4({31'b0, stage}, {25'b0, ftqTag}, queryCycle, startPc, candidatePc,
      {28'b0, pcId}, candidate, lookupFire, s3Eligible, admission,
      {4'b0, ith}, {4'b0, lhr}, {4'b0, call}, {4'b0, ret});
endmodule
""")
}
