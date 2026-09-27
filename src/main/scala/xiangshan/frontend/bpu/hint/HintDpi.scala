package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.util.HasBlackBoxInline

/** Simulation-only boundary. All stateful predictor tables remain in Hint. */
class HintDpi extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val loadReady = Input(Bool())
    val loadValid = Output(Bool())
    val loadKind = Output(UInt(2.W))
    val loadIndex = Output(UInt(12.W))
    val loadData = Output(UInt(64.W))
    val enabled = Output(Bool())
    val profile = Output(Bool())
    val measure = Output(Bool())
    val stats = Input(new HintStats)
  })

  setInline("HintDpi.sv", """
module HintDpi (
  input  logic        clock,
  input  logic        reset,
  input  logic        loadReady,
  output logic        loadValid,
  output logic [1:0]  loadKind,
  output logic [11:0] loadIndex,
  output logic [63:0] loadData,
  output logic        enabled,
  output logic        profile,
  output logic        measure,
  input logic [31:0] stats_camMiss,
  input logic [31:0] stats_contextMiss,
  input logic [31:0] stats_confZero,
  input logic [31:0] stats_overrides,
  input logic [31:0] stats_metaStall,
  input logic [31:0] stats_lhrStall,
  input logic [31:0] stats_confUp,
  input logic [31:0] stats_confDown,
  input logic [31:0] stats_confDropped
);
  import "DPI-C" function void h2p_hint_tick(
    input bit reset,
    input int unsigned camMiss, contextMiss, confZero, overrides,
    input int unsigned metaStall, lhrStall, confUp, confDown, confDropped,
    output bit valid, output byte unsigned kind,
    output int unsigned index, output longint unsigned data,
    output bit enabled, output bit profile, output bit measure);
  import "DPI-C" function void h2p_hint_ack(input bit reset, input bit valid, input bit ready);
  byte unsigned dpiKind;
  int unsigned dpiIndex;
  longint unsigned dpiData;
  initial begin
    loadValid = 0; loadKind = 0; loadIndex = 0; loadData = 0;
    enabled = 0; profile = 0; measure = 0;
  end
  always @(posedge clock) h2p_hint_ack(reset, loadValid, loadReady);
  always @(negedge clock) begin
    h2p_hint_tick(reset, stats_camMiss, stats_contextMiss, stats_confZero,
                  stats_overrides, stats_metaStall, stats_lhrStall,
                  stats_confUp, stats_confDown, stats_confDropped,
                  loadValid, dpiKind,
                  dpiIndex, dpiData, enabled, profile, measure);
    loadKind = dpiKind[1:0];
    loadIndex = dpiIndex[11:0];
    loadData = dpiData;
  end
endmodule
""")
}
