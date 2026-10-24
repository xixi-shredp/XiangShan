package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.util.HasBlackBoxInline

/** Simulation-only combinational S3 observation. All persistence lives in the host. */
class HintPredictionDpi extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val valid = Input(Bool())
    val ftqTag = Input(UInt(7.W))
    val queryCycle = Input(UInt(64.W))
    val cfiPc = Input(UInt(64.W))
    val baselineTarget = Input(UInt(64.W))
    val finalTarget = Input(UInt(64.W))
    val contextTarget = Input(UInt(64.W))
    val contextHash = Input(UInt(28.W))
    val contextHit = Input(Bool())
    val usedHint = Input(Bool())
    val conf = Input(UInt(2.W))
    val camClassified = Input(Bool())
    val isCall = Input(Bool())
    val finalTaken = Input(Bool())
    val finalPosition = Input(UInt(5.W))
    val baselineTaken = Input(Bool())
    val baselinePosition = Input(UInt(5.W))
    val ittageHit = Input(Bool())
    val ittageTarget = Input(UInt(64.W))
    val btbTargetValid = Input(Bool())
    val btbTarget = Input(UInt(64.W))
    val fallThroughTarget = Input(UInt(64.W))
    val finalIndirect = Input(Bool())
    val finalIsCall = Input(Bool())
    val s3Override = Input(Bool())
  })
  setInline("HintPredictionDpi.sv", """
module HintPredictionDpi (
 input logic clock, valid,
 input logic [6:0] ftqTag,
 input logic [63:0] queryCycle, cfiPc, baselineTarget, finalTarget, contextTarget,
 input logic [27:0] contextHash,
 input logic contextHit, usedHint,
 input logic [1:0] conf,
 input logic camClassified, isCall, finalTaken, baselineTaken, ittageHit, btbTargetValid,
 input logic finalIndirect, finalIsCall, s3Override,
 input logic [4:0] finalPosition, baselinePosition,
 input logic [63:0] ittageTarget, btbTarget, fallThroughTarget
);
 import "DPI-C" function void h2p_hint_prediction(
   input int unsigned ftqTag,
   input longint unsigned queryCycle, cfiPc, baselineTarget, finalTarget, contextTarget,
   input int unsigned contextHash,
   input bit contextHit, usedHint, input byte unsigned conf,
   input bit camClassified, isCall, finalTaken, input byte unsigned finalPosition,
   input bit baselineTaken, input byte unsigned baselinePosition,
   input bit ittageHit, input longint unsigned ittageTarget,
   input bit btbTargetValid, input longint unsigned btbTarget, fallThroughTarget,
   input bit finalIndirect, finalIsCall, s3Override);
 always @(posedge clock) if(valid)
   h2p_hint_prediction({25'b0,ftqTag}, queryCycle, cfiPc, baselineTarget, finalTarget,
     contextTarget, {4'b0,contextHash}, contextHit, usedHint, {6'b0,conf},
     camClassified, isCall, finalTaken, {3'b0,finalPosition},
     baselineTaken, {3'b0,baselinePosition}, ittageHit, ittageTarget,
     btbTargetValid, btbTarget, fallThroughTarget, finalIndirect, finalIsCall, s3Override);
endmodule
""")
}
