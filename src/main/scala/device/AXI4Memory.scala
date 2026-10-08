/***************************************************************************************
  * Copyright (c) 2020-2022 Institute of Computing Technology, Chinese Academy of Sciences
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  *          http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  ***************************************************************************************/

package device

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.experimental.{ExtModule, prefix}
import chisel3.util._
import difftest.common.DifftestMem
import freechips.rocketchip.amba.axi4.{AXI4MasterNode, AXI4Parameters, AXI4SlaveNode}
import freechips.rocketchip.diplomacy.{AddressSet, InModuleBody, LazyModule, LazyModuleImp}
import utility._

class MemoryRequestHelper(requestType: Int)
  extends ExtModule(Map("REQUEST_TYPE" -> requestType))
  with HasExtModuleInline
{
  val clock     = IO(Input(Clock()))
  val reset     = IO(Input(Reset()))
  val io = IO(new Bundle {
    val req = Flipped(ValidIO(new Bundle {
      val addr = UInt(64.W)
      val id   = UInt(32.W)
    }))
    val response = Output(Bool())
  })
  val cppExtModule =
    """
      |void MemoryRequestHelper (
      |  int       REQUEST_TYPE,
      |  uint8_t   reset,
      |  uint8_t   io_req_valid,
      |  uint64_t  io_req_bits_addr,
      |  uint32_t  io_req_bits_id,
      |  uint8_t&  io_response
      |) {
      |  if (reset) io_response = 0;
      |  else if (io_req_valid) io_response = memory_request(io_req_bits_addr, io_req_bits_id, REQUEST_TYPE);
      |  else io_response = 0;
      |}
      |""".stripMargin
  difftest.DifftestModule.createCppExtModule("MemoryRequestHelper", cppExtModule, Some("\"ram.h\""))


  val verilogLines = Seq(
    "import \"DPI-C\" function bit memory_request (",
    "  input longint address,",
    "  input int id,",
    "  input bit isWrite",
    ");",
    "",
    "module MemoryRequestHelper #(",
    "  parameter REQUEST_TYPE",
    ")(",
    "  input             clock,",
    "  input             reset,",
    "  input             io_req_valid,",
    "  input      [63:0] io_req_bits_addr,",
    "  input      [31:0] io_req_bits_id,",
    "  output            io_response",
    ");",
    "",
    " assign io_response = (reset ? 1'b0 :",
    "               (io_req_valid ? memory_request(io_req_bits_addr, io_req_bits_id, REQUEST_TYPE) : 1'b0));",
    "endmodule"
  )
  setInline(s"$desiredName.v", verilogLines.mkString("\n"))
}

class MemoryResponseHelper(requestType: Int)
  extends ExtModule(Map("REQUEST_TYPE" -> requestType))
  with HasExtModuleInline
{
  val clock    = IO(Input(Clock()))
  val reset    = IO(Input(Reset()))
  val enable   = IO(Input(Bool()))
  val response = IO(Output(UInt(64.W)))

  val cppExtModule =
    """
      |void MemoryResponseHelper (
      |  int       REQUEST_TYPE,
      |  uint8_t   reset,
      |  uint8_t   enable,
      |  uint64_t& response
      |) {
      |  if (reset) response = 0;
      |  else if (!reset && enable) response = memory_response(REQUEST_TYPE);
      |  else response = 0;
      |}
      |""".stripMargin
  difftest.DifftestModule.createCppExtModule("MemoryResponseHelper", cppExtModule, Some("\"ram.h\""))


  val verilogLines = Seq(
    "import \"DPI-C\" function longint memory_response (",
    "  input bit isWrite",
    ");",
    "",
    "module MemoryResponseHelper #(",
    "  parameter REQUEST_TYPE",
    ")(",
    "  input             clock,",
    "  input             reset,",
    "  input             enable,",
    "  output [63:0]     response",
    ");",
    "",
    "assign response = reset ? 64'b0 :",
    "((!reset && enable) ? memory_response(REQUEST_TYPE) : 64'b0);",
    "endmodule"
  )
  setInline(s"$desiredName.v", verilogLines.mkString("\n"))
}

trait MemoryHelper { this: Module =>
  private def requestType(isWrite: Boolean): Int = if (isWrite) 1 else 0
  private def request(valid: Bool, addr: UInt, id: UInt, isWrite: Boolean): Bool = {
    val helper = Module(new MemoryRequestHelper(requestType(isWrite)))
    helper.clock := clock
    helper.reset := reset
    helper.io.req.valid := valid
    helper.io.req.bits.addr := addr
    helper.io.req.bits.id := id
    helper.io.response
  }
  protected def readRequest(valid: Bool, addr: UInt, id: UInt): Bool =
    request(valid, addr, id, false)
  protected def writeRequest(valid: Bool, addr: UInt, id: UInt): Bool =
    request(valid, addr, id, true)
  private def response(enable: Bool, isWrite: Boolean): (Bool, UInt) = {
    val helper = Module(new MemoryResponseHelper(requestType(isWrite)))
    helper.clock := clock
    helper.reset := reset
    helper.enable := enable
    (helper.response(32), helper.response(31, 0))
  }
  protected def readResponse(enable: Bool): (Bool, UInt) =
    response(enable, false)
  protected def writeResponse(enable: Bool): (Bool, UInt) =
    response(enable, true)
}

// Simulation contract only: verify that the C++ instance uses the same clock and
// callback padding as this elaborated AXI memory. This never enters the SoC.
class MemoryConfigurationHelper(coreMHz: Int, paddingCycles: Int)
  extends ExtModule(Map("CORE_MHZ" -> coreMHz, "PADDING_CYCLES" -> paddingCycles))
  with HasExtModuleInline {
  val clock = IO(Input(Clock()))
  val reset = IO(Input(Reset()))
  val cppExtModule = """
    |void MemoryConfigurationHelper(int CORE_MHZ, int PADDING_CYCLES, uint8_t reset) {
    |  if (!reset) memory_model_validate(CORE_MHZ, PADDING_CYCLES);
    |}
    |""".stripMargin
  difftest.DifftestModule.createCppExtModule("MemoryConfigurationHelper", cppExtModule, Some("\"ram.h\""))
  setInline(s"$desiredName.v", """
    |import "DPI-C" function void memory_model_validate(input int core_mhz, input int padding_cycles);
    |module MemoryConfigurationHelper #(parameter CORE_MHZ, PADDING_CYCLES)(input clock, reset);
    |  always @(posedge clock) if (!reset) memory_model_validate(CORE_MHZ, PADDING_CYCLES);
    |endmodule
    |""".stripMargin)
}

// Actual dynamic-memory write-completion join, separated for directed simulation.
// IDs remain owned until the master consumes B; W data follows AXI AW order,
// whereas DRAM callbacks may complete different IDs in any order.
class AXIWriteCompletion(capacity: Int, idBits: Int = 0) extends Module {
  require(isPow2(capacity))
  require(idBits >= 0 && idBits <= 16)
  val tokenBits = if (idBits == 0) log2Ceil(capacity) else idBits
  val io = IO(new Bundle {
    val queryId = Input(UInt(32.W))
    val canAccept = Output(Bool())
    val allocSlot = Output(UInt(log2Ceil(capacity).W))
    val dataId = Input(UInt(32.W))
    val dataSlot = Output(UInt(log2Ceil(capacity).W))
    val active = Output(Bool())
    val accept = Flipped(Valid(UInt(32.W)))
    val wLast = Flipped(Decoupled(UInt(32.W)))
    val dram = Flipped(Valid(UInt(32.W)))
    val b = Decoupled(UInt(32.W))
    val retire = Flipped(Valid(UInt(32.W)))
  })
  val busy = RegInit(VecInit.fill(capacity)(false.B))
  val ids = Reg(Vec(capacity, UInt(32.W)))
  val done = RegInit(VecInit.fill(capacity)(false.B))
  def hits(id: UInt) = VecInit((0 until capacity).map(i => busy(i) && ids(i) === id))
  def slot(id: UInt) = PriorityEncoder(hits(id))
  val freeSlot = PriorityEncoder(VecInit(busy.map(v => !v)))
  io.allocSlot := freeSlot
  io.dataSlot := slot(io.dataId)
  val completedW = Module(new Queue(UInt(32.W),capacity,flow=false))
  io.canAccept := io.queryId < (BigInt(1) << tokenBits).U && !hits(io.queryId).asUInt.orR && !busy.asUInt.andR
  io.active := busy.asUInt.orR
  completedW.io.enq <> io.wLast
  io.b.valid := completedW.io.deq.valid && done(slot(completedW.io.deq.bits))
  io.b.bits := completedW.io.deq.bits
  completedW.io.deq.ready := io.b.ready && done(slot(completedW.io.deq.bits))
  when(io.accept.valid) {
    assert(io.accept.bits < (BigInt(1) << tokenBits).U && !hits(io.accept.bits).asUInt.orR && !busy.asUInt.andR, "write AXI ID already owned or trackers full")
    busy(freeSlot) := true.B
    ids(freeSlot) := io.accept.bits
    done(freeSlot) := false.B
  }
  when(io.wLast.fire) {
    assert(PopCount(hits(io.wLast.bits)) === 1.U, "W.last has no AW owner")
  }
  when(io.dram.valid) {
    assert(PopCount(hits(io.dram.bits)) === 1.U && !done(slot(io.dram.bits)), "unexpected DRAM write completion")
    done(slot(io.dram.bits)) := true.B
  }
  when(io.b.fire) { done(slot(io.b.bits)) := false.B }
  when(io.retire.valid) {
    assert(PopCount(hits(io.retire.bits)) === 1.U, "B has no AW owner")
    busy(slot(io.retire.bits)) := false.B
  }
}

class AXI4MemoryImp[T <: Data](outer: AXI4Memory) extends AXI4SlaveModuleImp(outer) with MemoryHelper {
  outer.dramContract.foreach { case (coreMHz, paddingCycles) =>
    val contract = Module(new MemoryConfigurationHelper(coreMHz, paddingCycles))
    contract.clock := clock
    contract.reset := reset
  }
  val ramBaseAddr = outer.address.head.base
  val (ramIndexBits, ramOffsetBits) = (log2Ceil(outer.beatBytes), log2Ceil(outer.memByte))
  def ramIndex(addr: UInt) = ((addr - ramBaseAddr.U)(ramOffsetBits - 1, 0) >> ramIndexBits).asUInt
  val ramHelper = DifftestMem(outer.memByte, outer.beatBytes, 8, singlePort = false)

  // TODO: *NOTICE* The simulated behaviour of DRAM (or other external models) might be inaccurate,
  //       since we are not simulating the actual DRAM chip bits width.
  //       Only the first request / burst is sent to the external model, regardless of the channel
  //       count and AXI4 channel width.
  //       This feature was not changed since the original implementation to keep the performance 
  //       consistent with the legacy AXI4Memory SoC. Which was natural for write transactions, and
  //       patched by 'read_flap*' for read transactions.

  // !! This implementation should be only used for simulation purposes. !! 

  val numOutstanding = 64
  // Xbar master offsets are part of the downstream ID. Index by the actual
  // negotiated bus width using 64 owner slots, never a 2^idBits table.
  val writeIdBits = in.aw.bits.id.getWidth
  val readIdBits = in.ar.bits.id.getWidth
  require(writeIdBits <= 16 && readIdBits <= 16)
  // Note: we are using in.ar.bits.addr.getWidth insead of ramOffsetBits here.
  // Why: the CPU may access out-of-range addresses. Let the RAM helper deal with it.
  val awQueue = Module(new Queue(chiselTypeOf(in.aw.bits), numOutstanding, flow = false))
  val wQueue = Module(new Queue(chiselTypeOf(in.w.bits), numOutstanding * 2, flow = false))
  val bQueue = Module(new Queue(chiselTypeOf(in.b.bits), numOutstanding, flow = false))
  val arQueue = Module(new Queue(chiselTypeOf(in.ar.bits), numOutstanding, flow = false))
  val rQueue = Module(new Queue(chiselTypeOf(in.r.bits), numOutstanding * 2, flow = false))

  val wTrackers = Module(new Queue(chiselTypeOf(in.aw.bits), numOutstanding, flow = true))
  val writeCompletion = Module(new AXIWriteCompletion(numOutstanding,writeIdBits))
  writeCompletion.io.queryId := in.aw.bits.id
  writeCompletion.io.accept.valid := in.aw.fire
  writeCompletion.io.accept.bits := in.aw.bits.id
  writeCompletion.io.retire.valid := in.b.fire
  writeCompletion.io.retire.bits := in.b.bits.id
  writeCompletion.io.dataId := wTrackers.io.deq.bits.id
  val wTrackersDataID = RegInit(VecInit.fill(numOutstanding)(0.U(log2Up(outer.burstLen + 1).W)))

  val rTrackers = Mem(numOutstanding, chiselTypeOf(in.ar.bits))
  val rTrackersDataID = RegInit(VecInit.fill(numOutstanding)(0.U(log2Up(outer.burstLen + 1).W)))
  val readBusy = RegInit(VecInit.fill(numOutstanding)(false.B))
  val readIds = Reg(Vec(numOutstanding,UInt(readIdBits.W)))
  def readHits(id: UInt) = VecInit((0 until numOutstanding).map(i => readBusy(i) && readIds(i) === id))
  def readSlot(id: UInt) = PriorityEncoder(readHits(id))
  val readAllocSlot = PriorityEncoder(VecInit(readBusy.map(v => !v)))

  val w_count = RegInit(0.U(log2Up(numOutstanding + 1).W))
  val r_count = RegInit(0.U(log2Up(numOutstanding + 1).W))

  // accept a read request and send it to the external model
  val readIdAvailable = !readHits(in.ar.bits.id).asUInt.orR && !readBusy.asUInt.andR
  in.ar.ready := arQueue.io.enq.ready && readIdAvailable
  arQueue.io.enq.valid := in.ar.valid && readIdAvailable
  arQueue.io.enq.bits := in.ar.bits

  when (arQueue.io.enq.fire) {
    rTrackers.write(readAllocSlot, in.ar.bits)
    rTrackersDataID(readAllocSlot) := 0.U
    readBusy(readAllocSlot) := true.B
    readIds(readAllocSlot) := in.ar.bits.id
  }

  arQueue.io.deq.ready := readRequest(arQueue.io.deq.valid, arQueue.io.deq.bits.addr, arQueue.io.deq.bits.id)

  // read data response: resp from DRAMsim3; read data and response to in.r
  val rPipe = Module(new Queue(chiselTypeOf(in.r.bits), 1, pipe = true, flow = false))

  val read_flap = RegInit(false.B)
  val read_flap_tracker = Reg(chiselTypeOf(in.ar.bits))

  val (read_resp_valid, read_resp_id_ext) = readResponse((r_count =/= 0.U) && rPipe.io.enq.ready && !read_flap)
  val read_resp_id = Mux(
    read_flap,
    read_flap_tracker.id, // second and further read burst if necessary, bypassing external model read
    read_resp_id_ext // first read burst
  )
  val read_resp_slot = readSlot(read_resp_id)
  when(read_resp_valid || read_flap) {
    assert(PopCount(readHits(read_resp_id)) === 1.U, "AXI4Memory: unexpected DRAM read ID")
  }
  val read_resp_addr = ramIndex(rTrackers(read_resp_slot).addr) + rTrackersDataID(read_resp_slot)

  rPipe.io.enq.valid := read_resp_valid || read_flap
  rPipe.io.enq.bits.id := read_resp_id
  rPipe.io.enq.bits.data := DontCare // read data is not available yet
  rPipe.io.enq.bits.resp := AXI4Parameters.RESP_OKAY
  rPipe.io.enq.bits.last := rTrackersDataID(read_resp_slot) === rTrackers(read_resp_slot).len

  when (rPipe.io.enq.fire) {
    when (!rPipe.io.enq.bits.last) {
      read_flap := true.B
      read_flap_tracker := Mux(read_flap, read_flap_tracker, rTrackers(read_resp_slot))
    }.otherwise {
      read_flap := false.B
    }
    rTrackersDataID(read_resp_slot) := rTrackersDataID(read_resp_slot) + 1.U
  }

  rPipe.io.deq.ready := rQueue.io.enq.ready
  rQueue.io.enq.valid := rPipe.io.deq.valid
  rQueue.io.enq.bits.id := rPipe.io.deq.bits.id
  rQueue.io.enq.bits.data := ramHelper.readAndHold(read_resp_addr, rPipe.io.enq.fire).asUInt
  rQueue.io.enq.bits.resp := rPipe.io.deq.bits.resp
  rQueue.io.enq.bits.last := rPipe.io.deq.bits.last

  rQueue.io.deq.ready := in.r.ready
  in.r.valid := rQueue.io.deq.valid
  in.r.bits := rQueue.io.deq.bits
  when(in.r.fire && in.r.bits.last) {
    assert(PopCount(readHits(in.r.bits.id)) === 1.U, "AXI4Memory: R has no AR owner")
    readBusy(readSlot(in.r.bits.id)) := false.B
  }

  // read request counter
  val r_count_inc = arQueue.io.deq.fire
  val r_count_dec = rQueue.io.enq.fire && rQueue.io.enq.bits.last
  when (r_count_inc ^ r_count_dec) {
    r_count := Mux(r_count_inc, r_count + 1.U, r_count - 1.U)
  }

  // accept a write request and send it to the external model
  val writeIdAvailable = writeCompletion.io.canAccept
  in.aw.ready := awQueue.io.enq.ready && wTrackers.io.enq.ready && writeIdAvailable
  awQueue.io.enq.valid := in.aw.valid && wTrackers.io.enq.ready && writeIdAvailable
  awQueue.io.enq.bits := in.aw.bits

  wTrackers.io.enq.valid := in.aw.valid && awQueue.io.enq.ready && writeIdAvailable
  wTrackers.io.enq.bits := in.aw.bits

  when (wTrackers.io.enq.fire) {
    wTrackersDataID(writeCompletion.io.allocSlot) := 0.U
  }
  
  awQueue.io.deq.ready := writeRequest(awQueue.io.deq.valid, awQueue.io.deq.bits.addr, awQueue.io.deq.bits.id)

  // accept a write data and write it to the internal memory
  in.w.ready := wQueue.io.enq.ready
  wQueue.io.enq.valid := in.w.valid
  wQueue.io.enq.bits := in.w.bits

  wQueue.io.deq.ready := (w_count =/= 0.U) && wTrackers.io.deq.valid &&
    (!wQueue.io.deq.bits.last || writeCompletion.io.wLast.ready)

  val write_req_addr = ramIndex(wTrackers.io.deq.bits.addr) + wTrackersDataID(writeCompletion.io.dataSlot)
  when (wQueue.io.deq.fire) {
    ramHelper.write(
      addr = write_req_addr,
      data = wQueue.io.deq.bits.data.asTypeOf(Vec(outer.beatBytes, UInt(8.W))),
      mask = wQueue.io.deq.bits.strb.asBools
    )
    wTrackersDataID(writeCompletion.io.dataSlot) := wTrackersDataID(writeCompletion.io.dataSlot) + 1.U
  }

  assert(!wQueue.io.deq.fire || wTrackers.io.deq.valid, "AXI4Memory: write tracker underflow on W channel")

  // write request counter
  val w_count_inc = awQueue.io.deq.fire
  val w_count_dec = wQueue.io.deq.fire && wQueue.io.deq.bits.last
  when (w_count_inc ^ w_count_dec) {
    w_count := Mux(w_count_inc, w_count + 1.U, w_count - 1.U)
  }

  // generate write response
  bQueue.io.deq.ready := in.b.ready
  in.b.valid := bQueue.io.deq.valid
  in.b.bits := bQueue.io.deq.bits

  val (writeRespValid, writeRespId) = writeResponse(writeCompletion.io.active)
  writeCompletion.io.dram.valid := writeRespValid
  writeCompletion.io.dram.bits := writeRespId
  writeCompletion.io.wLast.valid := wQueue.io.deq.valid && (w_count =/= 0.U) &&
    wTrackers.io.deq.valid && wQueue.io.deq.bits.last
  writeCompletion.io.wLast.bits := wTrackers.io.deq.bits.id
  wTrackers.io.deq.ready := writeCompletion.io.wLast.fire
  bQueue.io.enq.valid := writeCompletion.io.b.valid
  bQueue.io.enq.bits.id := writeCompletion.io.b.bits
  bQueue.io.enq.bits.resp := AXI4Parameters.RESP_OKAY
  writeCompletion.io.b.ready := bQueue.io.enq.ready

}

class AXI4Memory
(
  val address: Seq[AddressSet],
  val memByte: Long,
  val useBlackBox: Boolean = false,
  val executable: Boolean = true,
  val beatBytes: Int,
  val burstLen: Int,
  val dramContract: Option[(Int, Int)] = None,
)(implicit p: Parameters)
  extends AXI4SlaveModule(address, executable, beatBytes, burstLen)
{
  override lazy val module = new AXI4MemoryImp(this)
}

class AXI4MemoryWrapper (
  slave: AXI4SlaveNode,
  memByte: Long,
  useBlackBox: Boolean = false,
  dramContract: Option[(Int, Int)] = None
)(implicit p: Parameters) extends AXI4MemorySlave(slave, memByte, useBlackBox) {
  val ram = LazyModule(new AXI4Memory(
    slaveParam.address,
    memByte,
    useBlackBox,
    slaveParam.executable,
    portParam.beatBytes,
    burstLen,
    dramContract
  ))
  ram.node := master
}

abstract class AXI4MemorySlave (
  slave: AXI4SlaveNode,
  memByte: Long,
  useBlackBox: Boolean = false
)(implicit p: Parameters) extends LazyModule {
  val master = AXI4MasterNode(List(slave.in.head._2.master))

  val portParam = slave.portParams.head
  val slaveParam = portParam.slaves.head
  val burstLen = portParam.maxTransfer / portParam.beatBytes

  val io_axi4 = InModuleBody{ master.makeIOs() }

  lazy val module = new LazyModuleImp(this) { }
}

object AXI4MemorySlave {
  def apply(
    slave: AXI4SlaveNode,
    memByte: Long,
    useBlackBox: Boolean = false,
    dynamicLatency: Boolean = false,
    dramContract: Option[(Int, Int)] = None
  )(implicit p: Parameters): AXI4MemorySlave = {
    val memory = if (dynamicLatency) {
      LazyModule(new AXI4MemoryWrapper(slave, memByte, useBlackBox, dramContract))
    } else {
      LazyModule(new AXI4RAMWrapper(slave, memByte, useBlackBox))
    }
    memory
  }
}
