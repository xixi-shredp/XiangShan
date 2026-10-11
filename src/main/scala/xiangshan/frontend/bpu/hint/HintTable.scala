package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.sram.{FoldedSRAMTemplate, SRAMTemplate}
import xiangshan.frontend.bpu.BpuModule

class HintContextLoad extends Bundle {
  val index = UInt(12.W)
  val data = UInt(25.W)
}
class HintConfidenceWrite extends Bundle {
  val index = UInt(12.W)
  val conf = UInt(2.W)
}
class HintSlotLoad extends Bundle {
  val index = UInt(5.W)
  val data = UInt(47.W)
}

/** One logical Context port: prediction reads both ways, writes wait.
  * Metadata is immutable during execution; confidence writes need no read.
  */
class HintContextTable(implicit p: Parameters) extends BpuModule {
  val io = IO(new Bundle {
    val clear = Input(Bool())
    val resetDone = Output(Bool())
    val read = Flipped(Decoupled(UInt(11.W)))
    val rows = Output(Vec(2, UInt(25.W)))
    val respValid = Output(Bool())
    val readSet = Output(UInt(11.W))
    val load = Flipped(Decoupled(new HintContextLoad))
    val writeConf = Flipped(Decoupled(new HintConfidenceWrite))
  })
  private val meta = Module(new SRAMTemplate(UInt(23.W), set = 2048, way = 2,
    singlePort = true, holdRead = true, shouldReset = true, extraReset = true,
    withClockGate = true, hasMbist = hasMbist, hasSramCtl = hasSramCtl,
    suffix = Some("bpu_hint_context_meta"))).suggestName("hint_context_meta_sram")
  private val conf = Module(new FoldedSRAMTemplate(UInt(2.W), set = 2048, width = 8, way = 2,
    singlePort = true, holdRead = true, shouldReset = true, extraReset = true,
    withClockGate = true, hasMbist = hasMbist, hasSramCtl = hasSramCtl,
    suffix = Some("bpu_hint_context_conf"))).suggestName("hint_context_conf_sram")
  meta.extra_reset.get := io.clear
  conf.extra_reset.get := io.clear
  io.resetDone := meta.io.resetDone && conf.io.resetDone
  private val available = io.resetDone && !io.clear
  private val pairedReadReady = meta.io.r.req.ready && conf.io.r.req.ready
  private val pairedWriteReady = meta.io.w.req.ready && conf.io.w.req.ready
  io.read.ready := available && pairedReadReady
  private val readGrant = io.read.valid && available
  meta.io.r.req.valid := readGrant && pairedReadReady
  conf.io.r.req.valid := readGrant && pairedReadReady
  meta.io.r.req.bits.setIdx := io.read.bits
  conf.io.r.req.bits.setIdx := io.read.bits

  io.load.ready := available && !io.read.valid && pairedWriteReady
  io.writeConf.ready := available && !io.read.valid && !io.load.valid && conf.io.w.req.ready
  private val loadGrant = io.load.valid && io.load.ready
  private val confGrant = io.writeConf.valid && io.writeConf.ready
  private val loadWay = UIntToOH(io.load.bits.index(0), 2)
  private val confWay = UIntToOH(io.writeConf.bits.index(0), 2)
  private val metaData = Cat(io.load.bits.data(24), io.load.bits.data(21, 0))
  meta.io.w.apply(loadGrant, VecInit(Seq.fill(2)(metaData)), io.load.bits.index(11, 1), loadWay)
  conf.io.w.apply(loadGrant || confGrant,
    VecInit(Seq.fill(2)(Mux(loadGrant, io.load.bits.data(23, 22), io.writeConf.bits.conf))),
    Mux(loadGrant, io.load.bits.index(11, 1), io.writeConf.bits.index(11, 1)),
    Mux(loadGrant, loadWay, confWay))
  for (way <- 0 until 2) {
    val m = meta.io.r.resp.data(way)
    io.rows(way) := Cat(m(22), conf.io.r.resp.data(way), m(21, 0))
  }
  private val responseValid = RegNext(io.read.fire, false.B)
  when(io.clear) { responseValid := false.B }
  io.respValid := responseValid
  io.readSet := RegEnable(io.read.bits, io.read.fire)
  assert(meta.io.r.req.fire === conf.io.r.req.fire)
  assert(!loadGrant || (meta.io.w.req.fire && conf.io.w.req.fire))
  assert(!(io.read.fire && (loadGrant || confGrant)))
  assert(!meta.io.w.req.fire || loadGrant)
}

class HintSlotTable(implicit p: Parameters) extends BpuModule {
  val io = IO(new Bundle {
    val clear = Input(Bool())
    val resetDone = Output(Bool())
    val read = Flipped(Decoupled(UInt(5.W)))
    val target = Output(UInt(47.W))
    val respValid = Output(Bool())
    val readSlot = Output(UInt(5.W))
    val load = Flipped(Decoupled(new HintSlotLoad))
  })
  private val table = Module(new SRAMTemplate(UInt(47.W), set = 32, way = 1,
    singlePort = true, holdRead = true, shouldReset = true, extraReset = true,
    withClockGate = true, hasMbist = hasMbist, hasSramCtl = hasSramCtl,
    suffix = Some("bpu_hint_slot"))).suggestName("hint_slot_sram")
  table.extra_reset.get := io.clear
  io.resetDone := table.io.resetDone
  private val available = io.resetDone && !io.clear
  io.read.ready := available && table.io.r.req.ready
  table.io.r.req.valid := io.read.valid && available
  table.io.r.req.bits.setIdx := io.read.bits
  io.load.ready := available && !io.read.valid && table.io.w.req.ready
  table.io.w.apply(io.load.valid && io.load.ready, io.load.bits.data, io.load.bits.index, 1.U(1.W))
  io.target := table.io.r.resp.data.head
  private val responseValid = RegNext(io.read.fire, false.B)
  when(io.clear) { responseValid := false.B }
  io.respValid := responseValid
  io.readSlot := RegEnable(io.read.bits, io.read.fire)
  assert(!(io.read.fire && table.io.w.req.fire))
}
