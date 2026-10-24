package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSBundle
import xiangshan.frontend.bpu.BpuModule

/** A read-only FTQ commit request. No contexts are stored in the FTQ. */
class HintTraceCommit(implicit p: Parameters) extends XSBundle {
  val snapshot = new HintSnapshot
  val actualPc = UInt(64.W)
  val actualTarget = UInt(64.W)
  val baselineMiss = Bool()
  val queryStartPc = UInt(64.W)
  val ftqTag = UInt(7.W)
  val pcId = UInt(4.W)
  val queryValid = Bool()
  val eligible = Bool()
  val queryCycle = Option.when(!env.FPGAPlatform)(UInt(64.W))
  // Existing predictor baseline is forwarded for the old simulation audit
  // callback only. It is not retained in FTQ state or the hardware trace FIFO.
  val baselinePc = Option.when(!env.FPGAPlatform)(UInt(64.W))
  val baselineTarget = Option.when(!env.FPGAPlatform)(UInt(64.W))
  val baselineValid = Option.when(!env.FPGAPlatform)(Bool())
}

/** Folded pre-branch context, computed without changing predictor history. */
class HintTraceFoldEvent(implicit p: Parameters) extends XSBundle {
  val actualPc = UInt(64.W)
  val actualTarget = UInt(64.W)
  val baselineMiss = Bool()
  val queryStartPc = UInt(64.W)
  val ftqTag = UInt(7.W)
  val pcId = UInt(4.W)
  val queryValid = Bool()
  val eligible = Bool()
  val queryCycle = Option.when(!env.FPGAPlatform)(UInt(64.W))
  val ith = UInt(28.W)
  val lhr = UInt(28.W)
  val call = UInt(28.W)
  val ret = UInt(28.W)
}

/** Actual terminal / surviving-prefix boundary in existing resolve metadata (58 bits).
  * valid marks a known boundary; eligible marks a taken non-return indirect CFI.
  * Non-profiled boundaries prevent late wrong-path suffix resolves from refilling it.
  */
class HintTraceResolved extends Bundle {
  val target = UInt(47.W)
  val offset = UInt(6.W)
  val isRVC = Bool()
  val eligible = Bool()
  val valid = Bool()
  val baselineMiss = Bool()
  val generation = Bool()
}

object HintTracePacking {
  def discovery(event: HintTraceCommit): UInt = {
    val delta = ((event.actualPc.asSInt - event.queryStartPc.asSInt) >> 1).asUInt
    Cat(0.U(2.W), delta(6, 0), event.actualPc(47, 1))
  }
  def detail(event: HintTraceFoldEvent): UInt = {
    val delta = ((event.actualPc.asSInt - event.queryStartPc.asSInt) >> 1).asUInt
    Cat(0.U(3.W), 0.U(1.W), event.queryValid, event.baselineMiss,
      delta(6, 0),
      Mux(event.queryValid, event.ret, 0.U(28.W)),
      Mux(event.queryValid, event.call, 0.U(28.W)),
      Mux(event.queryValid, event.lhr, 0.U(28.W)),
      Mux(event.queryValid, event.ith, 0.U(28.W)), event.actualTarget(47, 1), event.pcId)
  }
}

class HintTracePacket(implicit p: Parameters) extends XSBundle {
  val data = UInt(176.W)
  // Identity is simulation-only; production storage remains exactly 16 x 22B.
  val ftqTag = Option.when(!env.FPGAPlatform)(UInt(7.W))
  val queryCycle = Option.when(!env.FPGAPlatform)(UInt(64.W))
}

/** Single FTQ commit producer. Full drops trace, never stalls the frontend. */
class HintTraceQueue(implicit p: Parameters) extends BpuModule {
  val io = IO(new Bundle {
    val clear = Input(Bool())
    val collect = Input(Bool())
    val discovery = Input(Bool())
    val stop = Input(Bool())
    val discoveryEvent = Input(Valid(new HintTraceCommit))
    val detailEvent = Input(Valid(new HintTraceFoldEvent))
    val out = Decoupled(new HintTracePacket)
    val overflow = Output(Bool())
    val stopped = Output(Bool())
    val occupancy = Output(UInt(5.W))
    val discoveryMissingStart = Output(UInt(32.W))
  })
  val queue = Module(new Queue(new HintTracePacket, 16, pipe = true, flow = false))
  val overflow = RegInit(false.B)
  val discoveryMissingStart = RegInit(0.U(32.W))
  val stopping = RegInit(false.B)
  val ended = RegInit(false.B)
  val sample = Mux(io.discovery,
    io.discoveryEvent.valid && io.discoveryEvent.bits.eligible && io.discoveryEvent.bits.baselineMiss && io.discoveryEvent.bits.queryValid,
    io.detailEvent.valid && io.detailEvent.bits.eligible)
  val accept = io.collect && !stopping && !io.stop && !io.clear && sample
  // Final follows all accepted records. It is not a branch sample.
  when(io.collect && io.discovery && !stopping && !io.stop && !io.clear &&
    io.discoveryEvent.valid && io.discoveryEvent.bits.eligible &&
    io.discoveryEvent.bits.baselineMiss && !io.discoveryEvent.bits.queryValid) {
    discoveryMissingStart := discoveryMissingStart + 1.U
  }
  val finalPending = stopping && !ended && !io.clear
  queue.io.enq.valid := accept || finalPending
  queue.io.enq.bits.data := Mux(finalPending,
    Mux(io.discovery, (BigInt(1) << 54).U(176.W), (BigInt(1) << 172).U(176.W)),
    Mux(io.discovery, HintTracePacking.discovery(io.discoveryEvent.bits).pad(176),
      HintTracePacking.detail(io.detailEvent.bits)))
  queue.io.enq.bits.ftqTag.foreach(_ := Mux(finalPending, 0.U,
    Mux(io.discovery, io.discoveryEvent.bits.ftqTag, io.detailEvent.bits.ftqTag)))
  queue.io.enq.bits.queryCycle.foreach(_ := Mux(finalPending, 0.U,
    Mux(io.discovery, io.discoveryEvent.bits.queryCycle.get, io.detailEvent.bits.queryCycle.get)))
  io.out <> queue.io.deq
  when(accept && !queue.io.enq.ready) { overflow := true.B }
  when(accept) {
    val pc = Mux(io.discovery, io.discoveryEvent.bits.actualPc, io.detailEvent.bits.actualPc)
    val start = Mux(io.discovery, io.discoveryEvent.bits.queryStartPc, io.detailEvent.bits.queryStartPc)
    val delta = (pc.asSInt - start.asSInt) >> 1
    assert(!pc(0) && !start(0))
    assert(delta >= (-64).S && delta <= 63.S)
  }
  when(io.stop) { stopping := true.B }
  when(finalPending && queue.io.enq.fire) { ended := true.B }
  // clear is a phase transition, after previously accepted output is drained.
  when(io.clear) {
    assert(queue.io.count === 0.U)
    overflow := false.B
    discoveryMissingStart := 0.U
    stopping := false.B
    ended := false.B
  }
  io.overflow := overflow
  io.stopped := ended && queue.io.count === 0.U
  io.occupancy := queue.io.count
  io.discoveryMissingStart := discoveryMissingStart
}
