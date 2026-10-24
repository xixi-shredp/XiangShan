package xiangshan.frontend.ftq

import chisel3._
import chisel3.util.Cat
import chisel3.reflect.DataMirror
import circt.stage.ChiselStage
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.cde.config.Parameters
import top.BackendV2NoRVVNoLLCConfig
import xiangshan.{DebugOptionsKey, XSCoreParamsKey, XSTileKey}
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import java.nio.file.{Files, Paths}

/** Keep every functional FTQ input/output observable, including Hint commit
  * trace. Ordinary BPU consumers do not read the training perfMeta payload, so
  * leave only that observation payload unconsumed, as in the closed frontend.
  * No functional input is tied off and no functional trace output is dropped.
  */
class HintFtqHardwareCone(implicit p: Parameters) extends Module {
  val ftq = Module(new Ftq)
  private def leaves(name: String, data: Data): Seq[(String, Data)] = data match {
    case record: Record => record.elements.toSeq.flatMap { case (key, value) => leaves(s"$name.$key", value) }
    case vector: Vec[_] => vector.getElements.zipWithIndex.flatMap { case (value, index) => leaves(s"$name.$index", value) }
    case _ => Seq(name -> data)
  }
  private val ports = leaves("io", ftq.io)
  private val inputs = ports.filter { case (_, data) => DataMirror.directionOf(data) == ActualDirection.Input }
  private val outputs = ports.filter { case (name, data) =>
    DataMirror.directionOf(data) == ActualDirection.Output && !name.startsWith("io.toBpu.train.bits.perfMeta.")
  }
  require(outputs.exists(_._1 == "io.toBpu.hintTraceCommit.valid"))
  require(outputs.exists(_._1 == "io.toBpu.hintTraceCommit.bits.actualTarget"))
  val io = IO(new Bundle {
    val stimulus = Input(UInt(inputs.map(_._2.getWidth).sum.W))
    val functional = Output(UInt(outputs.map(_._2.getWidth).sum.W))
  })
  private var offset = 0
  for ((_, data) <- inputs) {
    val width = data.getWidth
    data := io.stimulus(offset + width - 1, offset).asTypeOf(data)
    offset += width
  }
  io.functional := Cat(outputs.map(_._2.asUInt))
}

class HintPerfEliminationTest extends AnyFlatSpec {
  private val config = new BackendV2NoRVVNoLLCConfig
  implicit val params: Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
    case DebugOptionsKey => config(DebugOptionsKey).copy(FPGAPlatform = true)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, true, false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.NORMAL, 0)
  }
  behavior of "FTQ hardware observation elimination"
  it should "retain functional Hint trace while removing the performance queue" in {
    require(sys.env.get("H2P_HINT_DIAG").forall(_ != "1"))
    val sv = ChiselStage.emitSystemVerilog(new HintFtqHardwareCone,
      firtoolOpts = Array("-O=release", "--disable-all-randomization"))
    val directory = Paths.get(sys.env("HINT_PERF_NETLIST_DIR"))
    Files.createDirectories(directory)
    Files.writeString(directory.resolve("HintFtqHardwareCone.sv"), sv)
    val expectsOld = sys.env.get("HINT_EXPECT_PERF_QUEUE").contains("present")
    val hasPerfQueue = """(?m)\bperfQueue(?:_|\[|\b)""".r.findFirstIn(sv).nonEmpty
    assert(hasPerfQueue == expectsOld,
      s"performance storage presence=$hasPerfQueue expected=$expectsOld; functional Hint outputs stay observable")
    assert(sv.contains("module HintFtqHardwareCone"))
    println(s"HINT_PERF_ELIMINATION_PASS old_control=$expectsOld perf_queue_present=$hasPerfQueue functional_trace_observable=1")
  }
}
