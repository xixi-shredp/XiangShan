package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.BackendV2NoRVVNoLLCConfig
import xiangshan.{XSCoreParamsKey, XSTileKey}

/** Elaboration gate on the actual production Bundle fields, not a shadow schema. */
class HintMetadataLayoutProbe(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val snapshot = Input(new HintSnapshot)
    val record = Input(new HintRecord)
    val confEvent = Input(new HintConfEvent)
    val cam = Input(new HintCamEntry)
    val snapshotBits = Output(UInt(8.W))
    val recordBits = Output(UInt(8.W))
    val confEventBits = Output(UInt(8.W))
    val camBits = Output(UInt(8.W))
    val camWord = Output(UInt(58.W))
  })
  private def widths(record: Record): Map[String, Int] =
    record.elements.map { case (name, field) => name -> field.getWidth }.toMap

  require(widths(io.snapshot) == Map(
    "ithHead" -> 7, "callHead" -> 7, "retHead" -> 7,
    "callLow12" -> 12, "retLow12" -> 12),
    "global snapshot must contain only three heads and two low windows")
  require(widths(io.record) == Map(
    "set" -> 11, "way" -> 1, "valid" -> 1, "conf" -> 2,
    "lhrHead" -> 80, "lhrTarget" -> 2, "lhrSel" -> 4),
    "Hint queue must contain exactly the approved recovery/training fields")
  require(io.record.lhrHead.length == 16 && io.record.lhrHead.getElements.forall(_.getWidth == 5),
    "local heads must be sixteen five-bit bit offsets")
  require(io.snapshot.getWidth == 45 && io.record.getWidth == 101,
    "metadata widths must be 45/101 bits without padding or observation payload")
  require(widths(io.confEvent) == Map("setway" -> 12, "conf" -> 2) &&
    io.confEvent.getWidth == 14,
    "confidence FIFO payload must be setway12+conf2, with validity implicit in occupancy")
  require(widths(io.cam) == Map("pc" -> 47, "mask" -> 4, "valid" -> 1,
    "position" -> 5, "isCall" -> 1) && io.cam.getWidth == 58,
    "CAM must append the native final-halfword position and call flag to the old 52-bit ABI")

  io.snapshotBits := io.snapshot.getWidth.U
  io.recordBits := io.record.getWidth.U
  io.confEventBits := io.confEvent.getWidth.U
  io.camBits := io.cam.getWidth.U
  io.camWord := io.cam.asUInt
}

class HintMetadataLayoutTest extends AnyFlatSpec with ChiselSim {
  private val config = new BackendV2NoRVVNoLLCConfig
  implicit val params: Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
  }

  behavior of "Production Hint metadata layout"
  it should "elaborate exactly the approved 45-bit snapshot and 101-bit queue payload" in {
    simulate(new HintMetadataLayoutProbe) { top =>
      top.io.snapshotBits.expect(45.U)
      top.io.recordBits.expect(101.U)
      top.io.confEventBits.expect(14.U)
      top.io.camBits.expect(58.U)
      top.io.cam.pc.poke(0x12345.U); top.io.cam.mask.poke(10.U)
      top.io.cam.valid.poke(true.B); top.io.cam.position.poke(21.U)
      top.io.cam.isCall.poke(true.B)
      val expected = BigInt(0x12345) | (BigInt(10) << 47) | (BigInt(1) << 51) |
        (BigInt(21) << 52) | (BigInt(1) << 57)
      top.io.camWord.expect(expected.U)
    }
    println("HINT_METADATA_LAYOUT_PASS snapshot_bits=45 record_bits=101 conf_event_bits=14 cam_bits=58 exact_fields=1 cam_abi_bit_order=1")
  }
}
