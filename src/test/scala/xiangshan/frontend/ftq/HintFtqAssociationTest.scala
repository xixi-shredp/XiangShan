package xiangshan.frontend.ftq

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.BackendV2NoRVVNoLLCConfig
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.frontend.bpu.hint.HintOwner

/** Component gate on production ownership arithmetic and actual FTQ entry schema.
  * The small write fixture is not a substitute for whole-FTQ concurrency tests.
  */
class HintFtqAssociationProbe(implicit p: Parameters) extends Module {
  private val size = p(XSCoreParamsKey).frontendParameters.ftqParameters.FtqSize
  val io = IO(new Bundle {
    val write = Input(Bool())
    val writeIndex = Input(UInt(log2Ceil(size).W))
    val association = Input(Valid(UInt(4.W)))
    val correction = Input(Bool())
    val oldest = Input(new FtqPtr)
    val newest = Input(new FtqPtr)
    val query = Input(new FtqPtr)
    val redirectValid = Input(Bool())
    val redirectNewPtr = Input(new FtqPtr)
    val s3Override = Input(Bool())
    val s3Ptr = Input(new FtqPtr)
    val owners = Output(Vec(size, new HintOwner))
    val killed = Output(Vec(size, Bool()))
    val queryActive = Output(Bool())
  })
  private val entries = RegInit(VecInit(Seq.fill(size)(0.U.asTypeOf(new FtqEntry))))
  require(entries.head.elements.keySet == Set("startPc", "target", "taken", "endPosition", "hint"),
    "FTQ entry may add only the approved Hint association, without target/payload copies")
  require(entries.head.hint.getWidth == 5 &&
    entries.head.hint.bits.getWidth == 4 && entries.head.hint.valid.getWidth == 1)

  // Mirrors only the existing FTQ S1 association write enable. All arithmetic
  // below is the production helper used by FTQ and Hint, not a test reimplementation.
  when(io.write && !io.correction) {
    entries(io.writeIndex).hint := io.association
  }
  for (i <- 0 until size) {
    val ptr = HintOwnership.residentPtr(i.U, io.oldest)
    io.owners(i).valid := HintOwnership.active(ptr, io.oldest, io.newest) && entries(i).hint.valid
    io.owners(i).ptr := ptr
    io.owners(i).index := entries(i).hint.bits
    io.killed(i) := io.owners(i).valid && HintOwnership.killed(ptr,
      io.redirectValid, io.redirectNewPtr, io.s3Override, io.s3Ptr)
  }
  io.queryActive := HintOwnership.active(io.query, io.oldest, io.newest)
}

class HintFtqAssociationTest extends AnyFlatSpec with ChiselSim {
  private val config = new BackendV2NoRVVNoLLCConfig
  implicit val params: Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
  }
  behavior of "Production FTQ Hint ownership"
  it should "map wrap generations, retain selected redirect prefixes and initialize recycled slots" in {
    simulate(new HintFtqAssociationProbe) { top =>
      def ptr(p: FtqPtr, serial: Int): Unit = {
        p.value.poke((serial & 63).U)
        p.flag.poke(((serial & 64) != 0).B)
      }
      def write(index: Int, payload: Int, valid: Boolean = true, correction: Boolean = false): Unit = {
        top.io.write.poke(true.B); top.io.writeIndex.poke(index.U)
        top.io.association.valid.poke(valid.B); top.io.association.bits.poke(payload.U)
        top.io.correction.poke(correction.B); top.clock.step(); top.io.write.poke(false.B)
        top.io.correction.poke(false.B)
      }
      top.io.write.poke(false.B); top.io.writeIndex.poke(0.U)
      top.io.association.valid.poke(false.B); top.io.association.bits.poke(0.U)
      top.io.correction.poke(false.B); top.io.redirectValid.poke(false.B)
      top.io.s3Override.poke(false.B)
      ptr(top.io.oldest, 60); ptr(top.io.newest, 68); ptr(top.io.query, 60)
      ptr(top.io.redirectNewPtr, 66); ptr(top.io.s3Ptr, 64)
      top.reset.poke(true.B); top.clock.step(3); top.reset.poke(false.B)
      write(60, 0); write(0, 1); write(1, 2); write(2, 3); write(3, 15)
      top.io.owners(60).valid.expect(true.B); top.io.owners(60).ptr.flag.expect(false.B)
      top.io.owners(0).valid.expect(true.B); top.io.owners(0).ptr.flag.expect(true.B)
      top.io.owners(0).index.expect(1.U); top.io.owners(3).index.expect(15.U)
      top.io.owners(4).valid.expect(false.B)
      // The same physical slot's stale generation is never a live resolve.
      ptr(top.io.query, 0); top.io.queryActive.expect(false.B)
      ptr(top.io.query, 64); top.io.queryActive.expect(true.B)
      ptr(top.io.query, 68); top.io.queryActive.expect(false.B)

      // S3 keeps its own window and drops strictly younger owners.
      top.io.s3Override.poke(true.B)
      top.io.killed(60).expect(false.B); top.io.killed(0).expect(false.B)
      top.io.killed(1).expect(true.B); top.io.killed(3).expect(true.B)
      // Selected redirect wins over S3, including its new pointer.
      top.io.redirectValid.poke(true.B)
      top.io.killed(1).expect(false.B); top.io.killed(2).expect(true.B)
      top.io.killed(3).expect(true.B)
      top.io.redirectValid.poke(false.B); top.io.s3Override.poke(false.B)

      // Commit sees the pre-edge association; after the watermark advances the
      // slot ceases to own payload even though its physical five bits remain.
      ptr(top.io.query, 60); top.io.queryActive.expect(true.B)
      top.io.owners(60).valid.expect(true.B)
      top.clock.step(); ptr(top.io.oldest, 61)
      top.io.queryActive.expect(false.B); top.io.owners(60).valid.expect(false.B)

      // A recycled rejected admission clears the old association. Later accepted
      // reuse gets the new index, while S3 correction preserves that association.
      ptr(top.io.oldest, 124); ptr(top.io.newest, 125)
      write(60, 0, valid = false)
      top.io.owners(60).valid.expect(false.B)
      write(60, 5)
      top.io.owners(60).valid.expect(true.B); top.io.owners(60).ptr.flag.expect(true.B)
      top.io.owners(60).index.expect(5.U)
      write(60, 9, valid = false, correction = true)
      top.io.owners(60).valid.expect(true.B); top.io.owners(60).index.expect(5.U)
      ptr(top.io.query, 60); top.io.queryActive.expect(false.B)
      ptr(top.io.query, 124); top.io.queryActive.expect(true.B)
      // Empty FTQ and a full 64-entry interval have distinct flag semantics.
      ptr(top.io.newest, 124); top.io.owners(60).valid.expect(false.B)
      ptr(top.io.oldest, 60); ptr(top.io.newest, 124)
      top.io.owners(60).valid.expect(true.B); top.io.owners(60).ptr.flag.expect(false.B)
      top.io.owners(0).valid.expect(true.B); top.io.owners(0).ptr.flag.expect(true.B)
      println("HINT_FTQ_ASSOCIATION_PASS schema_extra_bits=5 generation_wrap=1 redirect_priority=1 commit_release=1 recycled_admission=1 s3_preserves=1 empty_full=1")
    }
  }
}
