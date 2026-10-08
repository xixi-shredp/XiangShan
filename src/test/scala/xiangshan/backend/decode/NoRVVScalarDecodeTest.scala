package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.BackendV2NoRVVNoLLCConfig
import xiangshan._

class NoRVVScalarDecodeHarness(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val instr = Input(UInt(32.W))
    val illegal = Output(Bool())
    val fu = Output(UInt(xiangshan.backend.fu.FuType.num.W))
  })
  val dec = Module(new DecodeUnit)
  dec.io.enq.decodeInUop := 0.U.asTypeOf(dec.io.enq.decodeInUop)
  dec.io.enq.decodeInUop.instr := io.instr
  dec.io.enq.vstart := 0.U
  dec.io.csrCtrl := 0.U.asTypeOf(dec.io.csrCtrl)
  dec.io.fromCSR := 0.U.asTypeOf(dec.io.fromCSR)
  dec.io.fromCSR.illegalInst.vsIsOff := true.B
  io.illegal := dec.io.deq.decodedInst.exceptionVec(ExceptionNO.illegalInstr)
  io.fu := dec.io.deq.decodedInst.fuType.asUInt
}

class NoRVVScalarDecodeTest extends AnyFlatSpec with ChiselSim {
  private val cfg = new BackendV2NoRVVNoLLCConfig
  implicit val p: Parameters = cfg.alterPartial {
    case XSCoreParamsKey => cfg(XSTileKey).head
  }
  behavior of "BackendV2 no-RVV scalar decoder"
  it should "retain every scalar FP decode and route each to an instantiated scalar FU" in {
    val rows = FpDecode.table ++ VecDecoder.scalarTable ++ ZfaDecode.table
    val available = p(XSCoreParamsKey).backendParams.allExuParams.flatMap(_.fuConfigs).map(_.fuType.ohid).toSet
    assert(VecDecoder.scalarTable.length == 90)
    for ((instruction, controls) <- rows) {
      assert(available.contains(controls(3).value), s"FP opcode ${instruction.value.toString(16)} has no instantiated FU")
    }
    simulate(new NoRVVScalarDecodeHarness) { m =>
      for ((instruction, controls) <- rows) {
        m.io.instr.poke(instruction.value.U)
        m.clock.step()
        m.io.illegal.expect(false.B)
        m.io.fu.expect(controls(3).value.U)
      }
      val vectorRows = VecDecoder.vset ++ VecDecoder.opivv ++ VecDecoder.opfvv ++ VecDecoder.vls
      for ((instruction, _) <- vectorRows) {
        m.io.instr.poke(instruction.value.U)
        m.clock.step()
        m.io.illegal.expect(true.B)
      }
    }
  }
}
