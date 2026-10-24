package xiangshan.frontend.ftq

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec

/** Exact production adapter conversion, with the same 48-bit unguarded PC width. */
class HintResolveCoordinatesTestTop extends Module {
  val io = IO(new Bundle {
    val windowStart = Input(UInt(48.W))
    val ftqOffset = Input(UInt(6.W))
    val internalPc = Output(UInt(64.W))
  })
  io.internalPc := HintResolveCoordinates.internalPc(io.windowStart, io.ftqOffset, 1)
}

class HintResolveCoordinatesTest extends AnyFlatSpec with ChiselSim {
  behavior of "Resident FTQ resolve final-halfword coordinates"
  it should "preserve backend and IFU offset identity across instruction sizes and block boundaries" in {
    simulate(new HintResolveCoordinatesTestTop) { m =>
      m.io.windowStart.poke(0.U); m.io.ftqOffset.poke(0.U)
      m.reset.poke(true.B); m.clock.step(2); m.reset.poke(false.B)
      // (window, offset, actual instruction first PC, size, expected internal last halfword)
      val rows = Seq(
        ("32bit split offset zero", BigInt(0x1002), 0, BigInt(0x1000), 4, BigInt(0x1002)),
        ("32bit offset three", BigInt(0x185eac), 3, BigInt(0x185eb0), 4, BigInt(0x185eb2)),
        ("RVC offset zero", BigInt(0x1002), 0, BigInt(0x1002), 2, BigInt(0x1002)),
        ("RVC offset three", BigInt(0x1002), 3, BigInt(0x1008), 2, BigInt(0x1008)),
        ("32bit non-4B-aligned", BigInt(0x1000), 2, BigInt(0x1002), 4, BigInt(0x1004)),
        ("cross 32B", BigInt(0x101e), 1, BigInt(0x101e), 4, BigInt(0x1020)),
        ("cross 64B", BigInt(0x103e), 1, BigInt(0x103e), 4, BigInt(0x1040)),
        ("RVC across 64B", BigInt(0x103e), 2, BigInt(0x1042), 2, BigInt(0x1042)),
        ("upper canonical address", BigInt("ffffffff103e", 16), 1,
          BigInt("ffffffff103e", 16), 4, BigInt("ffffffffffff1040", 16))
      )
      for ((name, window, offset, firstPc, size, expected) <- rows) {
        // Actual instruction first PC is diagnostic; the production input is the window base.
        val actualFirstPc = firstPc
        m.io.windowStart.poke(window.U); m.io.ftqOffset.poke(offset.U)
        m.io.internalPc.expect(expected.U)
        println(s"HINT_RESOLVE_COORDINATE name=$name actual_first_pc=${actualFirstPc.toString(16)} window=${window.toString(16)} size=$size offset=$offset internal=${m.io.internalPc.peek().litValue.toString(16)}")
      }
      println("HINT_RESOLVE_COORDINATES_REAL_RTL_PASS resident_window_base=1 offset0_3=1 rvc=1 halfword_aligned=1 cross32_64=1 canonical=1")
    }
  }
}
