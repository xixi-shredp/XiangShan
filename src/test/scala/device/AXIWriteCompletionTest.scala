package device

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec

class AXIWriteCompletionTest extends AnyFlatSpec with ChiselSim {
  behavior of "dynamic AXI write completion"
  private def init(m: AXIWriteCompletion): Unit = {
    m.io.dataId.poke(0.U); m.io.queryId.poke(0.U); m.io.accept.valid.poke(false.B); m.io.accept.bits.poke(0.U)
    m.io.wLast.valid.poke(false.B); m.io.wLast.bits.poke(0.U)
    m.io.dram.valid.poke(false.B); m.io.dram.bits.poke(0.U)
    m.io.retire.valid.poke(false.B); m.io.retire.bits.poke(0.U)
    m.io.b.ready.poke(false.B); m.clock.step(2)
  }
  private def accept(m: AXIWriteCompletion, id: Int): Unit = {
    m.io.queryId.poke(id.U); m.io.canAccept.expect(true.B)
    m.io.accept.bits.poke(id.U); m.io.accept.valid.poke(true.B)
    m.clock.step(); m.io.accept.valid.poke(false.B); m.io.canAccept.expect(false.B)
  }
  private def last(m: AXIWriteCompletion, id: Int): Unit = {
    m.io.wLast.bits.poke(id.U); m.io.wLast.valid.poke(true.B)
    m.io.wLast.ready.expect(true.B); m.clock.step(); m.io.wLast.valid.poke(false.B)
  }
  private def dram(m: AXIWriteCompletion, id: Int): Unit = {
    m.io.dram.bits.poke(id.U); m.io.dram.valid.poke(true.B)
    m.clock.step(); m.io.dram.valid.poke(false.B)
  }
  private def pop(m: AXIWriteCompletion, id: Int): Unit = {
    m.io.b.valid.expect(true.B); m.io.b.bits.expect(id.U)
    m.io.b.ready.poke(true.B); m.clock.step(); m.io.b.ready.poke(false.B)
    // Endpoint B can still sit in the real bQueue after the join dequeues it.
    m.io.queryId.poke(id.U); m.io.canAccept.expect(false.B)
    m.io.retire.bits.poke(id.U); m.io.retire.valid.poke(true.B)
    m.clock.step(); m.io.retire.valid.poke(false.B); m.io.canAccept.expect(true.B)
  }
  it should "wait for W.last after an early DRAM callback and retain B under backpressure" in {
    simulate(new AXIWriteCompletion(4)) { m =>
      init(m); accept(m,1); dram(m,1); m.io.b.valid.expect(false.B)
      last(m,1); m.io.b.valid.expect(true.B); m.io.b.bits.expect(1.U)
      m.clock.step(8); m.io.b.valid.expect(true.B); m.io.b.bits.expect(1.U)
      pop(m,1); m.io.active.expect(false.B)
    }
  }
  it should "join out-of-order DRAM IDs with ordered W completion" in {
    simulate(new AXIWriteCompletion(4)) { m =>
      init(m); accept(m,0); accept(m,2); last(m,0); last(m,2)
      dram(m,2); m.io.b.valid.expect(false.B)
      dram(m,0); pop(m,0); pop(m,2); m.io.active.expect(false.B)
    }
  }
  it should "hold every ID at tracker capacity until the actual B handshake" in {
    simulate(new AXIWriteCompletion(4)) { m =>
      init(m); (0 until 4).foreach(accept(m,_))
      (0 until 4).foreach { id => m.io.queryId.poke(id.U); m.io.canAccept.expect(false.B) }
      (0 until 4).foreach(last(m,_)); (0 until 4).reverse.foreach(dram(m,_))
      m.clock.step(5); (0 until 4).foreach(pop(m,_)); m.io.active.expect(false.B)
    }
  }
  it should "delay the write-complete barrier used before a dependent read" in {
    simulate(new AXIWriteCompletion(4)) { m =>
      init(m); accept(m,3); last(m,3); m.clock.step(12)
      // Cacheless home's RAW barrier is released by this B, never by W.last.
      m.io.b.valid.expect(false.B); dram(m,3); m.io.b.valid.expect(true.B); pop(m,3)
    }
  }
  it should "retain Xbar-remapped high IDs with an independent bounded capacity" in {
    simulate(new AXIWriteCompletion(2,14)) { m =>
      init(m); accept(m,70); accept(m,8191)
      m.io.queryId.poke(1.U); m.io.canAccept.expect(false.B)
      last(m,70); last(m,8191); dram(m,8191); m.io.b.valid.expect(false.B)
      dram(m,70); pop(m,70); pop(m,8191); m.io.active.expect(false.B)
    }
  }
}
