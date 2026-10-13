package xiangshan.frontend.bpu.hint

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import top.BackendV2NoRVVNoLLCConfig
import xiangshan.{XSCoreParamsKey, XSTileKey}

/** Real production single-producer FIFO/packing, no test-side queue model. */
class HintTraceTest extends AnyFlatSpec with ChiselSim {
  private val config=new BackendV2NoRVVNoLLCConfig
  implicit val params: org.chipsalliance.cde.config.Parameters=config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head
  }
  private def zero(x:Data):Unit=x match {
    case b:Bool=>b.poke(false.B)
    case u:UInt=>u.poke(0.U)
    case v:Vec[_]=>v.getElements.foreach(zero)
    case r:Record=>r.elements.values.foreach(zero)
    case _=>throw new IllegalArgumentException("unhandled trace field "+x)
  }
  behavior of "Commit trace compact format and bounded transport"
  it should "encode exact 6B and 22B records and stop only after draining a final sentinel" in {
    simulate(new HintTraceQueue) { m =>
      m.io.clear.poke(false.B);m.io.collect.poke(false.B);m.io.discovery.poke(true.B)
      m.io.stop.poke(false.B);m.io.discoveryEvent.valid.poke(false.B)
      m.io.detailEvent.valid.poke(false.B);zero(m.io.discoveryEvent.bits);zero(m.io.detailEvent.bits)
      m.io.out.ready.poke(false.B);m.reset.poke(true.B);m.clock.step(3);m.reset.poke(false.B)
      def clear(discovery:Boolean):Unit={
        m.io.occupancy.expect(0.U);m.io.clear.poke(true.B);m.clock.step()
        m.io.clear.poke(false.B);m.io.discovery.poke(discovery.B)
        m.io.stop.poke(false.B);m.io.out.ready.poke(false.B);m.io.collect.poke(true.B)
      }
      def drain(expected:Seq[BigInt]):Unit={
        m.io.discoveryEvent.valid.poke(false.B);m.io.detailEvent.valid.poke(false.B)
        m.io.stop.poke(true.B);m.clock.step();m.io.stop.poke(false.B)
        m.io.out.ready.poke(true.B)
        for(word<-expected){
          m.io.out.valid.expect(true.B);m.io.out.bits.data.expect(word.U)
          val kind=if(m.io.discovery.peek().litToBoolean)"discovery" else "detail"
          println(s"HINT_TRACE_OBSERVED kind=$kind word=${m.io.out.bits.data.peek().litValue.toString(16)}")
          m.clock.step()
        }
        val finalWord=BigInt(1)<<(if(m.io.discovery.peek().litToBoolean)47 else 172)
        var wait=0
        while(!m.io.out.valid.peek().litToBoolean && wait<4){m.clock.step();wait+=1}
        m.io.out.valid.expect(true.B);m.io.out.bits.data.expect(finalWord.U)
        val kind=if(m.io.discovery.peek().litToBoolean)"discovery" else "detail"
        println(s"HINT_TRACE_OBSERVED kind=$kind word=${m.io.out.bits.data.peek().litValue.toString(16)}")
        m.clock.step()
        m.io.stopped.expect(true.B);m.io.occupancy.expect(0.U)
      }
      clear(true)
      val pc=BigInt("123456789abc",16)
      m.io.discoveryEvent.bits.actualPc.poke(pc.U);m.io.discoveryEvent.bits.eligible.poke(true.B)
      m.io.discoveryEvent.bits.baselineMiss.poke(false.B);m.io.discoveryEvent.valid.poke(true.B)
      m.clock.step();m.io.occupancy.expect(0.U) // Discovery collects target misses only.
      m.io.discoveryEvent.bits.baselineMiss.poke(true.B);m.clock.step()
      m.io.discoveryEvent.valid.poke(false.B);m.io.out.bits.data.expect((pc>>1).U)
      drain(Seq(pc>>1));m.io.overflow.expect(false.B)

      clear(false)
      val event=m.io.detailEvent.bits
      event.eligible.poke(true.B);event.pcId.poke(13.U);event.actualTarget.poke(pc.U)
      event.actualPc.poke(0x1000.U);event.queryStartPc.poke(0x100a.U) // delta=-5
      event.queryValid.poke(true.B);event.baselineMiss.poke(true.B)
      val contexts=Vector(BigInt("1234567",16),BigInt("78",16),BigInt("3456789",16),BigInt("456789a",16))
      event.ith.poke(contexts(0).U);event.lhr.poke(contexts(1).U)
      event.call.poke(contexts(2).U);event.ret.poke(contexts(3).U)
      val packed=BigInt(13)|((pc>>1)<<4)|(contexts(0)<<51)|(contexts(1)<<79)|
        (contexts(2)<<107)|(contexts(3)<<135)|(BigInt(123)<<163)|(BigInt(1)<<170)|(BigInt(1)<<171)
      m.io.detailEvent.valid.poke(true.B);m.clock.step()
      event.queryValid.poke(false.B);event.baselineMiss.poke(false.B);m.clock.step()
      val invalid=BigInt(13)|((pc>>1)<<4)|(BigInt(123)<<163)
      drain(Seq(packed,invalid));m.io.overflow.expect(false.B)
      println("HINT_TRACE_COMPACT_REAL_RTL_PASS little_endian_bits=1 discovery_bytes=6 detail_bytes=22 delta=-5 query_invalid_zero_context_real_delta=1 final_drained=1")

      clear(false);event.queryValid.poke(true.B);event.baselineMiss.poke(true.B)
      m.io.detailEvent.valid.poke(true.B)
      for(i<-0 until 16){event.pcId.poke((i&15).U);m.clock.step();m.io.occupancy.expect((i+1).U)}
      event.pcId.poke(0.U);m.clock.step();m.io.occupancy.expect(16.U);m.io.overflow.expect(true.B)
      // The seventeenth producer has no ready input: only a sticky failure flag.
      val records=Vector.tabulate(16)(i=>(packed & ~BigInt(15))|BigInt(i))
      drain(records);m.io.overflow.expect(true.B)
      clear(false);m.io.overflow.expect(false.B)
      // clear wins over collect/sample and stop on the same legal empty edge.
      m.io.detailEvent.valid.poke(true.B);m.io.stop.poke(true.B);m.io.clear.poke(true.B)
      m.clock.step();m.io.occupancy.expect(0.U);m.io.overflow.expect(false.B)
      m.io.clear.poke(false.B);m.io.stop.poke(false.B);m.io.detailEvent.valid.poke(false.B)
      m.clock.step(2);m.io.occupancy.expect(0.U);m.io.stopped.expect(false.B)
      println("HINT_TRACE_FIFO16_REAL_RTL_PASS capacity=16 overflow_sticky=1 final_after_full=1 no_producer_backpressure=1 clear_after_drain=1")
    }
  }
}
