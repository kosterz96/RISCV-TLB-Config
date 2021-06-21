// See README.md for license details.

package CoLT

import chisel3._
import chisel3.tester._
import org.scalatest.FreeSpec
import chisel3.experimental.BundleLiterals._

class CoLT_FA_test extends FreeSpec with ChiselScalatestTester{
  "CoLT should read appropriate values" in {
      test (new CoLT_FA()) { c => 
      
      c.io.readEnable.poke(true.B)
      c.io.readAddress.bits.poke(168.U)
      c.io.readAddress.valid.poke(true.B)
      c.io.retAddress.ready.poke(true.B)
      c.clock.step(2)
      c.io.retAddress.bits.expect(59.U)
      c.io.retAddress.valid.expect(true.B)

          
      c.io.readEnable.poke(true.B)
      c.io.readAddress.bits.poke(170.U)
      c.io.readAddress.valid.poke(true.B)
      c.io.retAddress.ready.poke(true.B)
      c.clock.step(2)
      c.io.retAddress.bits.expect(61.U)
      c.io.retAddress.valid.expect(true.B)
      
      
      c.io.readEnable.poke(true.B)
      c.io.readAddress.bits.poke(211.U)
      c.io.readAddress.valid.poke(true.B)
      c.io.retAddress.ready.poke(true.B)
      c.clock.step(2)
      c.io.retAddress.valid.expect(true.B)
      c.io.retAddress.bits.expect(108.U)   

      //Test write after TLB Miss
      val readSeq =   Seq(55,  56, 57,  60,  61,  128,  129,  145,  219)  // request VPN
      val writeSeq =  Seq(4,    5,  6,   8,   9,   10,   11,   54,  116) // PTW result - PPNs
      for (i<-0 until readSeq.length){
        c.io.writeEnable.poke(false.B)       
        c.io.readEnable.poke(true.B)
        c.io.readAddress.bits.poke(readSeq(i).U)
        c.io.readAddress.valid.poke(true.B)
        c.io.retAddress.ready.poke(true.B)
        c.clock.step(2)

        c.io.writeEnable.poke(true.B)
        c.io.writeAddress.bits.poke(writeSeq(i).U)
        c.io.writeAddress.valid.poke(true.B)
        c.clock.step(4)

        c.io.retAddress.bits.expect(writeSeq(i).U)
      }

      println("Success!")
      }
  }
}