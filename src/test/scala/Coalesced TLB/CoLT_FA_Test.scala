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
      c.io.readAddress.bits.poke(5376.U)
      c.io.readAddress.valid.poke(true.B)
      c.io.retAddress.ready.poke(true.B)
      c.clock.step(2)
      c.io.retAddress.bits.expect(59.U)
      c.io.retAddress.valid.expect(true.B)

          
      c.io.readEnable.poke(true.B)
      c.io.readAddress.bits.poke(5440.U)
      c.io.readAddress.valid.poke(true.B)
      c.io.retAddress.ready.poke(true.B)
      c.clock.step(2)
      c.io.retAddress.bits.expect(61.U)
      c.io.retAddress.valid.expect(true.B)
      
      
      c.io.readEnable.poke(true.B)
      c.io.readAddress.bits.poke(6752.U)
      c.io.readAddress.valid.poke(true.B)
      c.io.retAddress.ready.poke(true.B)
      c.clock.step(2)
      c.io.retAddress.valid.expect(true.B)
      c.io.retAddress.bits.expect(108.U)   

      
      //Test write after TLB Miss
      c.io.readEnable.poke(true.B)
      c.io.readAddress.bits.poke(99999.U)
      c.io.readAddress.valid.poke(true.B)
      c.io.retAddress.ready.poke(true.B)
      c.clock.step(2)
      c.io.retAddress.valid.expect(false.B)
      c.io.retAddress.bits.expect(108.U) //miss

      c.io.writeEnable.poke(false.B)
      c.io.writeAddress.bits.poke(5.U)
      c.io.writeAddress.valid.poke(true.B)
      c.clock.step()
      
      val writeSeq = Seq(4,5,6,8,9,10,11,54)
      for (i<-0 until 8){
        c.io.writeEnable.poke(true.B)
        c.io.writeAddress.bits.poke(writeSeq(i).U)
        c.io.writeAddress.valid.poke(true.B)
        c.clock.step(2)
      }
      c.io.retAddress.bits.expect(108.U)
      /*
      
      c.io.writeEnable.poke(true.B)
      c.io.writeAddress.bits.poke(4.U)
      c.io.writeAddress.valid.poke(true.B)
      c.clock.step(2)

      c.io.writeEnable.poke(true.B)
      c.io.writeAddress.bits.poke(5.U)
      c.io.writeAddress.valid.poke(true.B)
      c.clock.step(2)

      c.io.writeEnable.poke(true.B)
      c.io.writeAddress.bits.poke(6.U)
      c.io.writeAddress.valid.poke(true.B)
      c.clock.step(2)

      c.io.writeEnable.poke(true.B)
      c.io.writeAddress.bits.poke(9.U)
      c.io.writeAddress.valid.poke(true.B)
      c.clock.step(2)
      */
      

      //c.clock.step(10)
      println("Success!")
      }
  }
}