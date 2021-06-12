// See README.md for license details.

package CoLT

import chisel3._
import chisel3.tester._
import org.scalatest.FreeSpec
import chisel3.experimental.BundleLiterals._

class CoLT_FA_test extends FreeSpec with ChiselScalatestTester{
  "CoLT should read appropriate values" in {
      test (new CoLT_FA()) { c => 
      c.io.fence_request.poke(false.B)
      c.io.fenceAddress.valid.poke(false.B)
      c.io.fenceAddress.bits.poke(4.U)

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
      c.io.readAddress.bits.poke(3350.U) //vpn=104
      c.io.readAddress.valid.poke(true.B)
      c.io.retAddress.ready.poke(true.B)
      c.clock.step(2)
      c.io.retAddress.valid.expect(false.B)
      c.io.retAddress.bits.expect(108.U) //miss

      c.io.writeEnable.poke(true.B)
      c.io.writeAddress.bits.poke(229.U)
      c.io.writeAddress.valid.poke(true.B)
      c.clock.step()
      c.io.retAddress.valid.expect(true.B)
      c.io.retAddress.bits.expect(229.U)
      c.clock.step(2)

      //Test write after TLB Miss
      c.io.writeEnable.poke(false.B)
      c.io.readEnable.poke(true.B)
      c.io.readAddress.bits.poke(3382.U) //vpn=104
      c.io.readAddress.valid.poke(true.B)
      c.io.retAddress.ready.poke(true.B)
      c.clock.step(2)
      c.io.retAddress.bits.expect(229.U)
      c.io.retAddress.valid.expect(false.B)
      
    

      c.io.writeEnable.poke(true.B)
      c.io.writeAddress.bits.poke(230.U)
      c.io.writeAddress.valid.poke(true.B)
      c.clock.step(3)
      c.io.retAddress.valid.expect(true.B)
      c.io.retAddress.bits.expect(230.U)

      //Test write after TLB Miss
      c.io.writeEnable.poke(false.B)
      c.io.readEnable.poke(true.B)
      c.io.readAddress.bits.poke(3414.U) //vpn=104
      c.io.readAddress.valid.poke(true.B)
      c.io.retAddress.ready.poke(true.B)
      c.clock.step(2)
 

      c.io.writeEnable.poke(true.B)
      c.io.writeAddress.bits.poke(231.U)
      c.io.writeAddress.valid.poke(true.B)
      c.clock.step(3)
      c.io.retAddress.valid.expect(true.B)
      c.io.retAddress.bits.expect(231.U)


      /*
      val readSeq = Seq (16,17,18,19,20,21,65,67) // VPN to miss
      val writeSeq = Seq(4,5,6,7,8,9,10,11,54)    // PPN to return from PTW
      for (i<-0 until 8){
        c.io.readEnable.poke(true.B)
        c.io.readAddress.bits.poke(readSeq(i).U)
        c.io.readAddress.valid.poke(true.B)
        c.io.retAddress.ready.poke(true.B)
        c.clock.step(2)

        c.io.writeEnable.poke(true.B)
        c.io.writeAddress.bits.poke(writeSeq(i).U)
        c.io.writeAddress.valid.poke(true.B)
        c.io.retAddress.ready.poke(true.B)

        c.clock.step(2)
      }
      */

      println("Success!")
      }
  }
}