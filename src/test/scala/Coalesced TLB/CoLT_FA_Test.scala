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
      c.io.readAddress.bits.poke(37.U)
      c.io.readAddress.valid.poke(true.B)
      c.io.retAddress.ready.poke(true.B)
      c.clock.step(2)
      c.io.retAddress.bits.expect(3.U)
      c.io.retAddress.valid.expect(true.B)

          
      c.io.readEnable.poke(true.B)
      c.io.readAddress.bits.poke(91.U)
      c.io.readAddress.valid.poke(true.B)
      c.io.retAddress.ready.poke(true.B)
      c.clock.step(2)
      c.io.retAddress.bits.expect(1.U)
      c.io.retAddress.valid.expect(true.B)
      
      
      c.io.readEnable.poke(true.B)
      c.io.readAddress.bits.poke(99.U)
      c.io.readAddress.valid.poke(true.B)
      c.io.retAddress.ready.poke(true.B)
      c.clock.step(2)
      c.io.retAddress.valid.expect(true.B)
      c.io.retAddress.bits.expect(2.U)   
      /*



      c.io.readAddress.initSource()
      c.io.readAddress.setSourceClock(c.clock)
      c.io.writeAddress.initSource()
      c.io.writeAddress.setSourceClock(c.clock)
      c.io.retAddress.initSink()
      c.io.retAddress.setSinkClock(c.clock)

      c.io.readEnable.poke(true.B)
      c.io.readAddress.enqueueNow(37.U)
      //c.clock.step()
      c.io.retAddress.expectDequeueNow(3.U)

*/
      println("Success!")
      }
  }
}