// See README.md for license details.

package CoLT
/*
import chisel3._
import chisel3.tester._
import chiseltest.RawTester._
import chisel3.testers._
import org.scalatest.FreeSpec
import chisel3.experimental.BundleLiterals._


import chisel3._
import chisel3.util._
import chisel3.tester._
import chisel3.iotesters.PeekPokeTester
import chiseltest._
import chiseltest.experimental._
import org.scalatest.{Matchers, FlatSpec}
import chisel3.testers.BasicTester

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals._
import chisel3.tester._
import chisel3.tester.RawTester.test
*/

import chisel3._
import chisel3.tester._
import org.scalatest.FreeSpec
import chisel3.experimental.BundleLiterals._



/*
  * This is a trivial example of how to run this Specification
  * From within sbt use:
  * {{{
  * testOnly gcd.GcdDecoupledTester
  * }}}
  * From a terminal shell use:
  * {{{
  * sbt 'testOnly gcd.GcdDecoupledTester'
  * }}}
 

object CoLT_FA_test extends App {
  chisel3.iotesters.Driver(() => new CoLT_FA()) { c => new CoLT_FA_test }
  //chisel3.testers.TesterDriver( () => new CoLT_FA_test)
}
   */
class CoLT_FA_test extends FreeSpec with ChiselScalatestTester{
    "CoLT should read appropriate values" in {
        test (new CoLT_FA()) { c => 
        /*
        // Read non-existent entry
        c.io.readEnable.poke(true.B)
        c.io.readAddress.poke(76.U)
        c.clock.step(2)
        c.io.validData.expect(false.B)
        c.io.retAddress.expect (0.U)
        
        
        // Write address 12 address at 3
        c.io.readEnable.poke(false.B)
        c.io.writeEnable.poke(true.B)
        c.io.writeAddress.poke(12.U)
        c.io.writeData.poke(3.U)
        c.clock.step(2)
        c.io.retAddress.expect (0.U)
        
        */
        // Read non-existent entry
        c.io.readEnable.poke(true.B)
        c.io.readAddress.poke(37.U)
        c.clock.step(3)
        c.io.retAddress.expect(3.U)
        c.io.validData.expect(true.B)
            
        c.io.readEnable.poke(true.B)
        c.io.readAddress.poke(91.U)
        c.clock.step(3)
        c.io.validData.expect(true.B)
        c.io.retAddress.expect(1.U)
        
        c.io.readEnable.poke(true.B)
        c.io.readAddress.poke(99.U)
        c.clock.step(3)
        c.io.validData.expect(true.B)
        c.io.retAddress.expect(2.U)   
        
        println("Success!")
        }
    }
}