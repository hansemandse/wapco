package norcas

import scala.util.Random

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import chiselverify.approximation._
import chiselverify.approximation.Metrics._

class Showcase extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  class ApproximateAdder(width: Int, approxWidth: Int) extends Module {
    val io = IO(new Bundle {
      val a    = Input(UInt(width.W))
      val b    = Input(UInt(width.W))
      val s    = Output(UInt(width.W))
      val cout = Output(Bool())
    })
    val aSum = (io.a(width-1, approxWidth) +& io.b(width-1, approxWidth)) ## (io.a(approxWidth-1, 0) ^ io.b(approxWidth-1, 0))
    io.s    := aSum(width-1, 0)
    io.cout := aSum(width)
  }

  "Approximate adder" should "verify with software model" in {
    test(new ApproximateAdder(32, 8)) { dut =>
      val er = new ErrorReporter(
        constrain(dut.io.s, RED(.1), MRED(.025)),
        constrain(dut.io.cout, ER(.01))
      )
      val mask = (BigInt(1) << 32) - 1
      val rng  = new Random(42)
      for (_ <- 0 until 10000) {
        val (a, b) = (BigInt(32, rng), BigInt(32, rng))
        val (cout, s) = (((a + b) >> 32) & 1, (a + b) & mask)
        dut.io.a.poke(a.U)
        dut.io.b.poke(b.U)
        er.sample(Map(dut.io.cout -> cout, dut.io.s -> s))
      }
      er.verify() should be (true)
      println(er.report())
    }
  }
}
