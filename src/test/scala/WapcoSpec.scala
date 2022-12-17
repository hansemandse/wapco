import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import approx.addition.{AdderIO, LOA, OFLOCA, GeAr}
import chiselverify.approximation._
import chiselverify.approximation.Metrics._

object TestConfiguration {
  val NumTests = 1000000
  val Width    = 32
  val Metrics  = Seq(ER(), MED(), SDED(), MRED())
}

class WapcoSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  import AdderConfigurations._
  import TestConfiguration._
  Seq(RCAParameters(0), CLAParameters(0), LOAParameters(0), OFLOCAParametersV1(0), 
      OFLOCAParametersV2(0), GeArParametersV1(0), GeArParametersV2(0))
    .forall(_ == Width) should be (true)

  // Convert a sequence of parameters to a comma-separated string form
  def params2paren(params: Seq[Int]): String = s"(${params.mkString(", ")})"

  // Generic exact adder
  class ExactAdder(width: Int) extends Module {
    val io = IO(new AdderIO(width))
    val sum  = Wire(UInt((width+1).W))
    sum     := io.a +& io.b + io.cin
    io.s    := sum(width-1, 0)
    io.cout := sum(width)
  }

  // Common top-level DUT with exact adder implemented
  abstract class DUT(width: Int) extends Module {
    val io = IO(new Bundle {
      val a   = Input(UInt(width.W))
      val b   = Input(UInt(width.W))
      val cin = Input(Bool())
      // Approximate outputs
      val sA    = Output(UInt(width.W))
      val coutA = Output(Bool())
      // Exact outputs
      val sE    = Output(UInt(width.W))
      val coutE = Output(Bool())
    })
    def approxAdder: Module
    val exactAdder = Module(new ExactAdder(width))
    exactAdder.io.a   := io.a
    exactAdder.io.b   := io.b
    exactAdder.io.cin := io.cin
    io.sE    := exactAdder.io.s
    io.coutE := exactAdder.io.cout
  }

  // Generate some random inputs to the adder and sample its registered outputs
  def generateMetrics[T <: DUT](dut: T): Unit = {
    val rng = new scala.util.Random(42)

    // Create an error reporter and track the sum output
    val er = new ErrorReporter(
      track(dut.io.sA, dut.io.sE, Metrics:_*)
    )

    // Apply NumTests random inputs and collect the outputs
    (0 until NumTests).foreach { _ =>
      dut.io.a.poke(BigInt(Width, rng).U)
      dut.io.b.poke(BigInt(Width, rng).U)
      dut.io.cin.poke(rng.nextBoolean().B)
      dut.clock.step() // advance time for visual changes in VCD
      er.sample()
    }

    // Generate an error report and print it
    println(er.report())
  }

  // Test LOA
  s"LOA${params2paren(LOAParameters)}" should "generate error metrics" in {
    class LOADUT extends DUT(Width) {
      val approxAdder = Module(new LOA(Width, LOAParameters(1)))
      approxAdder.io.a   := io.a
      approxAdder.io.b   := io.b
      approxAdder.io.cin := io.cin
      io.sA    := approxAdder.io.s
      io.coutA := approxAdder.io.cout
    }
    test(new LOADUT)(generateMetrics(_))
  }

  // Test OFLOCAV1
  s"OFLOCA${params2paren(OFLOCAParametersV1)}" should "generate error metrics" in {
    class OFLOCADUT extends DUT(Width) {
      val approxAdder = Module(new OFLOCA(Width, OFLOCAParametersV1(1), OFLOCAParametersV1(2)))
      approxAdder.io.a   := io.a
      approxAdder.io.b   := io.b
      approxAdder.io.cin := io.cin
      io.sA    := approxAdder.io.s
      io.coutA := approxAdder.io.cout
    }
    test(new OFLOCADUT)(generateMetrics(_))
  }

  // Test OFLOCAV2
  s"OFLOCA${params2paren(OFLOCAParametersV2)}" should "generate error metrics" in {
    class OFLOCADUT extends DUT(Width) {
      val approxAdder = Module(new OFLOCA(Width, OFLOCAParametersV2(1), OFLOCAParametersV2(2)))
      approxAdder.io.a   := io.a
      approxAdder.io.b   := io.b
      approxAdder.io.cin := io.cin
      io.sA    := approxAdder.io.s
      io.coutA := approxAdder.io.cout
    }
    test(new OFLOCADUT)(generateMetrics(_))
  }

  // Test GeArV1
  s"GeAr${params2paren(GeArParametersV1)}" should "generate error metrics" in {
    class GeArDUT extends DUT(Width) {
      val approxAdder = Module(new GeAr(Width, GeArParametersV1(1), GeArParametersV1(2)))
      approxAdder.io.a    := io.a
      approxAdder.io.b    := io.b
      approxAdder.io.cin  := io.cin
      approxAdder.io.ctrl.foreach(_ := false.B)
      io.sA    := approxAdder.io.s
      io.coutA := approxAdder.io.cout
    }
    test(new GeArDUT)(generateMetrics(_))
  }

  // Test GeArV2
  s"GeAr${params2paren(GeArParametersV2)}" should "generate error metrics" in {
    class GeArDUT extends DUT(Width) {
      val approxAdder = Module(new GeAr(Width, GeArParametersV2(1), GeArParametersV2(2)))
      approxAdder.io.a   := io.a
      approxAdder.io.b   := io.b
      approxAdder.io.cin := io.cin
      approxAdder.io.ctrl.foreach(_ := false.B)
      io.sA    := approxAdder.io.s
      io.coutA := approxAdder.io.cout
    }
    test(new GeArDUT)(generateMetrics(_))
  }
}
