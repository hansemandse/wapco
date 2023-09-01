package wapco

import chisel3._
import chiseltest._
import chiseltest.internal.NoThreadingAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import approx.addition.{AdderIO, Adder, LOA, OFLOCA, GeAr}
import chiselverify.approximation._
import chiselverify.approximation.Metrics._

trait WapcoSpec extends AnyFlatSpec with ChiselScalatestTester {
  import AdderConfigurations._

  val NumTests = 1000000
  val Width    = 32
  val Metrics  = Seq(ER(), MED(), SDED(), MRED())

  private val widths = Seq(RCAParameters(0), CLAParameters(0), LOAParameters(0), 
    OFLOCAParametersV1(0), OFLOCAParametersV2(0), GeArParametersV1(0), GeArParametersV2(0))
  require(widths.forall(_ == Width))

  // Convert a sequence of parameters to a comma-separated string form
  def params2paren(params: Seq[Int]): String = s"(${params.mkString(", ")})"
}

class StandaloneWapcoSpec extends WapcoSpec with Matchers {
  import AdderConfigurations._

  // Generate some random inputs to the adder and sample its registered outputs
  def generateMetrics[T <: Adder](dut: T): Unit = {
    val rng  = new scala.util.Random(42)
    val mask = (BigInt(1) << Width) - 1

    // Create an error reporter and track the sum output
    val er = new ErrorReporter(
      track(dut.io.s, Metrics:_*)
    )

    // Apply NumTests random inputs and collect the outputs
    (0 until NumTests).foreach { _ =>
      val (a, b, cin) = (BigInt(Width, rng), BigInt(Width, rng), rng.nextBoolean())
      val sum  = (a + b + (if (cin) 1 else 0)) & mask
      val cout = sum >> Width
      dut.io.a.poke(a.U)
      dut.io.b.poke(b.U)
      dut.io.cin.poke(cin.B)
      dut.clock.step()
      er.sample(Map(dut.io.s -> sum, dut.io.cout -> cout))
    }

    // Generate an error report and print it
    println(er.report())
  }

  // Test LOA
  s"LOA${params2paren(LOAParameters)}" should "generate error metrics" in {
    test(new LOA(Width, LOAParameters(1)))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMetrics(_))
  }

  // Test OFLOCAV1
  s"OFLOCA${params2paren(OFLOCAParametersV1)}" should "generate error metrics" in {
    test(new OFLOCA(Width, OFLOCAParametersV1(1), OFLOCAParametersV1(2)))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMetrics(_))
  }

  // Test OFLOCAV2
  s"OFLOCA${params2paren(OFLOCAParametersV2)}" should "generate error metrics" in {
    test(new OFLOCA(Width, OFLOCAParametersV2(1), OFLOCAParametersV2(2)))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMetrics(_))
  }

  // Test GeArV1 - needs redefinition as GeAr doesn't extend Adder
  s"GeAr${params2paren(GeArParametersV1)}" should "generate error metrics" in {
    class GeArV1 extends Adder(Width) {
      val approxAdder = Module(new GeAr(Width, GeArParametersV1(1), GeArParametersV1(2)))
      approxAdder.io.a   := io.a
      approxAdder.io.b   := io.b
      approxAdder.io.cin := io.cin
      approxAdder.io.ctrl.foreach(_ := false.B)
      io.s    := approxAdder.io.s
      io.cout := approxAdder.io.cout
    }
    test(new GeArV1)
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMetrics(_))
  }

  // Test GeArV2 - needs redefinition as GeAr doesn't extend Adder
  s"GeAr${params2paren(GeArParametersV2)}" should "generate error metrics" in {
    class GeArV2 extends Adder(Width) {
      val approxAdder = Module(new GeAr(Width, GeArParametersV2(1), GeArParametersV2(2)))
      approxAdder.io.a   := io.a
      approxAdder.io.b   := io.b
      approxAdder.io.cin := io.cin
      approxAdder.io.ctrl.foreach(_ := false.B)
      io.s    := approxAdder.io.s
      io.cout := approxAdder.io.cout
    }
    test(new GeArV2)
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMetrics(_))
  }
}

class CombinedWapcoSpec extends WapcoSpec with Matchers {
  import AdderConfigurations._

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
    test(new LOADUT)
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMetrics(_))
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
    test(new OFLOCADUT)
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMetrics(_))
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
    test(new OFLOCADUT)
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMetrics(_))
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
    test(new GeArDUT)
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMetrics(_))
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
    test(new GeArDUT)
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMetrics(_))
  }
}
