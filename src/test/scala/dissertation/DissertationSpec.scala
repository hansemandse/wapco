package dissertation

import chisel3._
import chiseltest._
import chiseltest.internal.NoThreadingAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import approx.addition.{
  Adder, FullAdder, AXA3, TCAA, SESA1, RCA, CLA, CSA, SklanskyPPA,
  LOA, OFLOCA, GeAr, SklanskyAxPPA, AdaptiveOFLOCA
}
import approx.multiplication.{
  Multiplier, MultiplierIO, Radix2Multiplier, Radix4Multiplier,
  RecursiveMultiplier, AdaptiveRadix2Multiplier
}
import approx.multiplication.comptree.{
  ColumnTruncation, Miscounting, ORCompression, RowTruncation
}
import chiselverify.approximation._
import chiselverify.approximation.Metrics._

class DissertationSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  import AdderConfigurations._
  import MultConfigurations._

  val NumTests = 1000000
  val Width    = 32
  val Metrics  = Seq(ER(), MED(), MRED())

  private val widths = Seq(rca, csa._1, cla._1, ppa, loa._1, laxa1._1, laxa2._1,
    laxa3._1, ofloca1._1, ofloca2._1, gear1._1, gear2._1, axppa._1, aofloca._1,
    r2m, r4m, rec, ctr1._1, rtr1._1, msc1._1, cmp1._1, kul._1, ar2m._1)
  require(widths.forall(_ == Width))

  // Insert the name of a DUT into its error report
  def insertModName[T <: Module](srcRprt: String, dut: T): String = {
    val modName  = dut.getClass().getSimpleName()
    val srcLines = srcRprt.split('\n')
    val hdr      = s" Error report ($modName) "
    val padLen   = srcLines.map(_.size).max - hdr.size
    val hdrLine  = "=" * (padLen / 2) ++ hdr ++ "=" * (padLen / 2 + (if ((padLen & 0x1) == 1) 1 else 0))
    srcLines(0)  = hdrLine
    srcLines.mkString("\n")
  }

  // Generate some random inputs to the adder and sample its registered outputs
  def generateAdderMetrics[T <: Adder](dut: T): Unit = {
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
    println(insertModName(er.report(), dut))
  }

  // Test LOA
  s"LOA$loa" should "generate error metrics" in {
    test(new LOA(Width, loa._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateAdderMetrics(_))
  }

  // Test LAXA1
  s"LAXA$laxa1" should "generate error metrics" in {
    test(new LAXA(Width, laxa1._2, laxa1._3))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateAdderMetrics(_))
  }

  // Test LAXA2
  s"LAXA$laxa2" should "generate error metrics" in {
    test(new LAXA(Width, laxa2._2, laxa2._3))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateAdderMetrics(_))
  }

  // Test LAXA3
  s"LAXA$laxa3" should "generate error metrics" in {
    test(new LAXA(Width, laxa3._2, laxa3._3))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateAdderMetrics(_))
  }

  // Test OFLOCAV1
  s"OFLOCA$ofloca1" should "generate error metrics" in {
    test(new OFLOCA(Width, ofloca1._2, ofloca1._3))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateAdderMetrics(_))
  }

  // Test OFLOCAV2
  s"OFLOCA$ofloca2" should "generate error metrics" in {
    test(new OFLOCA(Width, ofloca2._2, ofloca2._3))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateAdderMetrics(_))
  }

  // Test GeArV1 - needs redefinition as GeAr doesn't extend Adder
  s"GeAr$gear1" should "generate error metrics" in {
    class GeArV1 extends Adder(Width) {
      val approxAdder = Module(new GeAr(Width, gear1._2, gear1._3))
      approxAdder.io.a   := io.a
      approxAdder.io.b   := io.b
      approxAdder.io.cin := io.cin
      approxAdder.io.ctrl.foreach(_ := false.B)
      io.s    := approxAdder.io.s
      io.cout := approxAdder.io.cout
    }
    test(new GeArV1)
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateAdderMetrics(_))
  }

  // Test GeArV2 - needs redefinition as GeAr doesn't extend Adder
  s"GeAr$gear2" should "generate error metrics" in {
    class GeArV2 extends Adder(Width) {
      val approxAdder = Module(new GeAr(Width, gear2._2, gear2._3))
      approxAdder.io.a   := io.a
      approxAdder.io.b   := io.b
      approxAdder.io.cin := io.cin
      approxAdder.io.ctrl.foreach(_ := false.B)
      io.s    := approxAdder.io.s
      io.cout := approxAdder.io.cout
    }
    test(new GeArV2)
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateAdderMetrics(_))
  }

  // Test SklanskyAxPPA
  s"SklanskyAxPPA$axppa" should "generate error metrics" in {
    test(new SklanskyAxPPA(Width, axppa._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateAdderMetrics(_))
  }

  // Test AdaptiveOFLOCA with three approximate modes (four modes total)
  s"AdaptiveOFLOCA$aofloca" should "generate error metrics" in {
    (1 to aofloca._3).foreach { mode =>
      class Wrapper extends Adder(Width) {
        val adder = Module(new AdaptiveOFLOCA(Width, aofloca._2, aofloca._3))
        adder.io.ctrl := mode.U
        adder.io.a    := io.a
        adder.io.b    := io.b
        adder.io.cin  := io.cin
        io.s    := adder.io.s
        io.cout := adder.io.cout
      }
      test(new Wrapper)
        .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateAdderMetrics(_))
    }
  }

  // Generate some random inputs to the multiplier and sample its registered outputs
  def generateMultMetrics[T <: Multiplier](dut: T): Unit = {
    val rng = new scala.util.Random(42)

    // Create an error reporter and track the sum output
    val er = new ErrorReporter(
      track(dut.io.p, Metrics:_*)
    )

    // Apply NumTests random inputs and collect the outputs
    (0 until NumTests).foreach { _ =>
      val (a, b) = (BigInt(Width, rng), BigInt(Width, rng))
      val prod = a * b
      dut.io.a.poke(a.U)
      dut.io.b.poke(b.U)
      dut.clock.step()
      er.sample(Map(dut.io.p -> prod))
    }

    // Generate an error report and print it
    println(insertModName(er.report(), dut))
  }

  // Test Radix2Multiplier with RowTruncation(2)
  s"Radix2Multiplier$rtr1" should "generate error metrics" in {
    test(new Radix2Multiplier(Width, Width, comp=true, approx=rtr1._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMultMetrics(_))
  }

  // Test Radix2Multiplier with ColumnTruncation(16)
  s"Radix2Multiplier$ctr1" should "generate error metrics" in {
    test(new Radix2Multiplier(Width, Width, comp=true, approx=ctr1._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMultMetrics(_))
  }

  // Test Radix2Multiplier with ORCompression(16)
  s"Radix2Multiplier$cmp1" should "generate error metrics" in {
    test(new Radix2Multiplier(Width, Width, comp=true, approx=cmp1._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMultMetrics(_))
  }

  // Test Radix2Multiplier with Miscounting(16)
  s"Radix2Multiplier$msc1" should "generate error metrics" in {
    test(new Radix2Multiplier(Width, Width, comp=true, approx=msc1._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMultMetrics(_))
  }

  // Test Radix4Multiplier with RowTruncation(2)
  s"Radix4Multiplier$rtr1" should "generate error metrics" in {
    test(new Radix4Multiplier(Width, Width, comp=true, approx=rtr1._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMultMetrics(_))
  }

  // Test Radix4Multiplier with ColumnTruncation(16)
  s"Radix4Multiplier$ctr1" should "generate error metrics" in {
    test(new Radix4Multiplier(Width, Width, comp=true, approx=ctr1._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMultMetrics(_))
  }

  // Test Radix4Multiplier with ORCompression(16)
  s"Radix4Multiplier$cmp1" should "generate error metrics" in {
    test(new Radix4Multiplier(Width, Width, comp=true, approx=cmp1._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMultMetrics(_))
  }

  // Test Radix4Multiplier with Miscounting(16)
  s"Radix4Multiplier$msc1" should "generate error metrics" in {
    test(new Radix4Multiplier(Width, Width, comp=true, approx=msc1._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMultMetrics(_))
  }

  // Test RecursiveMultiplier with approximation
  s"RecursiveMultiplier$kul" should "generate error metrics" in {
    test(new RecursiveMultiplier(Width, kul._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMultMetrics(_))
  }

  s"AdaptiveRadix2Multiplier$ar2m" should "generate error metrics" in {
    (1 to ar2m._3).foreach { mode =>
      class Wrapper extends Multiplier(Width, Width) {
        val mult = Module(new AdaptiveRadix2Multiplier(Width, Width, ar2m._2, numModes=ar2m._3))
        mult.io.ctrl := mode.U
        mult.io.a    := io.a
        mult.io.b    := io.b
        io.p := mult.io.p
      }
      test(new Wrapper)
        .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMultMetrics(_))
    }
  }
}
