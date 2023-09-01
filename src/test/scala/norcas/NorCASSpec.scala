package norcas

import chisel3._
import chiseltest._
import chiseltest.internal.NoThreadingAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import approx.addition.{
  Adder, FullAdder, AXA1, AXA2, AXA3, RCA, CLA, CSA, LOA, OFLOCA, GeAr
}
import approx.multiplication.{Multiplier, Radix2Multiplier, RecursiveMultiplier}
import approx.multiplication.comptree.{
  NoApproximation, ColumnTruncation, Miscounting, ORCompression, RowTruncation
}
import chiselverify.approximation._
import chiselverify.approximation.Metrics._
import cmvm._

trait NorCASSpec extends AnyFlatSpec with ChiselScalatestTester {
  import AdderConfigurations._
  import MultConfigurations._

  val NumTests = 1000000
  val Width    = 32
  val Metrics  = Seq(ER(), MED(), SDED(), MRED())

  private val widths = Seq(rca, csa._1, cla._1, loa._1, laxa1._1, laxa2._1,
    laxa3._1, ofloca1._1, ofloca2._1, gear1._1, gear2._1, r2m, rec, ctr1._1,
    ctr2._1, rtr1._1, rtr2._1, msc1._1, msc2._1, cmp1._1, cmp2._1, kul._1)
  require(widths.forall(_ == Width))
}

class StandaloneNorCASSpec extends NorCASSpec with Matchers {
  import AdderConfigurations._
  import MultConfigurations._

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
    println(er.report())
  }

  // Test LOA
  s"LOA$loa" should "generate error metrics" in {
    test(new LOA(Width, loa._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateAdderMetrics(_))
  }

  // Test LAXA1
  s"LAXA1$laxa1" should "generate error metrics" in {
    test(new LAXA(Width, laxa1._2, laxa1._3))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateAdderMetrics(_))
  }

  // Test LAXA2
  s"LAXA1$laxa2" should "generate error metrics" in {
    test(new LAXA(Width, laxa2._2, laxa2._3))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateAdderMetrics(_))
  }

  // Test LAXA3
  s"LAXA1$laxa3" should "generate error metrics" in {
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
    println(er.report())
  }

  // Test Radix2Multiplier with RowTruncation(2)
  s"Radix2Multiplier$rtr1" should "generate error metrics" in {
    test(new Radix2Multiplier(rtr1._1, approx=rtr1._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMultMetrics(_))
  }

  // Test Radix2Multiplier with RowTruncation(4)
  s"Radix2Multiplier$rtr2" should "generate error metrics" in {
    test(new Radix2Multiplier(rtr2._1, approx=rtr2._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMultMetrics(_))
  }

  // Test Radix2Multiplier with ColumnTruncation(16)
  s"Radix2Multiplier$ctr1" should "generate error metrics" in {
    test(new Radix2Multiplier(ctr1._1, approx=ctr1._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMultMetrics(_))
  }

  // Test Radix2Multiplier with ColumnTruncation(32)
  s"Radix2Multiplier$ctr2" should "generate error metrics" in {
    test(new Radix2Multiplier(ctr2._1, approx=ctr2._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMultMetrics(_))
  }

  // Test Radix2Multiplier with ORCompression(16)
  s"Radix2Multiplier$cmp1" should "generate error metrics" in {
    test(new Radix2Multiplier(cmp1._1, approx=cmp1._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMultMetrics(_))
  }

  // Test Radix2Multiplier with ORCompression(32)
  s"Radix2Multiplier$cmp2" should "generate error metrics" in {
    test(new Radix2Multiplier(cmp2._1, approx=cmp2._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMultMetrics(_))
  }

  // Test Radix2Multiplier with Miscounting(16)
  s"Radix2Multiplier$msc1" should "generate error metrics" in {
    test(new Radix2Multiplier(msc1._1, approx=msc1._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMultMetrics(_))
  }

  // Test Radix2Multiplier with Miscounting(32)
  s"Radix2Multiplier$msc2" should "generate error metrics" in {
    test(new Radix2Multiplier(msc2._1, approx=msc2._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMultMetrics(_))
  }

  // Test RecursiveMultiplier with approximation
  s"RecursiveMultiplier$kul" should "generate error metrics" in {
    test(new RecursiveMultiplier(kul._1, kul._2))
      .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(generateMultMetrics(_))
  }
}

class CombinedNorCASSpec extends NorCASSpec with Matchers {
  import AdderConfigurations._
  import MultConfigurations._

  ???
}

class CMVMNorCASSpec extends NorCASSpec with Matchers {
  behavior of "Constant matrix-vector multiplier"
  import CMVMConfigurations._

  // Generate the same matrix as for hardware generation
  val rng = new scala.util.Random(42)
  val mat = (0 until targetS._1).map(_ => Array.fill(targetS._2) { rng.nextGaussian() }).toArray

  // Multiply a matrix of Doubles with a vector of BigInts and return the
  // result as a vector of BigInts limited to `dataW` bits
  def multiply(target: Array[Array[Double]], vec: Array[BigInt], dataW: Int): Array[BigInt] = {
    require(target.forall(_.length == vec.length))
    val mask = (BigInt(1) << dataW) - 1
    target.map { _.zip(vec).map { case (w, v) =>
      BigDecimal(w * v.toDouble).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toBigInt
    }.sum & mask}
  }

  // Generate some random inputs to the multiplier and sample its registered outputs
  def generateMultMetrics(target: Array[Array[Double]], dut: ConstantCMVM): Unit = {
    val nIns  = dut.io.ins.length
    val nOuts = dut.io.outs.length
    val dataW = dut.io.ins(0).getWidth

    // Create an error reporter and track the product outputs
    val er = new ErrorReporter(
      dut.io.outs.map(track(_, ER(), MRED())):_*
    )

    // Apply NumTests random inputs and collect the outputs
    (0 until NumTests).foreach { _ =>
      val ins = Array.fill(nIns) { BigInt(dataW, rng) }
      dut.io.ins.zip(ins).foreach { case (port, data) => port.poke(data.U) }
      dut.clock.step()
      val outs = multiply(target, ins, dataW)
      er.sample(dut.io.outs.zip(outs).map { case (port, data) => port -> data }.toMap)
    }

    // Generate an error report and print it
    val rprt = er.report()
    //println(rprt)
    val geomean = {
      val nums = rprt.split("\n").filter(_.contains("MRED")).map { line =>
        line.split(" ").last.dropRight(1).toDouble
      }
      scala.math.pow(nums.product, 1.0 / nums.length)
    }
    println(s"... got geomean MRED=$geomean!")
  }

  // Test ConstantCMVM with various configurations
  confs.foreach { conf =>
    val dec = decompose(mat, p=conf.p, e=conf.e, numBits=conf.numBits, minSqnr=conf.minSqnr)
    it should s"generate error metrics with p=${conf.p}, e=${conf.e}, and numBits=${conf.numBits}" in {
      println(s"Testing for p=${conf.p} and n=${conf.numBits} ...")
      test(new ConstantCMVM(dec, conf.numBits, conf.dataW))
        .withAnnotations(Seq(VerilatorBackendAnnotation, NoThreadingAnnotation))(dut =>
          generateMultMetrics(mat, dut))
    }
  }
}
