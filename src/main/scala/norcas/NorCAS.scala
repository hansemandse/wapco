package norcas

import chisel3._

import approx.addition.{
  Adder, FullAdder, AXA1, AXA2, AXA3, RCA, CLA, CSA, LOA, OFLOCA, GeAr
}
import approx.multiplication.{Radix2Multiplier, RecursiveMultiplier}
import approx.multiplication.comptree.{
  NoApproximation, ColumnTruncation, Miscounting, ORCompression, RowTruncation
}
import cmvm._

// Build some adders with inexact full adders
class LAXA(width: Int, approxWidth: Int, variant: Int) extends Adder(width) {
  val sums = Wire(Vec(width, Bool()))
  val cins = Wire(Vec(width + 1, Bool()))
  cins(0) := io.cin

  // Generate approximate part
  (0 until approxWidth).foreach { i =>
    val add = Module(variant match {
      case 1 => new AXA1
      case 2 => new AXA2
      case 3 => new AXA3
      case _ => throw new IllegalArgumentException(s"cannot generate LAXA for variant=$variant")
    })
    add.io.x   := io.a(i)
    add.io.y   := io.b(i)
    add.io.cin := cins(i)
    sums(i)    := add.io.s
    cins(i+1)  := add.io.cout
  }

  // Generate remaining part
  (approxWidth until width).foreach { i =>
    val add = Module(new FullAdder)
    add.io.x   := io.a(i)
    add.io.y   := io.b(i)
    add.io.cin := cins(i)
    sums(i)    := add.io.s
    cins(i+1)  := add.io.cout
  }

  // Combine results and output
  io.s    := sums.asUInt
  io.cout := cins(width)
}

// Contains configurations of the adders used
object AdderConfigurations {
  val rca     = (32)
  val csa     = (32, 8)
  val cla     = (32, 8)
  val loa     = (32, 12)
  val laxa1   = (32, 12, 1)
  val laxa2   = (32, 12, 2)
  val laxa3   = (32, 12, 3)
  val ofloca1 = (32, 12, 4)
  val ofloca2 = (32, 16, 8)
  val gear1   = (32, 6, 2)
  val gear2   = (32, 8, 8)
}

// Contains configurations of the multipliers used
object MultConfigurations {
  val r2m = (32)
  val rec = (32)
  val ctr1 = (32, ColumnTruncation(16))
  val ctr2 = (32, ColumnTruncation(32))
  val rtr1 = (32, RowTruncation(2))
  val rtr2 = (32, RowTruncation(4))
  val msc1 = (32, Miscounting(16))
  val msc2 = (32, Miscounting(32))
  val cmp1 = (32, ORCompression(16))
  val cmp2 = (32, ORCompression(32))
  val kul = (32, 16)
}

// Contains configurations of the matrix-vector multipliers used
case class CMVMParams(dataW: Int, p: Int, e: Int, numBits: Int, minSqnr: Int)
object CMVMConfigurations {
  val targetS = (16, 16)
  val confs = Seq(
    CMVMParams(32, 2, 2, 4, 45),
    CMVMParams(32, 3, 2, 4, 45),
    CMVMParams(32, 4, 2, 4, 45),
    CMVMParams(32, 5, 2, 4, 45),
    CMVMParams(32, 2, 2, 5, 45),
    CMVMParams(32, 3, 2, 5, 45),
    CMVMParams(32, 4, 2, 5, 45),
    CMVMParams(32, 5, 2, 5, 45),
    CMVMParams(32, 2, 2, 6, 45),
    CMVMParams(32, 3, 2, 6, 45),
    CMVMParams(32, 4, 2, 6, 45),
    CMVMParams(32, 5, 2, 6, 45)
  )
}

// Use this to generate the Verilog descriptions of the characterized adders and multipliers
object generate extends App {
  import AdderConfigurations._
  import MultConfigurations._
  import CMVMConfigurations._
  val stage = new chisel3.stage.ChiselStage
  val emitArgs = Array("--target-dir", "build")

  def moveAdder(file: String, args: Int*): Unit = java.nio.file.Files.move(
    new java.io.File(s"./build/$file.v").toPath(),
    new java.io.File(s"./build/$file${args.map(a => s"_$a").mkString("")}.v").toPath(),
    java.nio.file.StandardCopyOption.REPLACE_EXISTING
  )

  def moveMult(file: String, approx: String, args: Int*): Unit = java.nio.file.Files.move(
    new java.io.File(s"./build/$file.v").toPath(),
    new java.io.File(s"./build/${file}_${approx}${args.map(a => s"_$a").mkString("")}.v").toPath(),
    java.nio.file.StandardCopyOption.REPLACE_EXISTING
  )

  def moveCmvm(file: String, args: Int*): Unit = java.nio.file.Files.move(
    new java.io.File(s"./build/$file.v").toPath(),
    new java.io.File(s"./build/$file${args.map(a => s"_$a").mkString("")}.v").toPath(),
    java.nio.file.StandardCopyOption.REPLACE_EXISTING
  )

  // Exact adders
  stage.emitVerilog(new RCA(rca), emitArgs)
  moveAdder("RCA", rca)
  stage.emitVerilog(new CSA(csa._1, csa._2), emitArgs)
  moveAdder("CSA", csa._1, csa._2)
  stage.emitVerilog(new CLA(cla._1, cla._2), emitArgs)
  moveAdder("CLA", cla._1, cla._2)
  // Approximate adders
  stage.emitVerilog(new LOA(loa._1, loa._2), emitArgs)
  moveAdder("LOA", loa._1, loa._2)
  stage.emitVerilog(new LAXA(laxa1._1, laxa1._2, laxa1._3), emitArgs)
  moveAdder("LAXA", laxa1._1, laxa1._2, laxa1._3)
  stage.emitVerilog(new LAXA(laxa2._1, laxa2._2, laxa2._3), emitArgs)
  moveAdder("LAXA", laxa2._1, laxa2._2, laxa2._3)
  stage.emitVerilog(new LAXA(laxa3._1, laxa3._2, laxa3._3), emitArgs)
  moveAdder("LAXA", laxa3._1, laxa3._2, laxa3._3)
  stage.emitVerilog(new OFLOCA(ofloca1._1, ofloca1._2, ofloca1._3), emitArgs)
  moveAdder("OFLOCA", ofloca1._1, ofloca1._2, ofloca1._3)
  stage.emitVerilog(new OFLOCA(ofloca2._1, ofloca2._2, ofloca2._3), emitArgs)
  moveAdder("OFLOCA", ofloca2._1, ofloca2._2, ofloca2._3)
  stage.emitVerilog(new GeAr(gear1._1, gear1._2, gear1._3), emitArgs)
  moveAdder("GeAr", gear1._1, gear1._2, gear1._3)
  stage.emitVerilog(new GeAr(gear2._1, gear2._2, gear2._3), emitArgs)
  moveAdder("GeAr", gear2._1, gear2._2, gear2._3)

  // Exact multipliers
  stage.emitVerilog(new Radix2Multiplier(r2m), emitArgs)
  moveMult("Radix2Multiplier", "NoApproximation", r2m)
  stage.emitVerilog(new RecursiveMultiplier(rec), emitArgs)
  moveMult("RecursiveMultiplier", "NoApproximation", rec)
  // Approximate multipliers
  stage.emitVerilog(new Radix2Multiplier(ctr1._1, approx=ctr1._2), emitArgs)
  moveMult("Radix2Multiplier", s"ColumnTruncation${ctr1._2.width}", ctr1._1)
  stage.emitVerilog(new Radix2Multiplier(ctr2._1, approx=ctr2._2), emitArgs)
  moveMult("Radix2Multiplier", s"ColumnTruncation${ctr2._2.width}", ctr2._1)
  stage.emitVerilog(new Radix2Multiplier(rtr1._1, approx=rtr1._2), emitArgs)
  moveMult("Radix2Multiplier", s"RowTruncation${rtr1._2.rows}", rtr1._1)
  stage.emitVerilog(new Radix2Multiplier(rtr2._1, approx=rtr2._2), emitArgs)
  moveMult("Radix2Multiplier", s"RowTruncation${rtr2._2.rows}", rtr2._1)
  stage.emitVerilog(new Radix2Multiplier(msc1._1, approx=msc1._2), emitArgs)
  moveMult("Radix2Multiplier", s"Miscounting${msc1._2.width}", msc1._1)
  stage.emitVerilog(new Radix2Multiplier(msc2._1, approx=msc2._2), emitArgs)
  moveMult("Radix2Multiplier", s"Miscounting${msc2._2.width}", msc2._1)
  stage.emitVerilog(new Radix2Multiplier(cmp1._1, approx=cmp1._2), emitArgs)
  moveMult("Radix2Multiplier", s"ORCompression${cmp1._2.width}", cmp1._1)
  stage.emitVerilog(new Radix2Multiplier(cmp2._1, approx=cmp2._2), emitArgs)
  moveMult("Radix2Multiplier", s"ORCompression${cmp2._2.width}", cmp2._1)
  stage.emitVerilog(new RecursiveMultiplier(kul._1, kul._2), emitArgs)
  moveMult("RecursiveMultiplier", "Kulkarni", kul._1)

  // Matrix-vector multipliers
  val rng = new scala.util.Random(42)
  val mat = (0 until targetS._1).map(_ => Array.fill(targetS._2) { rng.nextGaussian() }).toArray
  confs.foreach { conf =>
    val dec = decompose(mat, p=conf.p, e=conf.e, numBits=conf.numBits, minSqnr=conf.minSqnr)
    stage.emitVerilog(new ConstantCMVM(dec, conf.numBits, conf.dataW), emitArgs)
    moveCmvm("ConstantCMVM", conf.p, conf.e, conf.numBits)
  }
}
