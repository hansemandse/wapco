package dissertation

import chisel3._

import approx.addition.{
  Adder, FullAdder, AXA3, TCAA, SESA1, RCA, CLA, CSA, SklanskyPPA, LOA, OFLOCA, GeAr, SklanskyAxPPA
}
import approx.multiplication.{Radix2Multiplier, Radix4Multiplier, RecursiveMultiplier}
import approx.multiplication.comptree.{
  ColumnTruncation, Miscounting, ORCompression, RowTruncation
}

// Build some adders with inexact full adders
class LAXA(width: Int, approxWidth: Int, variant: Int) extends Adder(width) {
  val sums = Wire(Vec(width, Bool()))
  val cins = Wire(Vec(width + 1, Bool()))
  cins(0) := io.cin

  // Generate approximate part
  (0 until approxWidth).foreach { i =>
    val add = Module(variant match {
      case 1 => new AXA3
      case 2 => new TCAA
      case 3 => new SESA1
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
  val ppa     = (32)
  val loa     = (32, 12)
  val laxa1   = (32, 12, 1)
  val laxa2   = (32, 12, 2)
  val laxa3   = (32, 12, 3)
  val ofloca1 = (32, 12, 4)
  val ofloca2 = (32, 16, 8)
  val gear1   = (32, 6, 2)
  val gear2   = (32, 8, 8)
  val axppa   = (32, 12)
}

// Contains configurations of the multipliers used
object MultConfigurations {
  val r2m  = (32)
  val r4m  = (32)
  val rec  = (32)
  val ctr1 = (32, Seq(ColumnTruncation(16)))
  val rtr1 = (32, Seq(RowTruncation(2)))
  val msc1 = (32, Seq(Miscounting(16)))
  val cmp1 = (32, Seq(ORCompression(16)))
  val kul  = (32, 16)
}

// Use this to generate the Verilog descriptions of the characterized adders and multipliers
object generate extends App {
  import AdderConfigurations._
  import MultConfigurations._
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

  // Exact adders
  stage.emitVerilog(new RCA(rca), emitArgs)
  moveAdder("RCA", rca)
  stage.emitVerilog(new CSA(csa._1, csa._2), emitArgs)
  moveAdder("CSA", csa._1, csa._2)
  stage.emitVerilog(new CLA(cla._1, cla._2), emitArgs)
  moveAdder("CLA", cla._1, cla._2)
  stage.emitVerilog(new SklanskyPPA(ppa), emitArgs)
  moveAdder("SklanskyPPA", ppa)
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
  stage.emitVerilog(new SklanskyAxPPA(axppa._1, axppa._2), emitArgs)
  moveAdder("SklanskyAxPPA", axppa._1, axppa._2)

  // Exact multipliers
  stage.emitVerilog(new Radix2Multiplier(r2m, r2m, comp=true), emitArgs)
  moveMult("Radix2Multiplier", "NoApproximation", r2m)
  stage.emitVerilog(new Radix4Multiplier(r4m, r4m, comp=true), emitArgs)
  moveMult("Radix4Multiplier", "NoApproximation", r4m)
  stage.emitVerilog(new RecursiveMultiplier(rec), emitArgs)
  moveMult("RecursiveMultiplier", "NoApproximation", rec)
  // Approximate multipliers
  stage.emitVerilog(new Radix2Multiplier(ctr1._1, ctr1._1, comp=true, approx=ctr1._2), emitArgs)
  moveMult("Radix2Multiplier", s"ColumnTruncation${ctr1._2.head.width}", ctr1._1)
  stage.emitVerilog(new Radix2Multiplier(rtr1._1, rtr1._1, comp=true, approx=rtr1._2), emitArgs)
  moveMult("Radix2Multiplier", s"RowTruncation${rtr1._2.head.rows}", rtr1._1)
  stage.emitVerilog(new Radix2Multiplier(msc1._1, msc1._1, comp=true, approx=msc1._2), emitArgs)
  moveMult("Radix2Multiplier", s"Miscounting${msc1._2.head.width}", msc1._1)
  stage.emitVerilog(new Radix2Multiplier(cmp1._1, cmp1._1, comp=true, approx=cmp1._2), emitArgs)
  moveMult("Radix2Multiplier", s"ORCompression${cmp1._2.head.width}", cmp1._1)
  stage.emitVerilog(new Radix4Multiplier(ctr1._1, ctr1._1, comp=true, approx=ctr1._2), emitArgs)
  moveMult("Radix4Multiplier", s"ColumnTruncation${ctr1._2.head.width}", ctr1._1)
  stage.emitVerilog(new Radix4Multiplier(rtr1._1, rtr1._1, comp=true, approx=rtr1._2), emitArgs)
  moveMult("Radix4Multiplier", s"RowTruncation${rtr1._2.head.rows}", rtr1._1)
  stage.emitVerilog(new Radix4Multiplier(msc1._1, msc1._1, comp=true, approx=msc1._2), emitArgs)
  moveMult("Radix4Multiplier", s"Miscounting${msc1._2.head.width}", msc1._1)
  stage.emitVerilog(new Radix4Multiplier(cmp1._1, cmp1._1, comp=true, approx=cmp1._2), emitArgs)
  moveMult("Radix4Multiplier", s"ORCompression${cmp1._2.head.width}", cmp1._1)
  stage.emitVerilog(new RecursiveMultiplier(kul._1, kul._2), emitArgs)
  moveMult("RecursiveMultiplier", "Kulkarni", kul._1)
}

// Use this run emixa on a subset of the designs
object emixa extends App {
  import AdderConfigurations._
  import MultConfigurations._
  import sys.process._

  def runEmixa(name: String, args: Int*): Int = s"python3 ./emixa/emixa.py -p dissertation.$name ${args.mkString(" ")}".!

  // Approximate adders
  runEmixa("LSESA1Spec", laxa3._1, laxa3._2)
  runEmixa("OFLOCASpec", ofloca1._1, ofloca1._2, ofloca1._3)
  runEmixa("GeArSpec", gear2._1, gear2._2, gear2._3)
  runEmixa("SklanskyAxPPASpec", axppa._1, axppa._2)

  // Approximate multipliers
  runEmixa("R2MORCompSpec", cmp1._1, cmp1._1, cmp1._2.head.width)
  runEmixa("R4MORCompSpec", cmp1._1, cmp1._1, cmp1._2.head.width)
}
