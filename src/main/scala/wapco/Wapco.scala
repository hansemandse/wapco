package wapco

import chisel3._

import approx.addition.{RCA, CLA, LOA, OFLOCA, GeAr}

// Contains configurations of the adders used
object AdderConfigurations {
  val RCAParameters = Seq(32)
  val CLAParameters = Seq(32, 8)
  val LOAParameters = Seq(32, 12)
  val OFLOCAParametersV1 = Seq(32, 12, 4)
  val OFLOCAParametersV2 = Seq(32, 16, 8)
  val GeArParametersV1 = Seq(32, 6, 2)
  val GeArParametersV2 = Seq(32, 8, 8)
}

// Use this to generate the Verilog descriptions of the characterized adders
object generate extends App {
  import AdderConfigurations._
  val stage = new chisel3.stage.ChiselStage
  val emitArgs  = Array("--target-dir", "build")
  def move(adder: String, args: Int*): Unit = java.nio.file.Files.move(
    new java.io.File(s"./build/$adder.v").toPath(),
    new java.io.File(s"./build/$adder${args.map(a => s"_$a").reduceOption(_ + _).getOrElse("")}.v").toPath(),
    java.nio.file.StandardCopyOption.REPLACE_EXISTING
  )
  // Exact adders
  stage.emitVerilog(new RCA(RCAParameters(0)), emitArgs)
  move("RCA", RCAParameters:_*)
  stage.emitVerilog(new CLA(CLAParameters(0), CLAParameters(1)), emitArgs)
  move("CLA", CLAParameters:_*)
  // Approximate adders
  stage.emitVerilog(new LOA(LOAParameters(0), LOAParameters(1)), emitArgs)
  move("LOA", LOAParameters:_*)
  stage.emitVerilog(new OFLOCA(OFLOCAParametersV1(0), OFLOCAParametersV1(1), OFLOCAParametersV1(2)), emitArgs)
  move("OFLOCA", OFLOCAParametersV1:_*)
  stage.emitVerilog(new OFLOCA(OFLOCAParametersV2(0), OFLOCAParametersV2(1), OFLOCAParametersV2(2)), emitArgs)
  move("OFLOCA", OFLOCAParametersV2:_*)
  stage.emitVerilog(new GeAr(GeArParametersV1(0), GeArParametersV1(1), GeArParametersV1(2)), emitArgs)
  move("GeAr", GeArParametersV1:_*)
  stage.emitVerilog(new GeAr(GeArParametersV2(0), GeArParametersV2(1), GeArParametersV2(2)), emitArgs)
  move("GeAr", GeArParametersV2:_*)
}
