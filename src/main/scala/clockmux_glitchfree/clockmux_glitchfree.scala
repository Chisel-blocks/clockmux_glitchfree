
// Chisel module clockmux_glitchfree
// Description here
// Inititally written by chisel-blocks-utils initmodule.sh, 2025-03-13
package clockmux_glitchfree

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}
import dsptools.{DspTester, DspTesterOptionsManager, DspTesterOptions}
import dsptools.numbers._
import breeze.math.Complex

/** IO definitions for clockmux_glitchfree */
class clockmux_glitchfreeIO[T <:Data](proto: T,n: Int)
   extends Bundle {
        val A       = Input(Vec(n,proto))
        val B       = Output(Vec(n,proto))
}

/** Module definition for clockmux_glitchfree
  * @param proto type information
  * @param n number of elements in register
  */
class clockmux_glitchfree[T <:Data] (proto: T,n: Int) extends Module {
    val io = IO(new clockmux_glitchfreeIO( proto=proto, n=n))
    val register=RegInit(VecInit(Seq.fill(n)(0.U.asTypeOf(proto.cloneType))))
    register:=io.A
    io.B:=register
}

/** This gives you verilog */
object clockmux_glitchfree extends App {
    val annos = Seq(ChiselGeneratorAnnotation(() => new clockmux_glitchfree(
        proto=DspComplex(UInt(16.W),UInt(16.W)), n=8
    )))
    (new ChiselStage).execute(args, annos)
}

