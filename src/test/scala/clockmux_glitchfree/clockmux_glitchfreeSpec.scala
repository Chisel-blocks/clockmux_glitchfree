package clockmux_glitchfree

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import dsptools.{DspTester, DspTesterOptionsManager, DspTesterOptions}
import dsptools.numbers._

class clockmux_glitchfreeSpec extends AnyFlatSpec with ChiselScalatestTester {

  it should "propagate values in the register" in {
    test(new clockmux_glitchfree(proto = DspComplex(UInt(16.W), UInt(16.W)), n = 8)) { dut =>
      dut.io.A(0).real poke 5
      dut.io.A(0).imag poke 102 
      dut.clock.step(1)
      dut.io.B(0).real expect 5
      dut.io.B(0).imag expect 102
    }
  }
}
