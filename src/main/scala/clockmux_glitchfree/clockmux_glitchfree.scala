// SPDX-License-Identifier: Apache-2.0

// Generalization of a 2-to-1 clock multiplexer shown in 
// Intel Quartus Prime Pro Edition User Guide: Design Recommendations
// https://www.intel.com/content/www/us/en/docs/programmable/683082/22-1/clock-multiplexing.html

// Initially written by Tomi Valkonen (tomi.j.valkonen@aalto.fi), 2025-02-06
package clockmux_glitchfree

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

class clockmux_glitchfreeIO(n_clocks: Int) extends Bundle {
 val clock_in = Input(Vec(n_clocks, Clock()))
 val sel = Input(UInt(log2Ceil(n_clocks).W))
 val clock_out = Output(Clock())
}

class clockmux_glitchfree(n_clocks: Int) extends Module {
  require(n_clocks > 0, "Number of clocks cannot be zero")
  val io = IO(new clockmux_glitchfreeIO(n_clocks))

  // Clock select is encoded with one hot encoding, each of the bits
  // is synchronized through 3 flipflops with the bit's respective clock.
  // The idea of this circuit is that all the zero bits in the one hot select clock gate 
  // their respective clocks, and only after that the hot bit is allowed to enable its
  // respective clock
  val one_hot_sel = UIntToOH(io.sel)

  val enables = for (i <- 0 until n_clocks) yield {
    val out_reg = withClock((~io.clock_in(i).asBool).asClock) { RegInit(false.B) }
    out_reg
  }

  val synchronizer_inputs = VecInit(Seq.tabulate(n_clocks) { i =>
    val masked = VecInit(enables.zipWithIndex.map { case (value, idx) =>
      if (idx == i) false.B else value
    })
    one_hot_sel(i) && !masked.reduce(_ || _)
  })

  val synchronizers = for (i <- 0 until n_clocks) yield {
    val first = withClock(io.clock_in(i)) { RegNext(synchronizer_inputs(i)) }
    val second = withClock(io.clock_in(i)) { RegNext(first) }
    second 
  }

  for (i <- 0 until n_clocks) {
    enables(i) := synchronizers(i)
  }

  val gated_clocks = VecInit(Seq.tabulate(n_clocks) { i =>
    io.clock_in(i).asBool && enables(i)
  })

  io.clock_out := gated_clocks.reduce(_ || _).asClock
}

object clockmux_glitchfree extends App {
  val annos = Seq(ChiselGeneratorAnnotation(() => new clockmux_glitchfree(
    n_clocks = 2
  )))
  (new ChiselStage).execute(args, annos)
}

