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

class ClockMuxResetSync(activeLow: Boolean = false) extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock        = Input(Clock())        // system clock
    val areset_in    = Input(AsyncReset())   // async reset from external world with configurable polarity
    val areset_sync  = Output(AsyncReset())  // active high async reset with synchronized deassertion
  })
  val polarity = if (activeLow) "negedge" else "posedge"
  val level = if (activeLow) "!areset_in" else "areset_in"

  override def desiredName = s"ClockMuxResetSync_$polarity"

  setInline(
    s"ClockMuxResetSync_$polarity.v",
    s"""
    |module ClockMuxResetSync_$polarity(
    |  input  wire clock,
    |  input  wire areset_in,
    |  output wire areset_sync,
    |  output wire aresetn_sync
    |);
    |  reg first, second;
    |  always @(posedge clock or $polarity areset_in)
    |    if ($level) begin
    |      first <= 1'b1;
    |      second <= 1'b1;
    |    end else begin
    |      first <= 1'b0;
    |      second <= first;
    |    end
    |  assign areset_sync = second;
    |endmodule
    """.stripMargin
  )
}


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

  // Synchronize reset to all clock domains
  val reset_syncs = for (i <- 0 until n_clocks) yield {
    val reset_sync = Module(new ClockMuxResetSync(false))
    reset_sync.io.areset_in := reset.asAsyncReset
    reset_sync.io.clock     := io.clock_in(i)
    reset_sync
  }

  // Synchronize reset to inverted clock domain
  val reset_n_syncs = for (i <- 0 until n_clocks) yield {
    val reset_sync = Module(new ClockMuxResetSync(false))
    reset_sync.io.areset_in := reset.asAsyncReset
    reset_sync.io.clock     := (~io.clock_in(i).asBool).asClock
    reset_sync
  }

  val enables = for (i <- 0 until n_clocks) yield {
    val out_reg = withClockAndReset((~io.clock_in(i).asBool).asClock, reset_n_syncs(i).io.areset_sync) { RegInit(false.B) }
    out_reg
  }

  val synchronizer_inputs = VecInit(Seq.tabulate(n_clocks) { i =>
    val masked = VecInit(enables.zipWithIndex.map { case (value, idx) =>
      if (idx == i) false.B else value
    })
    one_hot_sel(i) && !masked.reduce(_ || _)
  })

  val synchronizers = for (i <- 0 until n_clocks) yield {
    val first = withClockAndReset(io.clock_in(i), reset_syncs(i).io.areset_sync) { RegNext(synchronizer_inputs(i), false.B) }
    val second = withClockAndReset(io.clock_in(i), reset_syncs(i).io.areset_sync) { RegNext(first, false.B) }
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

