// ADS I Class Project
// Pipelined RISC-V Core - Branch Target Buffer Unit Tests
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
//
// Task 6.5 - Test your Implementation
//
// This test suite exercises the BTB module in ISOLATION (i.e. it does not
// run through the full pipelined core). This makes it possible to poke the
// BTB's inputs directly and check its outputs cycle by cycle, without
// having to hand-assemble RISC-V machine code and step the whole pipeline.
//
// PC layout used by the BTB (NUM_SETS = 8 -> INDEX_BITS = 3):
//   bits [1:0]   -> always 0 (word alignment)
//   bits [4:2]   -> index (3 bits, selects one of 8 sets)
//   bits [31:5]  -> tag   (27 bits)
//
// Helper `makePC(tag, index)` below builds a PC with a chosen tag/index pair
// so that we can deliberately create hits, misses, tag collisions in the
// same set (for LRU testing), etc.

package core_tile

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class BTB_UnitTest extends AnyFlatSpec with ChiselScalatestTester {

  // Build a PC with a specific tag (27 bits) and set index (3 bits).
  // Returns a plain BigInt (pure Scala arithmetic, no chisel hardware
  // operators like <</| here) since this can be called outside of module
  // elaboration, e.g. directly inside a `test(...) { dut => ... }` block.
  // Call sites append `.U` to turn the BigInt into a UInt *literal* right
  // before poke() - that's just literal construction, so it's safe even
  // outside a Module context (unlike hardware ops such as `someUInt << 5`).
  def makePC(tag: Int, index: Int): BigInt = {
    (BigInt(tag) << 5) | (BigInt(index) << 2)
  }

  // Predictor state encoding used inside BTB.scala:
  //   "b00" = strongly not taken
  //   "b01" = weakly   not taken
  //   "b10" = weakly   taken     (== reset / newly-allocated-as-taken value)
  //   "b11" = strongly taken
  // predictTaken := hit && hitPredictor(1)  -> true for "b10"/"b11"

  behavior of "BTB"

  // -----------------------------------------------------------------
  // 1) Correct predictions for PCs with valid entries (and misses)
  // -----------------------------------------------------------------
  it should "report invalid (miss) for a PC that was never trained" in {
    test(new BTB) { dut =>
      dut.io.update.poke(false.B)
      dut.io.mispredicted.poke(false.B)
      dut.io.branchTaken.poke(false.B)
      dut.io.updatePC.poke(0.U)
      dut.io.updateTarget.poke(0.U)

      dut.io.PC.poke(makePC(tag = 5, index = 3).U)
      dut.clock.step(1)
      dut.io.valid.expect(false.B)
    }
  }

  it should "report a valid prediction with correct target after training an entry" in {
    test(new BTB) { dut =>
      // Train: branch at (tag=5, index=3) taken, target = 0x1000
      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(makePC(tag = 5, index = 3).U)
      dut.io.updateTarget.poke(0x1000.U)
      dut.io.branchTaken.poke(true.B)
      dut.io.mispredicted.poke(false.B)
      dut.clock.step(1)

      // Stop updating, look the same PC up
      dut.io.update.poke(false.B)
      dut.io.PC.poke(makePC(tag = 5, index = 3).U)
      dut.clock.step(1)

      dut.io.valid.expect(true.B)
      dut.io.target.expect(0x1000.U)
      dut.io.predictTaken.expect(true.B) // new entry trained as taken -> weakly taken (b10)
    }
  }

  it should "distinguish between different tags mapped to the same set (no false hit)" in {
    test(new BTB) { dut =>
      // Train entry A: tag=1, index=4
      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(makePC(tag = 1, index = 4).U)
      dut.io.updateTarget.poke(0x2000.U)
      dut.io.branchTaken.poke(true.B)
      dut.io.mispredicted.poke(false.B)
      dut.clock.step(1)
      dut.io.update.poke(false.B)

      // Look up a DIFFERENT tag in the SAME set -> must miss
      dut.io.PC.poke(makePC(tag = 2, index = 4).U)
      dut.clock.step(1)
      dut.io.valid.expect(false.B)

      // Look up the ORIGINAL tag -> must hit
      dut.io.PC.poke(makePC(tag = 1, index = 4).U)
      dut.clock.step(1)
      dut.io.valid.expect(true.B)
      dut.io.target.expect(0x2000.U)
    }
  }

  // -----------------------------------------------------------------
  // 2) Handling updates to the BTB properly (target update on re-train)
  // -----------------------------------------------------------------
  it should "update the target address of an existing entry without creating a duplicate" in {
    test(new BTB) { dut =>
      // Initial training
      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(makePC(tag = 7, index = 1).U)
      dut.io.updateTarget.poke(0x3000.U)
      dut.io.branchTaken.poke(true.B)
      dut.io.mispredicted.poke(false.B)
      dut.clock.step(1)

      // Re-train same branch with a NEW target (e.g. branch target moved)
      dut.io.updateTarget.poke(0x4000.U)
      dut.io.branchTaken.poke(true.B)
      dut.clock.step(1)

      dut.io.update.poke(false.B)
      dut.io.PC.poke(makePC(tag = 7, index = 1).U)
      dut.clock.step(1)

      dut.io.valid.expect(true.B)
      dut.io.target.expect(0x4000.U) // target must reflect the latest update
    }
  }

  // -----------------------------------------------------------------
  // 3) Correct eviction behaviour based on LRU replacement policy
  // -----------------------------------------------------------------
  it should "evict the least-recently-used way when a set is full and a third tag arrives" in {
    test(new BTB) { dut =>
      dut.io.mispredicted.poke(false.B)

      // --- Insert tagA into set index=2 ---
      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(makePC(tag = 0xA, index = 2).U)
      dut.io.updateTarget.poke(0xA000.U)
      dut.io.branchTaken.poke(true.B)
      dut.clock.step(1)

      // --- Insert tagB into the SAME set index=2 (fills the second way) ---
      dut.io.updatePC.poke(makePC(tag = 0xB, index = 2).U)
      dut.io.updateTarget.poke(0xB000.U)
      dut.io.branchTaken.poke(true.B)
      dut.clock.step(1)

      // Both ways are now occupied (tagA, tagB). Stop updating.
      dut.io.update.poke(false.B)

      // "Use" tagA via a plain lookup (not an update) so it becomes the
      // most-recently-used entry, and tagB becomes the LRU candidate.
      dut.io.PC.poke(makePC(tag = 0xA, index = 2).U)
      dut.clock.step(1)
      dut.io.valid.expect(true.B)
      dut.io.target.expect(0xA000.U)

      // --- Insert tagC into the SAME set -> must evict tagB (the LRU one) ---
      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(makePC(tag = 0xC, index = 2).U)
      dut.io.updateTarget.poke(0xC000.U)
      dut.io.branchTaken.poke(true.B)
      dut.clock.step(1)
      dut.io.update.poke(false.B)

      // tagB should have been evicted
      dut.io.PC.poke(makePC(tag = 0xB, index = 2).U)
      dut.clock.step(1)
      dut.io.valid.expect(false.B)

      // tagA should still be present (it was recently used, so it survived)
      dut.io.PC.poke(makePC(tag = 0xA, index = 2).U)
      dut.clock.step(1)
      dut.io.valid.expect(true.B)
      dut.io.target.expect(0xA000.U)

      // tagC should be present (the newly inserted entry)
      dut.io.PC.poke(makePC(tag = 0xC, index = 2).U)
      dut.clock.step(1)
      dut.io.valid.expect(true.B)
      dut.io.target.expect(0xC000.U)
    }
  }

  // -----------------------------------------------------------------
  // 4) Accurate 2-bit predictor FSM state transitions
  // -----------------------------------------------------------------
  it should "correctly saturate and transition through all 4 predictor FSM states" in {
    test(new BTB) { dut =>
      dut.io.mispredicted.poke(false.B)

      // Allocate entry as TAKEN -> starts in "weakly taken" (b10) per spec
      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(makePC(tag = 9, index = 5).U)
      dut.io.updateTarget.poke(0x5000.U)
      dut.io.branchTaken.poke(true.B)
      dut.clock.step(1)

      dut.io.update.poke(false.B)
      dut.io.PC.poke(makePC(tag = 9, index = 5).U)
      dut.clock.step(1)
      dut.io.predictTaken.expect(true.B) // weaklyTaken -> predict taken

      // --- Drive it down to strongly-not-taken with repeated NOT-TAKEN updates ---
      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(makePC(tag = 9, index = 5).U)
      dut.io.branchTaken.poke(false.B) // weaklyTaken(10) -> weaklyNotTaken(01)
      dut.clock.step(1)
      dut.io.update.poke(false.B)
      dut.io.PC.poke(makePC(tag = 9, index = 5).U)
      dut.clock.step(1)
      dut.io.predictTaken.expect(false.B)

      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(makePC(tag = 9, index = 5).U)
      dut.io.branchTaken.poke(false.B) // weaklyNotTaken(01) -> stronglyNotTaken(00)
      dut.clock.step(1)
      dut.io.update.poke(false.B)
      dut.io.PC.poke(makePC(tag = 9, index = 5).U)
      dut.clock.step(1)
      dut.io.predictTaken.expect(false.B)

      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(makePC(tag = 9, index = 5).U)
      dut.io.branchTaken.poke(false.B) // stronglyNotTaken(00) -> saturates, stays 00
      dut.clock.step(1)
      dut.io.update.poke(false.B)
      dut.io.PC.poke(makePC(tag = 9, index = 5).U)
      dut.clock.step(1)
      dut.io.predictTaken.expect(false.B) // still not-taken, no wraparound

      // --- Now drive it back up to strongly-taken with repeated TAKEN updates ---
      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(makePC(tag = 9, index = 5).U)
      dut.io.branchTaken.poke(true.B) // stronglyNotTaken(00) -> weaklyNotTaken(01)
      dut.clock.step(1)
      dut.io.update.poke(false.B)
      dut.io.PC.poke(makePC(tag = 9, index = 5).U)
      dut.clock.step(1)
      dut.io.predictTaken.expect(false.B) // still predicts not-taken

      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(makePC(tag = 9, index = 5).U)
      dut.io.branchTaken.poke(true.B) // weaklyNotTaken(01) -> weaklyTaken(10)
      dut.clock.step(1)
      dut.io.update.poke(false.B)
      dut.io.PC.poke(makePC(tag = 9, index = 5).U)
      dut.clock.step(1)
      dut.io.predictTaken.expect(true.B) // now flips to predict-taken

      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(makePC(tag = 9, index = 5).U)
      dut.io.branchTaken.poke(true.B) // weaklyTaken(10) -> stronglyTaken(11)
      dut.clock.step(1)
      dut.io.update.poke(false.B)
      dut.io.PC.poke(makePC(tag = 9, index = 5).U)
      dut.clock.step(1)
      dut.io.predictTaken.expect(true.B)

      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(makePC(tag = 9, index = 5).U)
      dut.io.branchTaken.poke(true.B) // stronglyTaken(11) -> saturates, stays 11
      dut.clock.step(1)
      dut.io.update.poke(false.B)
      dut.io.PC.poke(makePC(tag = 9, index = 5).U)
      dut.clock.step(1)
      dut.io.predictTaken.expect(true.B)
    }
  }

  it should "initialize a newly allocated NOT-TAKEN entry as weakly-not-taken" in {
    test(new BTB) { dut =>
      dut.io.mispredicted.poke(false.B)
      dut.io.update.poke(true.B)
      dut.io.updatePC.poke(makePC(tag = 3, index = 6).U)
      dut.io.updateTarget.poke(0x6000.U)
      dut.io.branchTaken.poke(false.B) // allocate as not-taken -> "b01" weaklyNotTaken
      dut.clock.step(1)

      dut.io.update.poke(false.B)
      dut.io.PC.poke(makePC(tag = 3, index = 6).U)
      dut.clock.step(1)

      dut.io.valid.expect(true.B)
      dut.io.predictTaken.expect(false.B)
    }
  }
}