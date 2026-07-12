// ADS I Class Project
// Pipelined RISC-V Core - Branch Target Buffer
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 05/12/2026 by Tobias Jauch (@tojauch)

/*
Branch Target Buffer (BTB): a hardware component that predicts the target address of conditional branch instructions to improve pipeline performance

Functionality (cf. slide 6-48 of the lecture slides):
    Stores target addresses and prediction information for conditional branch instructions
    On a branch instruction, checks if the instruction is in the BTB and retrieves the predicted target address and prediction state
    If the prediction is taken, the processor fetches the instruction from the predicted target address; if not taken, it continues sequentially
    Updates the BTB entry based on the actual outcome of the branch instruction (taken or not taken) and updates the prediction state accordingly

Inputs:
    PC: A 32-bit program counter representing the address of the branch instruction being fetched or executed.
    update: A 1-bit signal indicating whether the BTB should be updated with new information.
    updatePC: A 32-bit program counter associated with the branch instruction being updated.
    updateTarget: A 32-bit branch target address to be stored in the BTB.
    mispredicted: A 1-bit signal indicating whether the prediction turned out to be incorrect during execution (used to update the predictor).

Outputs:
    valid: A 1-bit signal indicating whether the BTB has a valid prediction for the provided program counter.
    target: A 32-bit signal representing the predicted branch target address when a valid prediction exists.
    predictTaken: A 1-bit signal indicating whether the branch is predicted to be taken or not.

*/

package core_tile

import chisel3._
import chisel3.util._

class BTB extends Module {

  val io = IO(new Bundle {
    val PC             = Input(UInt(32.W))
    val update         = Input(Bool())
    val updatePC       = Input(UInt(32.W))
    val updateTarget   = Input(UInt(32.W))
    val mispredicted   = Input(Bool())
    val branchTaken    = Input(Bool())

    val valid          = Output(Bool())
    val target         = Output(UInt(32.W))
    val predictTaken   = Output(Bool())
  })

  //------------------------------------------------------------
  // Parameters
  //------------------------------------------------------------

  val NUM_SETS  = 8
  val NUM_WAYS  = 2

  val INDEX_BITS = log2Ceil(NUM_SETS)
  val TAG_BITS   = 32 - INDEX_BITS - 2

  //------------------------------------------------------------
  // BTB Entry
  //------------------------------------------------------------

  class BTBEntry extends Bundle {
    val valid     = Bool()
    val tag       = UInt(TAG_BITS.W)
    val target    = UInt(32.W)
    val predictor = UInt(2.W)
  }

  //------------------------------------------------------------
  // Empty Entry
  //------------------------------------------------------------

  val emptyEntry = Wire(new BTBEntry)

  emptyEntry.valid := false.B
  emptyEntry.tag := 0.U
  emptyEntry.target := 0.U
  emptyEntry.predictor := "b10".U // Weakly Taken

  //------------------------------------------------------------
  // BTB Memory
  //------------------------------------------------------------

  val btb = RegInit(
    VecInit(
      Seq.fill(NUM_SETS)(
        VecInit(
          Seq.fill(NUM_WAYS)(emptyEntry)
        )
      )
    )
  )

  //------------------------------------------------------------
  // LRU
  //------------------------------------------------------------

  val lru = RegInit(VecInit(Seq.fill(NUM_SETS)(0.U(1.W))))

  //------------------------------------------------------------
  // Lookup Address
  //------------------------------------------------------------

  val index = io.PC(INDEX_BITS + 1, 2)
  val tag   = io.PC(31, INDEX_BITS + 2)

  //------------------------------------------------------------
  // Update Address
  //------------------------------------------------------------

  val updateIndex = io.updatePC(INDEX_BITS + 1, 2)
  val updateTag   = io.updatePC(31, INDEX_BITS + 2)

  //------------------------------------------------------------
  // Lookup
  //------------------------------------------------------------

  val hit = WireDefault(false.B)
  val hitWay = WireDefault(0.U(1.W))
  val hitTarget = WireDefault(0.U(32.W))
  val hitPredictor = WireDefault("b10".U(2.W))

  for (way <- 0 until NUM_WAYS) {
    when(btb(index)(way).valid &&
         btb(index)(way).tag === tag) {

      hit := true.B
      hitWay := way.U
      hitTarget := btb(index)(way).target
      hitPredictor := btb(index)(way).predictor
    }
  }

  //------------------------------------------------------------
  // Outputs
  //------------------------------------------------------------

  io.valid := hit
  io.target := hitTarget
  io.predictTaken := hit && hitPredictor(1)

  //------------------------------------------------------------
  // BTB Update
  //------------------------------------------------------------

  when(io.update) {

    val updateHit = WireDefault(false.B)
    val updateWay = WireDefault(0.U(1.W))

    for (way <- 0 until NUM_WAYS) {
      when(btb(updateIndex)(way).valid &&
           btb(updateIndex)(way).tag === updateTag) {

        updateHit := true.B
        updateWay := way.U
      }
    }

    when(updateHit) {

      //----------------------------------------
      // Existing entry
      //----------------------------------------

      btb(updateIndex)(updateWay).target := io.updateTarget

      when(io.branchTaken) {

        when(btb(updateIndex)(updateWay).predictor =/= "b11".U) {
          btb(updateIndex)(updateWay).predictor :=
            btb(updateIndex)(updateWay).predictor + 1.U
        }

      }.otherwise {

        when(btb(updateIndex)(updateWay).predictor =/= "b00".U) {
          btb(updateIndex)(updateWay).predictor :=
            btb(updateIndex)(updateWay).predictor - 1.U
        }

      }

      lru(updateIndex) := updateWay

    }.otherwise {

      //----------------------------------------
      // Allocate new entry
      //----------------------------------------

      val replaceWay = ~lru(updateIndex)

      btb(updateIndex)(replaceWay).valid := true.B
      btb(updateIndex)(replaceWay).tag := updateTag
      btb(updateIndex)(replaceWay).target := io.updateTarget

      when(io.branchTaken) {
        btb(updateIndex)(replaceWay).predictor := "b10".U
      }.otherwise {
        btb(updateIndex)(replaceWay).predictor := "b01".U
      }

      lru(updateIndex) := replaceWay
    }
  }

  //------------------------------------------------------------
  // Update LRU on Lookup Hit
  //------------------------------------------------------------

  when(hit && !io.update) {
    lru(index) := hitWay
  }


  // 1. Debug Look
  printf("==============================\n")
    printf(" BTB MODULE UPDATE (LEARNING)\n")
    printf("------------------------------\n")
    printf(" Update PC     : %x\n", io.updatePC)
    printf(" Update Target : %x\n", io.updateTarget)
    printf(" Actual Taken  : %d\n", io.branchTaken)
    printf(" Mispredicted  : %d\n", io.mispredicted)
    printf(" Way Updated   : %d\n", hitWay) // If Way 0 or 1 was selected
    printf("==============================\n\n")

 
}
