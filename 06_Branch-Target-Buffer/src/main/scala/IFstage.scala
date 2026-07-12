// ADS I Class Project
// Pipelined RISC-V Core - IF Stage
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 01/09/2026 by Tobias Jauch (@tojauch)

/*
The Instruction Fetch (IF) stage is the first stage of the pipeline and handles instruction retrieval from memory.

Memory:
    IMem: instruction memory with 4096 32-bit unsigned integer entires, loaded from a binary file at compile time

Internal Registers:
    PC: 32-bit unsigned integer register, initialized to 0 holding the current program counter address

Internal Signals:
    none

Functionality:
    Fetch the instruction at the current PC (word-aligned addressing)
    Increment the PC (word-aligned) each clock cycle to fetch the next sequential instruction

Parameters:
    BinaryFile: String - path to the binary file to load into instruction memory

Inputs:
    none

Outputs:
    instr: send the fetched instruction to IF Barrier
*/

package core_tile

import chisel3._
import chisel3.util.experimental.loadMemoryFromFile

// -----------------------------------------
// Fetch Stage
// -----------------------------------------

class IFStage (BinaryFile: String) extends Module {
  val io = IO(new Bundle {
    // ToDo: Add I/O ports
    val instr = Output(UInt(32.W))
    val PC = Output(UInt(32.W))

    // Branch control from EX stage
    val inPCNewEx = Input(UInt(32.W))

    //flush
    val inFlush = Input(Bool())

    //BTB
    val btbValid = Input(Bool())
    val btbTarget = Input(UInt(32.W))
    val btbPredictTaken = Input(Bool())
    val btbLookupPC = Output(UInt(32.W))
  })

  //ToDo: Add your implementation according to the specification above here
  val PC = RegInit(0.U(32.W))
  val IMem = Mem(4096, UInt(32.W))
  val nextPC = WireDefault(PC + 4.U)

  loadMemoryFromFile(IMem, BinaryFile)

  io.PC := PC
  io.instr := IMem(PC >> 2.U)
  io.btbLookupPC := PC  // Send PC to BTB for lookup

  // Priority 1: Execute stage flush (misprediction recovery)
  when(io.inFlush) {
    nextPC := io.inPCNewEx
  }.otherwise {
    // Priority 2: Dynamic prediction from BTB
    when(io.btbValid && io.btbPredictTaken) {
      nextPC := io.btbTarget
    }.otherwise {
      // Priority 3: Normal sequential flow
      nextPC := PC + 4.U
    }
  }

  PC := nextPC


  //debugging
printf("==============================\n")
    printf(" IF STAGE DEBUG (PC: %x)\n", PC)
    printf("------------------------------\n")
    printf(" PC            : %x\n", PC)
    printf(" Next PC       : %x\n", nextPC)
    printf(" BTB Valid     : %d\n", io.btbValid)
    printf(" BTB Predict   : %d\n", io.btbPredictTaken)
    printf(" BTB Target    : %x\n", io.btbTarget)
    printf(" Flush (In)    : %d\n", io.inFlush)
    printf("==============================\n\n")


}
