// ADS I Class Project
// Assignment 02: Arithmetic Logic Unit and UVM Testbench
//
// Chair of Electronic Design Automation, RPTU University Kaiserslautern-Landau
// File created on 09/21/2025 by Tharindu Samarakoon (gug75kex@rptu.de)
// File updated on 10/29/2025 by Tobias Jauch (tobias.jauch@rptu.de)

package Assignment02

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

//ToDo: define AluOp Enum

object ALUOp extends ChiselEnum {
  val ADD, SUB, AND, OR, XOR, SLL, SRL, SRA, SLT, SLTU, PASSB = Value 
}

class ALU extends Module {
  
  val io = IO(new Bundle {
    //ToDo: define IOs

    val operandA = Input(UInt(32.W))
    val operandB = Input(UInt(32.W))
    val operation = Input(ALUOp())
    val aluResult = Output(UInt(32.W))
  })

  // Set a safe default output so the ALU maintains no internal state
  io.aluResult := 0.U
  
  /*
The Multiplexer: switch(io.operation)
In hardware, a switch statement generates a combinational multiplexer.
The io.operation signal (which contains your RV32I instruction control field) acts as the selector pin for this MUX.
It determines which internal hardware circuit gets routed to the final output.

  */

  switch(io.operation) {
    is(ALUOp.ADD) {
      // Chisel's '+' operator automatically handles the modulo-2^32 
      // wraparound for 32-bit UInts natively.
      io.aluResult := io.operandA + io.operandB

      /*

      When you write the + operator, Chisel instantiates a physical 32-bit adder circuit.
      Because you defined your operands as 32-bit Unsigned Integers (UInt(32.W)), Chisel builds an adder with exactly 32 bits of output.
      If you add $1$ to the maximum 32-bit number (which is 0xFFFFFFFF), the actual mathematical answer requires 33 bits.
      
      However, since the hardware only has 32 physical wires for the output, that 33rd "carry" bit is simply dropped.
      The hardware naturally "wraps around" back to $0$. The comment is just reminding you that you don't have to write any extra logic to handle overflow limits;
      the physical constraints of the 32-bit wires handle the modulo-2^32 RISC-V specification automatically.


      */

      
    }
    is(ALUOp.SUB) {
      io.aluResult := io.operandA - io.operandB
    }
    is(ALUOp.AND) {
      io.aluResult := io.operandA & io.operandB
    }
    is(ALUOp.OR) {
      io.aluResult := io.operandA | io.operandB
    }
    is(ALUOp.XOR) {
      io.aluResult := io.operandA ^ io.operandB
    }
    is(ALUOp.SLL) {
      io.aluResult := io.operandA << io.operandB(4, 0)
    }
    is(ALUOp.SRL) {
      io.aluResult := io.operandA >> io.operandB(4, 0)
    }
    is(ALUOp.SRA) { 
      // Cast to SInt for arithmetic shift, then back to UInt for output
      io.aluResult := (io.operandA.asSInt >> io.operandB(4, 0)).asUInt 
    }
    is(ALUOp.SLT) { 
      io.aluResult := Mux(io.operandA.asSInt < io.operandB.asSInt, 1.U, 0.U) 
    }
    is(ALUOp.SLTU) { 
      io.aluResult := Mux(io.operandA < io.operandB, 1.U, 0.U) 
    }
     is(ALUOp.PASSB) { io.aluResult := io.operandB }
    
    
  }


  //ToDo: implement ALU functionality according to the task specification

}