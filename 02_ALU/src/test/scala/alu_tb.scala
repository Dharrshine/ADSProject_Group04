// ADS I Class Project
// Pipelined RISC-V Core with Hazard Detection and Resolution
//
// Chair of Electronic Design Automation, RPTU in Kaiserslautern
// File created on 10/31/2025 by Tobias Jauch (tobias.jauch@rptu.de)

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import Assignment02._

// Test ADD operation
class ALUAddTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Add_Tester" should "test ADD operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)

      dut.io.operandA.poke(10.U)
      dut.io.operandB.poke(10.U)
      dut.io.operation.poke(ALUOp.ADD)
      dut.io.aluResult.expect(20.U)
      dut.clock.step(1)

      // Corner Case: Modulo-2^32 Wraparound
      dut.io.operandA.poke("hFFFFFFFF".U) //Maximum 32-bit value, h stands for hexadecimal
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.ADD)
      dut.clock.step(1)              //This tells the Chisel simulator to tick the hardware clock forward by one cycle. 
                                    // It gives the physical signals time to propagate through the multiplexer and adder gates to reach the output pin.
      
      dut.io.aluResult.expect(0.U) //should wrap around to 0



      //ToDo: add more test cases for ADD operation


    }
  }
}

class ALUSubTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Sub_Tester" should "test SUB operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) {
      dut => dut.clock.setTimeout(0)

      dut.io.operandA.poke(15.U)
      dut.io.operandB.poke(10.U)
      dut.io.operation.poke(ALUOp.SUB)
      dut.io.aluResult.expect(5.U)
      dut.clock.step(1)
    }
  }
}

class ALUAndTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_And_Tester" should "test AND operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) {
      dut => dut.clock.setTimeout(0)

      dut.io.operandA.poke(3.U)
      dut.io.operandB.poke(2.U)
      dut.io.operation.poke(ALUOp.AND)
      dut.io.aluResult.expect(2.U)
      dut.clock.step(1)
    }
  }
}

class ALUOrTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Or_Tester" should "test OR operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) {
      dut => dut.clock.setTimeout(0)

      dut.io.operandA.poke(3.U)
      dut.io.operandB.poke(2.U)
      dut.io.operation.poke(ALUOp.OR)
      dut.io.aluResult.expect(3.U)
      dut.clock.step(1)
    }
  }
}

class ALUXorTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Xor_Tester" should "test XOR operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) {
      dut => dut.clock.setTimeout(0)

      dut.io.operandA.poke(3.U)
      dut.io.operandB.poke(2.U)
      dut.io.operation.poke(ALUOp.XOR)
      dut.io.aluResult.expect(1.U)
      dut.clock.step(1)
    }
  }
}

class ALUSllTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Sll_Tester" should "test SLL operation and 5-bit shift limit" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      
      // Corner Case: Shift amount > 31 (Uses only lowest 5 bits)
      dut.io.operandA.poke(1.U)
      dut.io.operandB.poke(33.U) // 33 = 100001 in binary -> lowest 5 bits = 1
      dut.io.operation.poke(ALUOp.SLL)
      dut.clock.step(1)
      dut.io.aluResult.expect(2.U) // 1 shifted left by 1 is 2
    }
  }
}

class ALUSrlTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Srl_Tester" should "test SRL operation and 5-bit shift limit" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      
      // Standard SRL: Logical shift brings in 0s from the left
      dut.io.operandA.poke("hFFFFFFFF".U)
      dut.io.operandB.poke(4.U)
      dut.io.operation.poke(ALUOp.SRL)
      dut.clock.step(1)
      dut.io.aluResult.expect("h0FFFFFFF".U) 

      // Corner Case: Shift amount > 31 (Uses only lowest 5 bits)
      dut.io.operandA.poke("h80000000".U)
      dut.io.operandB.poke(33.U) // 33 = 100001 in binary -> lowest 5 bits = 1
      dut.io.operation.poke(ALUOp.SRL)
      dut.clock.step(1)
      dut.io.aluResult.expect("h40000000".U) // Shifted right by 1
    }
  }
}


class ALUSraTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Sra_Tester" should "test SRA operation sign extension" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      
      // Corner Case: Arithmetic shift on a negative number (MSB is 1)
      dut.io.operandA.poke("h80000000".U) // MSB is 1
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.SRA)
      dut.clock.step(1)
      dut.io.aluResult.expect("hC0000000".U) // 1 gets shifted in from the left
    }
  }
}



class ALUSltTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Slt_Tester" should "test SLT (signed) operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      
      // Corner Case: Signed comparison (-1 < 1 is True)
      dut.io.operandA.poke("hFFFFFFFF".U) // -1 in two's complement
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.SLT)
      dut.clock.step(1)
      dut.io.aluResult.expect(1.U) 
    }
  }
}

class ALUSltuTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Sltu_Tester" should "test SLTU (unsigned) operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      
      // Corner Case: Unsigned comparison (4,294,967,295 < 1 is False)
      dut.io.operandA.poke("hFFFFFFFF".U) // Max 32-bit unsigned integer
      dut.io.operandB.poke(1.U)
      dut.io.operation.poke(ALUOp.SLTU)
      dut.clock.step(1)
      dut.io.aluResult.expect(0.U) 
    }
  }
}

class ALUPassbTest extends AnyFlatSpec with ChiselScalatestTester {
  "ALU_Passb_Tester" should "test PASSB operation" in {
    test(new ALU).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(0)
      
      dut.io.operandA.poke("hFFFFFFFF".U) // Shouldn't affect the output
      dut.io.operandB.poke(42.U)
      dut.io.operation.poke(ALUOp.PASSB)
      dut.clock.step(1)
      dut.io.aluResult.expect(42.U)
    }
  }
}


// ---------------------------------------------------
// ToDo: Add test classes for all other ALU operations
//----------------------------------------------------
