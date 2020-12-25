package days.day8

import utils.{DaySolution, FileReader}

import scala.collection.mutable

object Day8Part2 extends DaySolution(8, 2) {
    final val NOP = "nop"
    final val ACC = "acc"
    final val JMP = "jmp"

    var accumulator = 0
    var pc = 0

    override def calculate: String = {
        val instructions = input.map(parseInstruction)

        for (i <- instructions.indices) {
            instructions(i) = swapInstruction(instructions(i))
            if (execProgram(instructions.clone())) {
                return accumulator.toString
            }
            instructions(i) = swapInstruction(instructions(i))
        }

        throw new Exception("No solution found")
    }

    def execProgram(instructions: Array[(String, Int)]): Boolean = {
        val visited_instructions = new mutable.HashSet[Int]()
        accumulator = 0
        pc = 0

        while (pc >= 0 && pc < instructions.length) {
            if (visited_instructions.contains(pc)) {
                return false
            } else {
                visited_instructions.add(pc)
                execInstruction(instructions(pc))
            }
        }
        true
    }

    def swapInstruction(instruction: (String, Int)): (String, Int) = instruction match {
        case (JMP, op) => (NOP, op)
        case (NOP, op) => (JMP, op)
        case instr => instr
    }

    def parseInstruction(instruction: String): (String, Int) = {
        val tokens = instruction.split(" ")
        (tokens(0), tokens(1).toInt)
    }

    def execInstruction(instruction: (String, Int)): Unit = {
        instruction match {
            case (ACC, op) => accumulator += op
            case (JMP, op) => pc += op - 1
            case _ =>
        }
        pc += 1
    }
}