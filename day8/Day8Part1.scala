package day8

import utils.FileReader

import scala.collection.mutable

object Day8Part1 {
    final val NOP = "nop"
    final val ACC = "acc"
    final val JMP = "jmp"

    var accumulator = 0
    var pc = 0

    def main(args: Array[String]): Unit = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day8/input.txt").toArray
        val instructions = lines.map(parseInstruction)

        var loop_found = false
        val visited_instructions = new mutable.HashSet[Int]()

        while (!loop_found) {
            if (pc < 0 || pc >= instructions.length) {
                throw new Exception("Program counter out of bounds")
            }

            if (visited_instructions.contains(pc)) {
                loop_found = true
            } else {
                visited_instructions.add(pc)
                execInstruction(instructions(pc))
            }
        }

        println(accumulator)
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