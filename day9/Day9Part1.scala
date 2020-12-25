package day9

import utils.{DaySolution, FileReader}

import scala.collection.mutable

object Day9Part1 extends DaySolution(9, 1) {
    final val PREAMBLE_SIZE = 25

    override def calculate: String = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day9/input.txt").toArray.map(_.toLong)

        for (i <- PREAMBLE_SIZE until lines.length) {
            if (!sumTwo(lines, i)) {
                return lines(i).toString
            }
        }

        throw new Exception("Solution not found")
    }

    def sumTwo(arr: Array[Long], idx: Int): Boolean = {
        val target_sum = arr(idx)
        val visited = new mutable.HashSet[Long]()

        for (i <- idx - PREAMBLE_SIZE until idx) {
            if (visited.contains(target_sum - arr(i))) return true
            visited.add(arr(i))
        }

        false
    }
}
