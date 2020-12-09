package day9

import utils.FileReader

import scala.collection.mutable

object Day9Part1 {
    final val PREAMBLE_SIZE = 25

    def main(args: Array[String]): Unit = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day9/input.txt").toArray.map(_.toLong)

        for (i <- PREAMBLE_SIZE until lines.length) {
            if (!sumTwo(lines, i)) {
                println(lines(i))
                return
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
