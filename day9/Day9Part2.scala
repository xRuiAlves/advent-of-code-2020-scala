package day9

import utils.FileReader

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Day9Part2 {
    final val PREAMBLE_SIZE = 25

    def main(args: Array[String]): Unit = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day9/input.txt").toArray.map(_.toLong)
        val target_sum = findInvalidNum(lines)

        val window = new ArrayBuffer[Long]
        var curr_sum: Long = 0L

        for (num <- lines) {
            curr_sum += num
            window.addOne(num)

            while (curr_sum > target_sum) {
                curr_sum -= window.head
                window.remove(0)
            }

            if (curr_sum == target_sum) {
                println(getWeakness(window))
                return
            }
        }

        throw new Exception("Solution not found")
    }

    def getWeakness(window: ArrayBuffer[Long]): Long = window.min + window.max

    def findInvalidNum(lines: Array[Long]): Long = {
        val prev = new ArrayBuffer[Long]()

        for (i <- 0 until PREAMBLE_SIZE) {
            prev.addOne(lines(i))
        }

        for (i <- PREAMBLE_SIZE until lines.length) {
            if (!sumTwo(prev, lines(i))) {
                return lines(i)
            } else {
                prev.remove(0)
                prev.addOne(lines(i))
            }
        }

        throw new Exception("Solution not found")
    }

    def sumTwo(arr: ArrayBuffer[Long], target_sum: Long): Boolean = {
        val visited = new mutable.HashSet[Long]()

        for (num <- arr) {
            if (visited.contains(target_sum - num)) return true
            visited.add(num)
        }

        false
    }
}