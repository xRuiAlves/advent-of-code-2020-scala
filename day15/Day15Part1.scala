package day15

import utils.FileReader

import scala.collection.mutable

object Day15Part1 {
    final val TARGET_NUMBER_ITERATION = 2020
    final val UNVISITED = 0

    def main(args: Array[String]): Unit = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day15/input.txt").toArray
        val initial_numbers = lines.last.split(",").map(_.toInt)
        val visited = new mutable.HashMap[Int, Int]()
        var curr = initial_numbers(0)

        for (i <- 1 until initial_numbers.length) {
            visited(curr) = i
            curr = initial_numbers(i)
        }

        for (i <- initial_numbers.length until TARGET_NUMBER_ITERATION) {
            if (visited.contains(curr)) {
                val diff = i - visited(curr)
                visited(curr) = i
                curr = diff
            } else {
                visited(curr) = i
                curr = UNVISITED
            }
        }

        println(curr)
    }
}
