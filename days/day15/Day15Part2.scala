package days.day15

import utils.{DaySolution, FileReader}

import scala.collection.mutable

object Day15Part2 extends DaySolution(15, 2) {
    final val TARGET_NUMBER_ITERATION = 30000000
    final val UNVISITED = 0

    override def calculate: String = {
        val initial_numbers = input.last.split(",").map(_.toInt)
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

        curr.toString
    }
}
