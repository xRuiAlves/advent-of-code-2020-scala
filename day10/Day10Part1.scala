package day10

import utils.FileReader

object Day10Part1 {
    def main(args: Array[String]): Unit = {
        val joltages = FileReader.readFile("Advent-Of-Code-2020/day10/input.txt").map(_.toInt).toArray.sortWith(_ < _)
        var ones_count = 1
        var threes_count = 1

        for (i <- 0 until (joltages.length - 1)) {
            val diff = joltages(i + 1) - joltages(i)
            ones_count += (if (diff == 1) 1 else 0)
            threes_count += (if (diff == 3) 1 else 0)
        }

        println(ones_count * threes_count)
    }
}
