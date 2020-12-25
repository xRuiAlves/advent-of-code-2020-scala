package days.day10

import utils.{DaySolution, FileReader}

object Day10Part1 extends DaySolution(10, 1) {
    override def calculate: String = {
        val joltages = input.map(_.toInt).toArray.sortWith(_ < _)
        var ones_count = 1
        var threes_count = 1

        for (i <- 0 until (joltages.length - 1)) {
            val diff = joltages(i + 1) - joltages(i)
            ones_count += (if (diff == 1) 1 else 0)
            threes_count += (if (diff == 3) 1 else 0)
        }

        (ones_count * threes_count).toString
    }
}
