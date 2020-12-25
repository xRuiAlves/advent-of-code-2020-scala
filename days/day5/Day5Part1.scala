package days.day5

import utils.{DaySolution, FileReader}

object Day5Part1 extends DaySolution(5, 1) {
    final val NUM_ROWS = 128
    final val NUM_COLS = 8
    final val NUM_ROW_INDICATORS = 7
    final val NUM_COL_INDICATORS = 3

    override def calculate: String = {
        val max_id = input.map(line => getId(line)).max
        max_id.toString
    }

    def getId(coordinates: String): Int = {
        var ri = 0
        var rf = NUM_ROWS - 1

        for (i <- 0 until NUM_ROW_INDICATORS) {
            val m = (rf - ri + 1) / 2
            if (coordinates(i) == 'F') rf = ri + m - 1
            else ri = ri + m
        }

        var ci = 0
        var cf = NUM_COLS - 1

        for (i <- NUM_ROW_INDICATORS until (NUM_ROW_INDICATORS + NUM_COL_INDICATORS)) {
            val m = (cf - ci + 1) / 2
            if (coordinates(i) == 'L') cf = ci + m - 1
            else ci = ci + m
        }

        ri * NUM_COLS + ci
    }
}