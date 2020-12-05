package day5

import utils.FileReader

object Day5Part2 {
    final val NUM_ROWS = 128
    final val NUM_COLS = 8
    final val NUM_ROW_INDICATORS = 7
    final val NUM_COL_INDICATORS = 3

    def main(args: Array[String]): Unit = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day5/input.txt").toList
        val ids = lines.map(line => getId(line)).sortWith(_<_)

        for (i <- 0 until ids.length - 1) {
            if (ids(i) + 1 != ids(i + 1)) {
                println(ids(i) + 1)
                return
            }
        }
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