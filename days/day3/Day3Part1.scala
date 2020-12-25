package days.day3

import utils.{DaySolution, FileReader}

object Day3Part1 extends DaySolution(3, 1) {
    final val TREE = '#'
    final val OPEN = '.'
    final val SLOPE_RIGHT = 3
    final val SLOPE_DOWN = 1

    override def calculate: String = {
        val map = input
        var x = 0
        var y = 0
        var tree_count = 0

        while (y < map.length) {
            if (map(y)(x % map(y).length) == TREE) tree_count += 1
            x += SLOPE_RIGHT
            y += SLOPE_DOWN
        }

        tree_count.toString
    }
}
