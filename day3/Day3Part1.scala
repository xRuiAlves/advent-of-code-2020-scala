package day3

import utils.FileReader

object Day3Part1 {
    final val TREE = '#'
    final val OPEN = '.'
    final val SLOPE_RIGHT = 3
    final val SLOPE_DOWN = 1

    def main(args: Array[String]): Unit = {
        val map = FileReader.readFile("Advent-Of-Code-2020/day3/input.txt").toArray
        var x = 0
        var y = 0
        var tree_count = 0

        while (y < map.length) {
            if (map(y)(x % map(y).length) == TREE) tree_count += 1
            x += SLOPE_RIGHT
            y += SLOPE_DOWN
        }

        println(tree_count)
    }
}
