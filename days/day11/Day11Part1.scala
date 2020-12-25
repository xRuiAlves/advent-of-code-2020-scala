package days.day11

import utils.{DaySolution, FileReader}

object Day11Part1 extends DaySolution(11, 1) {
    final val EMPTY = 'L'
    final val OCCUPIED = '#'
    final val FLOOR = '.'
    final val INVALID = '_'

    override def calculate: String = {
        var map = input.map(_.split("").map(_.last))
        var map_updated = true

        while (map_updated) {
            val update = updateMap(map)
            map_updated = update._1
            map = update._2
        }

        countOccupiedSeats(map).toString
    }

    def updateMap(map: Array[Array[Char]]): (Boolean, Array[Array[Char]]) = {
        var updated = false
        val new_map = map.map(_.clone())

        for (i <- map.indices; j <- map(i).indices) {
            val adjacent = getAdjacent(map, i, j)
            val num_adj_occupied = adjacent.count(_ == OCCUPIED)

            if (map(i)(j) == EMPTY && num_adj_occupied == 0) {
                new_map(i)(j) = OCCUPIED
                updated = true
            } else if (map(i)(j) == OCCUPIED & num_adj_occupied >= 4) {
                new_map(i)(j) = EMPTY
                updated = true
            }
        }
        (updated, new_map)
    }

    def getAdjacent(map: Array[Array[Char]], i: Int, j: Int): Array[Char] = Array(
        getCell(map, i - 1, j - 1),
        getCell(map, i - 1, j),
        getCell(map, i - 1, j + 1),
        getCell(map, i, j - 1),
        getCell(map, i, j + 1),
        getCell(map, i + 1, j - 1),
        getCell(map, i + 1, j),
        getCell(map, i + 1, j + 1)
    )

    def getCell(map: Array[Array[Char]], i: Int, j: Int): Char = {
        if (i < 0 || j < 0 || i >= map.length || j >= map(i).length) INVALID else map(i)(j)
    }

    def countOccupiedSeats(map: Array[Array[Char]]): Int = map.map(line => line.count(_ == OCCUPIED)).sum
}
