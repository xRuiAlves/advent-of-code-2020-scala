package day11

import utils.FileReader

import scala.collection.mutable

object Day11Part2 {
    final val EMPTY = 'L'
    final val OCCUPIED = '#'
    final val FLOOR = '.'
    final val INVALID = '_'

    val neighbors_map = new mutable.HashMap[(Int, Int), Array[(Int, Int)]]()

    def main(args: Array[String]): Unit = {
        var map = FileReader.readFile("Advent-Of-Code-2020/day11/input.txt").toArray.map(_.split("").map(_.last))
        populateNeighbors(map)
        var map_updated = true

        while (map_updated) {
            val update = updateMap(map)
            map_updated = update._1
            map = update._2
        }

        println(countOccupiedSeats(map))
    }

    def updateMap(map: Array[Array[Char]]): (Boolean, Array[Array[Char]]) = {
        var updated = false
        val new_map = map.map(_.clone())

        for (i <- map.indices; j <- map(i).indices) {
            val adjacent = neighbors_map((i, j)).map(cell => map(cell._1)(cell._2))
            val num_adj_occupied = adjacent.count(_ == OCCUPIED)

            if (map(i)(j) == EMPTY && num_adj_occupied == 0) {
                new_map(i)(j) = OCCUPIED
                updated = true
            } else if (map(i)(j) == OCCUPIED & num_adj_occupied >= 5) {
                new_map(i)(j) = EMPTY
                updated = true
            }
        }
        (updated, new_map)
    }

    def getCell(map: Array[Array[Char]], i: Int, j: Int): Char = {
        if (isInBounds(map, i, j)) map(i)(j) else INVALID
    }

    def countOccupiedSeats(map: Array[Array[Char]]): Int = map.map(line => line.count(_ == OCCUPIED)).sum

    def populateNeighbors(map: Array[Array[Char]]): Unit = {
        for (i <- map.indices; j <- map(i).indices) {
            neighbors_map((i, j)) = Array(
                findNeighbor(map, i, j, -1, -1),
                findNeighbor(map, i, j, -1, 0),
                findNeighbor(map, i, j, -1, 1),
                findNeighbor(map, i, j, 0, -1),
                findNeighbor(map, i, j, 0, 1),
                findNeighbor(map, i, j, 1, -1),
                findNeighbor(map, i, j, 1, 0),
                findNeighbor(map, i, j, 1, 1),
            ).filter(_ != null)

        }
    }

    def findNeighbor(map: Array[Array[Char]], i: Int, j: Int, diff_i: Int, diff_j: Int): (Int, Int) = {
        var curr_y = i + diff_i
        var curr_x = j + diff_j

        while (isInBounds(map, curr_y, curr_x)) {
            if (map(curr_y)(curr_x) == OCCUPIED || map(curr_y)(curr_x) == EMPTY) return (curr_y, curr_x)
            curr_y += diff_i
            curr_x += diff_j
        }

        null
    }

    def isInBounds(map: Array[Array[Char]], i: Int, j: Int): Boolean = {
        i >= 0 && j >= 0 && i < map.length && j < map(i).length
    }
}
