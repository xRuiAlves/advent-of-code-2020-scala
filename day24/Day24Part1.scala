package day24

import utils.FileReader

import scala.collection.mutable.ArrayBuffer

object Day24Part1 {
    type Cell = (Int, Int)

    final val DIRECTION_PREFIXES = Set('n', 's')
    final val GRID_SIZE = 150
    final val REFERENCE_CELL = (GRID_SIZE / 2, GRID_SIZE / 2)

    final val WHITE = 0
    final val BLACK = 1
    final val grid = Array.ofDim[Int](GRID_SIZE, GRID_SIZE)

    final val STEPS_DELTAS = Map(
        "w" -> (-1, 0),
        "e" -> (1, 0),
        "sw" -> (0, 1),
        "se" -> (1, 1),
        "nw" -> (-1, -1),
        "ne" -> (0, -1)
    )

    def main(args: Array[String]): Unit = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day24/input.txt").toArray
        val paths = lines.map(line => parsePath(line))

        paths.foreach(path => {
            val cell = travelPath(path)
            toggleCell(cell)
        })

        println(numBlackTiles)
    }

    def travelPath(path: Array[String]): Cell = {
        var curr = REFERENCE_CELL
        path.foreach(move => curr = applyDelta(curr, STEPS_DELTAS(move)))
        curr
    }

    def applyDelta(cell: Cell, delta: Cell): Cell = (cell._1 + delta._1, cell._2 + delta._2)

    def toggleCell(cell: Cell): Unit = cell match {
        case (x, y) => grid(x)(y) = 1 - grid(x)(y)
    }

    def numBlackTiles: Int = grid
        .map(row => row.count(_ == BLACK))
        .sum

    def parsePath(line: String): Array[String] = {
        val steps = new ArrayBuffer[String]()
        var i = 0

        while (i < line.length) {
            steps.addOne(
                if (DIRECTION_PREFIXES.contains(line(i))) line.substring(i, i + 2)
                else line(i).toString
            )
            i += steps.last.length
        }
        steps.toArray
    }
}
