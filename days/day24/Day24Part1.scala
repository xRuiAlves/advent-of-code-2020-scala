package days.day24

import utils.{DaySolution, FileReader}

import scala.collection.mutable.ArrayBuffer

object Day24Part1 extends DaySolution(24, 1) {
    final val DIRECTION_PREFIXES = Set('n', 's')
    final val GRID_SIZE = 150
    final val REFERENCE_CELL = new Cell(GRID_SIZE / 2, GRID_SIZE / 2)

    final val WHITE = 0
    final val BLACK = 1
    final val grid = Array.ofDim[Int](GRID_SIZE, GRID_SIZE)

    final val STEPS_DELTAS = Map(
        "w" -> new Cell(-1, 0),
        "e" -> new Cell(1, 0),
        "sw" -> new Cell(0, 1),
        "se" -> new Cell(1, 1),
        "nw" -> new Cell(-1, -1),
        "ne" -> new Cell(0, -1)
    )

    override def calculate: String = {
        val paths = input.map(line => parsePath(line))

        paths.foreach(path => {
            val cell = travelPath(path)
            toggleCell(cell)
        })

        numBlackTiles.toString
    }

    def travelPath(path: Array[String]): Cell = {
        var curr = REFERENCE_CELL
        path.foreach(move => curr += STEPS_DELTAS(move))
        curr
    }

    def toggleCell(cell: Cell): Unit = {
        grid(cell.x)(cell.y) = 1 - grid(cell.x)(cell.y)
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
