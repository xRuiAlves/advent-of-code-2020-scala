package day24

import utils.FileReader

import scala.collection.mutable.ArrayBuffer

object Day24Part2 {
    final val DIRECTION_PREFIXES = Set('n', 's')
    final val GRID_SIZE = 200
    final val NUM_DAYS = 100
    final val REFERENCE_CELL = new Cell(GRID_SIZE / 2, GRID_SIZE / 2)

    final val WHITE = 0
    final val BLACK = 1
    private final var grid = Array.ofDim[Int](GRID_SIZE, GRID_SIZE)
    private final var grid_swap = Array.ofDim[Int](GRID_SIZE, GRID_SIZE)

    final val STEPS_DELTAS = Map(
        "w" -> new Cell(-1, 0),
        "e" -> new Cell(1, 0),
        "sw" -> new Cell(0, 1),
        "se" -> new Cell(1, 1),
        "nw" -> new Cell(-1, -1),
        "ne" -> new Cell(0, -1)
    )

    def main(args: Array[String]): Unit = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day24/input.txt").toArray
        val paths = lines.map(line => parsePath(line))

        paths.foreach(path => {
            val cell = travelPath(path)
            toggleCell(cell)
        })

        for (_ <- 0 until NUM_DAYS) {
            applyDailyCycle()
            memSwap()
        }

        println(numBlackTiles)
    }

    def applyDailyCycle(): Unit = {
        for (x <- grid.indices; y <- grid(x).indices) {
            val b_count = countNeighborBlackTiles(new Cell(x, y))
            grid(x)(y) match {
                case BLACK => grid_swap(x)(y) =
                    if (b_count == 0 || b_count > 2) WHITE
                    else grid(x)(y)
                case WHITE => grid_swap(x)(y) =
                    if (b_count == 2) BLACK
                    else grid(x)(y)
            }
        }
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

    def getCellValue(cell: Cell): Int = cell.value match { case (x, y) => getCellValue(x, y) }

    def getCellValue(x: Int, y: Int): Int = {
        if (x < 0 || y < 0 || x >= grid.length || y >= grid(x).length) WHITE
        else grid(x)(y)
    }

    def getCellNeighbors(cell: Cell): Iterable[Int] = STEPS_DELTAS
        .values
        .map(delta => getCellValue(cell + delta))

    def countNeighborBlackTiles(cell: Cell): Int = getCellNeighbors(cell).count(_ == BLACK)

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

    def memSwap(): Unit = {
        val temp = grid
        grid = grid_swap
        grid_swap = temp
    }
}
