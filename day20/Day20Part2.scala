package day20

import utils.FileReader
import utils.MatrixOps

object Day20Part2 {
    final val BLUR_CELL = '#'
    final val TAGGED_MONSTER_CELL = 'O'

    def main(args: Array[String]): Unit = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day20/input.txt").toArray

        val tile_sets = lines
            .mkString("\n")
            .split("\n\n")
            .map(raw_tile => new TileSet(raw_tile))

        for (i <- tile_sets.indices; j <- (i + 1) until tile_sets.length) {
            val ts1 = tile_sets(i)
            val ts2 = tile_sets(j)

            ts1.tiles.foreach(t1 => {
                ts2.tiles.foreach(t2 => {
                    matchTiles(t1, t2)
                })
            })
        }

        val tile_puzzle_size = math.round(math.sqrt(tile_sets.length)).toInt
        val tile_puzzle = Array.ofDim[Tile](tile_puzzle_size, tile_puzzle_size)

        val tiles = tile_sets.flatMap(_.tiles)
        val isAssembled = assemblePuzzle(tiles, tile_puzzle, 0)

        if (!isAssembled) throw new Exception("Puzzle assembly is impossible.")

        val puzzle = mergePuzzle(tile_puzzle)
        val count = countSafeBlurCells(puzzle)
        println(count)
    }

    def matchTiles(t1: Tile, t2: Tile): Unit = {
        if (t1.right == t2.left) {
            t1.right_neighbor = t2
            t2.left_neighbor = t1
        }
        if (t1.left == t2.right) {
            t1.left_neighbor = t2
            t2.right_neighbor = t1
        }
        if (t1.top == t2.bottom) {
            t1.top_neighbor = t2
            t2.bottom_neighbor = t1
        }
        if (t1.bottom == t2.top) {
            t1.bottom_neighbor = t2
            t2.top_neighbor = t1
        }
    }

    def assemblePuzzle(tiles: Array[Tile], puzzle: Array[Array[Tile]], id: Int): Boolean = {
        if (id >= puzzle.length * puzzle.length) return true

        val row = id / puzzle.length
        val col = id % puzzle.length

        val top_neighbor = getCell(puzzle, row - 1, col)
        val bottom_neighbor = getCell(puzzle, row + 1, col)
        val left_neighbor = getCell(puzzle, row, col - 1)
        val right_neighbor = getCell(puzzle, row, col + 1)

        def tileFits(tile: Tile): Boolean =
            (top_neighbor == null || tile.top_neighbor == top_neighbor) &&
            (bottom_neighbor == null || tile.bottom_neighbor == bottom_neighbor) &&
            (left_neighbor == null || tile.left_neighbor == left_neighbor) &&
            (right_neighbor == null || tile.right_neighbor == right_neighbor)

        for (candidate <- tiles) {
            if (tileFits(candidate)) {
                puzzle(row)(col) = candidate
                if (assemblePuzzle(tiles, puzzle, id + 1)) {
                    return true
                }
                puzzle(row)(col) = null
            }
        }
        false
    }

    def getCell(puzzle: Array[Array[Tile]], row: Int, col: Int): Tile = {
        if (row >= 0 && col >= 0 && row < puzzle.length && col < puzzle(row).length) puzzle(row)(col)
        else null
    }

    def mergePuzzle(puzzle: Array[Array[Tile]]): Array[Array[Char]] = {
        val tile_size = puzzle.head.head.img.length - 2
        val side_size = tile_size * puzzle.length
        val merged_puzzle = Array.ofDim[Char](side_size, side_size)

        for (i <- puzzle.indices; j <- puzzle(i).indices) {
            val tile = puzzle(i)(j)
            val row_padding = i * tile_size
            val col_padding = j * tile_size
            for (row <- 0 until tile.img.length - 2; col <- 0 until tile.img(row).length - 2) {
                merged_puzzle(row_padding + row)(col_padding + col) = tile.img(row + 1)(col + 1)
            }
        }
        merged_puzzle
    }

    def countSafeBlurCells(puzzle: Array[Array[Char]]): Int = {
        var curr_puzzle = puzzle

        for (i <- 1 to 8) {
            val num_monsters = countMonsters(curr_puzzle)
            if (num_monsters > 0) {
                return countBlurCells(curr_puzzle) - num_monsters * Monster.num_body_cells
            }

            curr_puzzle = MatrixOps.rotateMatrix(curr_puzzle)
            if (i % 4 == 0) curr_puzzle = MatrixOps.flipMatrix(curr_puzzle)
        }

        throw new Exception("No monsters found")
    }

    def countBlurCells(puzzle: Array[Array[Char]]): Int = puzzle
        .map(line => line.count(_ == BLUR_CELL))
        .sum

    def countMonsters(puzzle: Array[Array[Char]]): Int = {
        var count = 0
        for (i <- 0 until (puzzle.length - Monster.height); j <- 0 until (puzzle(i).length - Monster.width)) {
            if (Monster.isMonster(puzzle, i, j)) {
                count += 1
            }
        }
        count
    }
}
