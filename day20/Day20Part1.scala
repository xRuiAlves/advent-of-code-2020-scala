package day20

import utils.FileReader

object Day20Part1 {
    def main(args: Array[String]): Unit = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day20/input.txt").toArray
        val tiles = lines
            .mkString("\n")
            .split("\n\n")
            .map(raw_tile => new Tile(raw_tile))


        val corner_tiles = tiles
            .map(tile => (tile.id, countSharedTileBorders(tiles, tile)))
            .filter(_._2 == 2)
            .map(_._1.toLong)

        println(corner_tiles.product)
    }

    def getBorderIntersectionCount(t1: Tile, t2: Tile): Int = {
        t1.border_combinations.count(b1 => t2.border_combinations.exists(b2 => b1.equals(b2)))
    }

    def countSharedTileBorders(tiles: Array[Tile], tile: Tile): Int = {
        tiles
            .filter(_.id != tile.id)
            .map(t2 => getBorderIntersectionCount(tile, t2))
            .sum / 2
    }
}
