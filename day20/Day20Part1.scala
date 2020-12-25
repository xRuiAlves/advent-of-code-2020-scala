package day20

import utils.{DaySolution, FileReader}

object Day20Part1 extends DaySolution(20, 1) {
    override def calculate: String = {
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

        val corner_ids = tile_sets.flatMap(ts => ts.tiles
            .filter(t => t.neighbors.count(_ != null) == 2)
            .map(_.ts.id.toLong)
        ).toSet

        corner_ids.product.toString
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
}
