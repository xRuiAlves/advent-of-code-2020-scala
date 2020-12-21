package day20

import utils.FileReader

import scala.collection.mutable

object Day20Part1 {
    def main(args: Array[String]): Unit = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day20/input.txt").toArray
        val tile_sets = lines
            .mkString("\n")
            .split("\n\n")
            .map(raw_tile => new TileSet(raw_tile))

        val tile_neighbors = new mutable.HashMap[Tile, mutable.HashSet[Tile]]()
        def addNeighbor(t1: Tile, t2: Tile): Unit = {
            tile_neighbors.get(t1) match {
                case Some(neighbors) => neighbors.add(t2)
                case None => {
                    tile_neighbors(t1) = new mutable.HashSet[Tile]()
                    tile_neighbors(t1).add(t2)
                }
            }
            tile_neighbors.get(t2) match {
                case Some(neighbors) => neighbors.add(t1)
                case None => {
                    tile_neighbors(t2) = new mutable.HashSet[Tile]()
                    tile_neighbors(t2).add(t1)
                }
            }
        }

        for (i <- tile_sets.indices; j <- tile_sets.indices) if (i != j) {
            val ts1 = tile_sets(i)
            val ts2 = tile_sets(j)

            ts1.tiles.foreach(t1 => {
                ts2.tiles.foreach(t2 => {
                    t1.borders.foreach(border => if (t2.borders.contains(border)) {
                        addNeighbor(t1, t2)
                    })
                })
            })
        }

        val corner_ids = tile_neighbors.filter(kv => kv._2.size / 4 == 2).keys.map(_.ts.id.toLong)
        println(corner_ids.product)
    }
}
