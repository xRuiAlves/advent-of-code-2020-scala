package days.day20

import utils.MatrixOps.{ rotateMatrix, flipMatrix }

class TileSet(val raw_tile: String) {
    private val lines = raw_tile.split("\n")
    val id: Int = lines.head.substring(5, 9).toInt
    val img: Array[Array[Char]] = lines.drop(1).map(_.toCharArray)

    val tiles: Array[Tile] = Array(
        new Tile(img, this),
        new Tile(rotateMatrix(img), this),
        new Tile(rotateMatrix(rotateMatrix(img)), this),
        new Tile(rotateMatrix(rotateMatrix(rotateMatrix(img))), this),
        new Tile(flipMatrix(img), this),
        new Tile(rotateMatrix(flipMatrix(img)), this),
        new Tile(rotateMatrix(rotateMatrix(flipMatrix(img))), this),
        new Tile(rotateMatrix(rotateMatrix(rotateMatrix(flipMatrix(img)))), this)
    )
    for (i <- tiles.indices) tiles(i).id = i
}
