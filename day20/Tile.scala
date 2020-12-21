package day20

import scala.collection.mutable

class Tile(val img: Array[Array[Char]], val ts: TileSet) {

    val bitmap: Array[Int] = img.map(arr => arrayToBitMask(arr))

    val top: Int = bitmap.head
    val bottom: Int = bitmap.last
    val left: Int = arrayToBitMask(img.map(_.head))
    val right: Int = arrayToBitMask(img.map(_.last))

    val borders: Set[Int] = Set(top, bottom, left, right)

    def arrayToBitMask(arr: Array[Char]): Int = {
        var bit_mask = 0
        arr.reverse.foreach {
            case '#' => bit_mask = (bit_mask << 1) | 1
            case '.' => bit_mask = (bit_mask << 1)
        }
        bit_mask
    }

    override def toString: String = s"${ts.id}"
}
