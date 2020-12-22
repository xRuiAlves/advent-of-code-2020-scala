package day20

class Tile(val img: Array[Array[Char]], val ts: TileSet, var id: Int = -1) {

    val bitmap: Array[Int] = img.map(arr => arrayToBitMask(arr))

    val top: Int = bitmap.head
    val bottom: Int = bitmap.last
    val left: Int = arrayToBitMask(img.map(_.head))
    val right: Int = arrayToBitMask(img.map(_.last))

    var top_neighbor: Tile = null
    var bottom_neighbor: Tile = null
    var left_neighbor: Tile = null
    var right_neighbor: Tile = null

    def neighbors: Array[Tile] = Array(
        top_neighbor,
        bottom_neighbor,
        left_neighbor,
        right_neighbor
    )

    def arrayToBitMask(arr: Array[Char]): Int = {
        var bit_mask = 0
        arr.reverse.foreach {
            case '#' => bit_mask = (bit_mask << 1) | 1
            case '.' => bit_mask = (bit_mask << 1)
        }
        bit_mask
    }
}
