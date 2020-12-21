package day20

class TileSet(val raw_tile: String) {
    type Mat2D = Array[Array[Char]]
    private val lines = raw_tile.split("\n")
    val id: Int = lines.head.substring(5, 9).toInt
    val img: Mat2D = lines.drop(1).map(_.toCharArray)

    val tiles: Array[Tile] = Array(
        new Tile(img, this),
        new Tile(rotateImage(img), this),
        new Tile(rotateImage(rotateImage(img)), this),
        new Tile(rotateImage(rotateImage(rotateImage(img))), this),
        new Tile(flipImage(img), this),
        new Tile(rotateImage(flipImage(img)), this),
        new Tile(rotateImage(rotateImage(flipImage(img))), this),
        new Tile(rotateImage(rotateImage(rotateImage(flipImage(img)))), this)
    )

    def flipImage(img: Mat2D): Mat2D = {
        img.reverse
    }

    def rotateImage(img: Mat2D): Mat2D = {
        val rotated_img = Array.ofDim[Char](img.length, img.head.length)
        val num_levels = math.floor(img.length / 2).toInt

        for (level <- 0 until num_levels) {
            for (i <- 0 until (img.length - 2*level - 1)) {
                val x = level + i
                val rx = img.length - x - 1
                val rlevel = img.length - level - 1

                rotated_img(level)(x) = img(rx)(level)
                rotated_img(rx)(level) = img(rlevel)(rx)
                rotated_img(rlevel)(rx) = img(x)(rlevel)
                rotated_img(x)(rlevel) = img(level)(x)
            }
        }
        rotated_img
    }
}
