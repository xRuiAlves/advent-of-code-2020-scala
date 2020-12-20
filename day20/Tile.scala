package day20

class Tile(val raw_tile: String) {
    private val lines = raw_tile.split("\n")
    val id: Int = lines.head.substring(5, 9).toInt
    val img: Array[Array[Char]] = lines.drop(1).map(_.toCharArray)

    val borders: Array[String] = Array(
        img.head.mkString(""),
        img.last.mkString(""),
        img.map(_.head).mkString(""),
        img.map(_.last).mkString("")
    )
    val reversed_borders: Array[String] = borders.map(_.reverse)
    val border_combinations: Array[String] = borders ++ reversed_borders

    override def toString: String = {
        val sb = new StringBuilder()
        sb.append(s"ID: $id\n")
        img.foreach(line => {
            line.foreach(cell => sb.append(cell))
            sb.append("\n")
        })
        sb.toString()
    }
}
