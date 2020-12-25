package days.day20

object Monster {
    private final val MONSTER = Array(
        "                  # ".toCharArray,
        "#    ##    ##    ###".toCharArray,
        " #  #  #  #  #  #   ".toCharArray
    )
    final val BODY = '#'
    final val num_body_cells = MONSTER
        .map(line => line.count(_ == BODY))
        .sum
    final val height = MONSTER.length
    final val width = MONSTER.head.length

    def isMonster(puzzle: Array[Array[Char]], row: Int, col: Int): Boolean = {
        for (i <- MONSTER.indices; j <- MONSTER(i).indices) if (MONSTER(i)(j) == BODY) {
            if (puzzle(row + i)(col + j) != BODY) return false
        }
        true
    }
}
