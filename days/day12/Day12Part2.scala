package days.day12

import utils.{DaySolution, FileReader}

object Day12Part2 extends DaySolution(12, 2) {
    final val DIRECTIONS = Map(
        'E' -> (1, 0),
        'W' -> (-1, 0),
        'N' -> (0, 1),
        'S' -> (0, -1)
    )

    override def calculate: String = {
        val instructions = input.map(line => (line(0), line.substring(1).toInt))

        var x = 0
        var y = 0
        var rel_x = 10
        var rel_y = 1

        instructions.foreach { case (op, value) => {
                if (op == 'E' || op == 'W' || op == 'N' || op == 'S') {
                    rel_x += value * DIRECTIONS(op)._1
                    rel_y += value * DIRECTIONS(op)._2
                } else if (op == 'L' || op == 'R') {
                    val new_relative_point = rotateRelativePoint(op, value, rel_x, rel_y)
                    rel_x = new_relative_point._1
                    rel_y = new_relative_point._2
                } else {
                    x += value * rel_x
                    y += value * rel_y
                }
            }
        }

        val dist = math.abs(x) + math.abs(y)
        dist.toString
    }

    def rotateRelativePoint(way: Char, amount: Int, x: Int, y: Int): (Int, Int) = {
        val angle = amount * (if (way == 'L') 1 else -1) * (math.Pi / 180)
        (
            math.round(x * math.cos(angle) - y * math.sin(angle)).toInt,
            math.round(x * math.sin(angle) + y * math.cos(angle)).toInt
        )
    }
}
