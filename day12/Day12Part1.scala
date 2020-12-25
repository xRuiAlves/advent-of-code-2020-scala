package day12

import utils.{DaySolution, FileReader}

object Day12Part1 extends DaySolution(12, 1) {
    final val DIRECTIONS = Map(
        'E' -> (1, 0),
        'W' -> (-1, 0),
        'N' -> (0, 1),
        'S' -> (0, -1)
    )

    override def calculate: String = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day12/input.txt").toArray
        val instructions = lines.map(line => (line(0), line.substring(1).toInt))

        var x = 0
        var y = 0
        var dir = 'E'

        instructions.foreach { case (op, value) => {
                if (op == 'E' || op == 'W' || op == 'N' || op == 'S') {
                    x += value * DIRECTIONS(op)._1
                    y += value * DIRECTIONS(op)._2
                } else if (op == 'L' || op == 'R') {
                    dir = turn(dir, op, value)
                } else {
                    x += value * DIRECTIONS(dir)._1
                    y += value * DIRECTIONS(dir)._2
                }
            }
        }

        val dist = math.abs(x) + math.abs(y)
        dist.toString
    }

    def turn(dir: Char, way: Char, amount: Int): Char = {
        if (amount % 90 != 0) {
            throw new Exception(f"Invalid turn amount: $amount")
        }

        var curr_dir = dir

        for (_ <- 0 until (amount/90)) {
            curr_dir = turn(curr_dir, way)
        }

        curr_dir
    }

    def turn(dir: Char, way: Char): Char = way match {
        case 'L' => dir match {
            case 'E' => 'N'
            case 'N' => 'W'
            case 'W' => 'S'
            case 'S' => 'E'
        }
        case 'R' => dir match {
            case 'N' => 'E'
            case 'E' => 'S'
            case 'S' => 'W'
            case 'W' => 'N'
        }
    }
}
