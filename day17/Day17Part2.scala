package day17

import utils.FileReader

object Day17Part2 {
    type Array4D = Array[Array[Array[Array[Char]]]]

    final val ACTIVE = '#'
    final val INACTIVE = '.'
    final val OUT_OF_BOUNDS = '_'
    final val NUM_CYCLES = 6

    def main(args: Array[String]): Unit = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day17/input.txt").toArray
        val N = lines.last.length
        val hcube_side = N * NUM_CYCLES
        var hcube_space = Array.ofDim[Char](hcube_side, hcube_side, hcube_side, hcube_side)
        var hcube_space_swap = Array.ofDim[Char](hcube_side, hcube_side, hcube_side, hcube_side)

        for (i <- 0 until N; j <- 0 until N) {
            val x = i + hcube_side / 2
            val y = j + hcube_side / 2
            hcube_space(x)(y)(hcube_side / 2)(hcube_side / 2) = lines(i)(j)
            hcube_space_swap(x)(y)(hcube_side / 2)(hcube_side / 2) = lines(i)(j)
        }

        for (_ <- 0 until NUM_CYCLES) {
            applyCycle(hcube_space, hcube_space_swap)

            val temp = hcube_space
            hcube_space = hcube_space_swap
            hcube_space_swap = temp
        }

        println(countActiveCubes(hcube_space))
    }

    def applyCycle(hcube_space: Array4D, hcube_space_swap: Array4D): Unit = {
        for (i <- hcube_space.indices; j <- hcube_space(i).indices; k <- hcube_space(i)(j).indices; l <- hcube_space(i)(j)(k).indices) {
            val active_neighbors = countActiveNeighbors(hcube_space, i, j, k, l)
            hcube_space(i)(j)(k)(l) match {
                case ACTIVE => hcube_space_swap(i)(j)(k)(l) =
                    if (active_neighbors == 2 || active_neighbors == 3) ACTIVE
                    else INACTIVE
                case _ => hcube_space_swap(i)(j)(k)(l) =
                    if (active_neighbors == 3) ACTIVE
                    else INACTIVE
            }
        }
    }

    def countActiveNeighbors(hcube_space: Array4D, x: Int, y: Int, z: Int, w: Int): Int = {
        var count = 0
        for (i <- (x - 1) to (x + 1); j <- (y - 1) to (y + 1); k <- (z - 1) to (z + 1); l <- (w - 1) to (w + 1)) {
            if ((i, j, k, l) != (x, y, z, w) && getValue(hcube_space, i, j, k, l) == ACTIVE) count += 1
        }
        count
    }

    def countActiveCubes(hcube_space: Array4D): Int = {
        var count = 0
        for (i <- hcube_space.indices; j <- hcube_space(i).indices; k <- hcube_space(i)(j).indices; l <- hcube_space(i)(j)(k).indices) {
            if (hcube_space(i)(j)(k)(l) == ACTIVE) count += 1
        }
        count
    }

    def getValue(hcube_space: Array4D, i: Int, j: Int, k: Int, l: Int): Int = {
        if (
            i >= 0 && j >= 0 && k >= 0 && l >= 0 &&
            i < hcube_space.length && j < hcube_space(i).length && k < hcube_space(i)(j).length && l < hcube_space(i)(j)(k).length
        ) hcube_space(i)(j)(k)(l)
        else OUT_OF_BOUNDS
    }
}
