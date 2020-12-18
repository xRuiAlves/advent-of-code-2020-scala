package day17

import utils.FileReader

object Day17Part1 {
    type Array3D = Array[Array[Array[Char]]]

    final val ACTIVE = '#'
    final val INACTIVE = '.'
    final val OUT_OF_BOUNDS = '_'
    final val NUM_CYCLES = 6

    def main(args: Array[String]): Unit = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day17/input.txt").toArray
        val N = lines.last.length
        val cube_side = N + 2*NUM_CYCLES
        var cube_space = Array.ofDim[Char](cube_side, cube_side, cube_side)
        var cube_space_swap = Array.ofDim[Char](cube_side, cube_side, cube_side)

        for (i <- 0 until N; j <- 0 until N) {
            val x = i + NUM_CYCLES
            val y = j + NUM_CYCLES
            cube_space(x)(y)(NUM_CYCLES) = lines(i)(j)
            cube_space_swap(x)(y)(NUM_CYCLES) = lines(i)(j)
        }

        for (cycle <- 1 to NUM_CYCLES) {
            for (
                i <- (NUM_CYCLES - cycle) until (NUM_CYCLES + N + cycle);
                j <- (NUM_CYCLES - cycle) until (NUM_CYCLES + N + cycle);
                k <- (NUM_CYCLES - cycle) until (NUM_CYCLES + N + cycle)
            ) {
                val active_neighbors = countActiveNeighbors(cube_space, i, j, k)
                cube_space(i)(j)(k) match {
                    case ACTIVE => cube_space_swap(i)(j)(k) =
                        if (active_neighbors == 2 || active_neighbors == 3) ACTIVE
                        else INACTIVE
                    case _ => cube_space_swap(i)(j)(k) =
                        if (active_neighbors == 3) ACTIVE
                        else INACTIVE
                }
            }

            val temp = cube_space
            cube_space = cube_space_swap
            cube_space_swap = temp
        }

        println(countActiveCubes(cube_space))
    }

    def countActiveNeighbors(cube_space: Array3D, x: Int, y: Int, z: Int): Int = {
        var count = 0
        for (i <- (x - 1) to (x + 1); j <- (y - 1) to (y + 1); k <- (z - 1) to (z + 1)) {
            if ((i, j, k) != (x, y, z) && getValue(cube_space, i, j, k) == ACTIVE) count += 1
        }
        count
    }

    def countActiveCubes(cube_space: Array3D): Int = {
        var count = 0
        for (i <- cube_space.indices; j <- cube_space(i).indices; k <- cube_space(i)(j).indices) {
            if (cube_space(i)(j)(k) == ACTIVE) count += 1
        }
        count
    }

    def getValue(cube_space: Array3D, i: Int, j: Int, k: Int): Int = {
        if (i >= 0 && j >= 0 && k >= 0 && i < cube_space.length && j < cube_space(i).length && k < cube_space(i)(j).length) cube_space(i)(j)(k)
        else OUT_OF_BOUNDS
    }
}
