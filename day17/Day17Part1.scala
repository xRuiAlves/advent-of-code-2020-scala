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
        val cube_side = N * NUM_CYCLES
        var cube_space = Array.ofDim[Char](cube_side, cube_side, cube_side)
        var cube_space_swap = Array.ofDim[Char](cube_side, cube_side, cube_side)

        for (i <- 0 until N; j <- 0 until N) {
            val x = i + cube_side / 2
            val y = j + cube_side / 2
            cube_space(x)(y)(cube_side / 2) = lines(i)(j)
            cube_space_swap(x)(y)(cube_side / 2) = lines(i)(j)
        }

        for (_ <- 0 until NUM_CYCLES) {
            applyCycle(cube_space, cube_space_swap)

            val temp = cube_space
            cube_space = cube_space_swap
            cube_space_swap = temp
        }

        println(countActiveCubes(cube_space))
    }

    def applyCycle(cube_space: Array3D, cube_space_swap: Array3D): Unit = {
        for (i <- cube_space.indices; j <- cube_space(i).indices; k <- cube_space(i)(j).indices) {
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
