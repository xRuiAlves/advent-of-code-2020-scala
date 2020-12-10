package day10

import utils.FileReader

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day10Part2 {
    val cache = new mutable.HashMap[Int, Long]()

    def main(args: Array[String]): Unit = {
        val nums = FileReader.readFile("Advent-Of-Code-2020/day10/input.txt").map(_.toLong).toArray
        val joltages = new ArrayBuffer[Long]()
        joltages.addOne(0)
        joltages.addAll(nums.sortWith(_ < _))
        joltages.addOne(joltages.last + 3)

        cache(joltages.length - 1) = 1
        val res = dfs(joltages, 0)
        println(res)
    }

    def dfs(joltages: ArrayBuffer[Long], i: Int): Long = {
        if (cache.contains(i)) return cache(i)

        cache(i) = (1 to 3).map(j => {
            if ((i + j < joltages.length) && (joltages(i + j) <= joltages(i) + 3)) dfs(joltages, i + j)
            else 0
        }).sum

        cache(i)
    }
}
