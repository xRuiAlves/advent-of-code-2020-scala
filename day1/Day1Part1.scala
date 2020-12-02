package day1

import utils.FileReader

import scala.collection.mutable

object Day1Part1 {
    final val YEAR = 2020

    def main(args: Array[String]): Unit = {
        val nums = FileReader.readFile("Advent-Of-Code-2020/day1/input.txt").map(_.toInt)
        val visited = new mutable.HashSet[Int]()

        def findPair: (Int, Int) = {
            for (num <- nums) {
                val complement = YEAR - num
                if (visited.contains(YEAR - num)) return (num, complement)
                visited.add(num)
            }
            null
        }

        val pair = findPair
        println(pair._1 * pair._2)
    }
}
