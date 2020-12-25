package day1

import utils.{DaySolution, FileReader}

import scala.collection.mutable

object Day1Part1 extends DaySolution(1, 1) {
    final val YEAR = 2020

    override def calculate: String = {
        val nums = FileReader.readFile(s"Advent-Of-Code-2020/day$day/input.txt").map(_.toInt)
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
        (pair._1 * pair._2).toString
    }
}
