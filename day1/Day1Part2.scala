package day1

import utils.FileReader

import scala.collection.mutable

object Day1Part2 {
    final val YEAR = 2020

    def main(args: Array[String]): Unit = {
        val nums: Array[Int] = FileReader.readFile("Advent-Of-Code-2020/day1/input.txt").map(_.toInt).toArray.sortWith(_<_)

        def findTrio: (Int, Int, Int) = {
            for (i <- nums.indices) {
                var l = i + 1
                var r = nums.length - 1

                while (l < r) {
                    val sum = nums(i) + nums(l) + nums(r)
                    if (sum == YEAR) return (nums(i), nums(l), nums(r))
                    if (sum > YEAR) r -= 1
                    else if (sum < YEAR) l += 1
                }
            }
            null
        }

        val trio = findTrio
        println(trio._1 * trio._2 * trio._3)
    }
}
