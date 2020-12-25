package day1

import utils.{DaySolution, FileReader}

object Day1Part2 extends DaySolution(1, 2) {
    final val YEAR = 2020

    override def calculate: String = {
        val nums: Array[Int] = FileReader.readFile(s"Advent-Of-Code-2020/day$day/input.txt").map(_.toInt).toArray.sortWith(_<_)

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
        (trio._1 * trio._2 * trio._3).toString
    }
}
