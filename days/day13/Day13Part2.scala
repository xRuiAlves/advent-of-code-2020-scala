package days.day13

import utils.{DaySolution, FileReader}

object Day13Part2 extends DaySolution(13, 2) {
    override def calculate: String = {
        val pairs = input
            .last
            .split(",")
            .zipWithIndex
            .filter(pair => pair._1 != "x")
            .map(pair => (pair._1.toLong, pair._1.toLong - pair._2))

        val nums = pairs.map(_._1)
        val remainders = pairs.map(_._2)

        val timestamp = crt(nums, remainders)
        timestamp.toString
    }

    def crt(nums: Array[Long], remainders: Array[Long]): Long = {
        val prod = nums.product
        var res = 0L

        for (i <- nums.indices) {
            val sub_prod = prod / nums(i)
            res += remainders(i) * inverseModulo(sub_prod, nums(i)) * sub_prod
        }

        res % prod
    }

    def inverseModulo(a0: Long, m0: Long): Long = {
        var a = a0
        var m = m0
        var x0 = 0L
        var x1 = 1L

        if (m == 1) return 0

        while (a > 1) {
            val q = a / m
            var t = m

            m = a % m
            a = t
            t = x0
            x0 = x1 - q * x0
            x1 = t
        }

        if (x1 < 0) x1 + m0 else x1
    }
}
