package days.day13

import utils.{DaySolution, FileReader}

object Day13Part1 extends DaySolution(13, 1) {
    override def calculate: String = {
        val lines = input

        val min_time = lines(0).toInt
        val ids = lines(1).split(",").filter(_ != "x").map(_.toInt)

        val bus_info = findBus(min_time, ids)
        ((bus_info._1 - min_time) * bus_info._2).toString
    }

    def findBus(min_time: Int, ids: Array[Int]): (Int, Int) = {
        val max_time = min_time + ids.max
        for (t <- min_time to max_time) {
            for (id <- ids) {
                if (t % id == 0) return (t, id)
            }
        }

        throw new Exception("Solution not found")
    }
}
