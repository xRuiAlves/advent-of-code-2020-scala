package day25

import utils.{DaySolution, FileReader}

object Day25 extends DaySolution(25) {
    final val LOOP_DIVISOR = 20201227L
    final val DEFAULT_SUBJECT = 7L
    final val INIT_VALUE = 1L

    override def calculate: String = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day25/input.txt").toArray
        val pub_key1 = lines.head.toLong
        val pub_key2 = lines.last.toLong

        val l1 = findLoopSize(pub_key1)
        val l2 = findLoopSize(pub_key2)

        val v1 = loop(pub_key1, l2)
        val v2 = loop(pub_key2, l1)

        assert(v1 == v2)
        v1.toString
    }

    def transform(value: Long, subject: Long): Long = (value * subject) % LOOP_DIVISOR

    def findLoopSize(pub_key: Long): Int = {
        var value = INIT_VALUE
        var loop = 0

        while (value != pub_key) {
            value = transform(value, DEFAULT_SUBJECT)
            loop += 1
        }
        loop
    }

    def loop(subject: Long, num_loops: Int): Long = {
        var value = INIT_VALUE
        for (_ <- 0 until num_loops) value = transform(value, subject)
        value
    }
}
