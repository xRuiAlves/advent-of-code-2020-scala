package days.day6

import utils.{DaySolution, FileReader}

object Day6Part2 extends DaySolution(6, 2) {
    final val ALPHABET_START = 'a'

    override def calculate: String = {
        val lines = input.mkString("\n")
        val group_answers = lines.split("\n\n").map(group => group.split("\n"))
        var count = 0

        group_answers.foreach(group_answer => {
            var mask = Int.MaxValue
            group_answer.foreach(answer => {
                mask &= strToMask(answer)
            })
            count += countMaskOnes(mask)
        })

        count.toString
    }

    def strToMask(str: String): Int = {
        var mask = 0
        str.foreach(q => {
            mask |= (1 << (q - ALPHABET_START))
        })
        mask
    }

    def countMaskOnes(mask: Int): Int = {
        var curr_mask = mask
        var count = 0
        while (curr_mask != 0) {
            count += (curr_mask & 1)
            curr_mask >>= 1
        }
        count
    }
}