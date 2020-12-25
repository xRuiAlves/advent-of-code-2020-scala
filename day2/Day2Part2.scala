package day2

import utils.{DaySolution, FileReader}

object Day2Part2 extends DaySolution(2, 2) {
    final val PASS_RULE_REGEX = "(\\d+)-(\\d+) (\\w): (\\w+)".r

    override def calculate: String = {
        val list = FileReader.readFile("Advent-Of-Code-2020/day2/input.txt")
        val num_valid = list.count(s => isValid(s))
        num_valid.toString
    }

    def isValid(pass_rule: String): Boolean = {
        val results = PASS_RULE_REGEX.findAllMatchIn(pass_rule).toList(0)
        val idx1 = results.group(1).toInt
        val idx2 = results.group(2).toInt
        val char = results.group(3)(0)
        val str = results.group(4)

        val occ1 = if (str(idx1 - 1).equals(char)) 1 else 0
        val occ2 = if (str(idx2 - 1).equals(char)) 1 else 0
        occ1 + occ2 == 1
    }
}
