package day2

import utils.{DaySolution, FileReader}

object Day2Part1 extends DaySolution(2, 1) {
    final val PASS_RULE_REGEX = "(\\d+)-(\\d+) (\\w): (\\w+)".r

    override def calculate: String = {
        val list = FileReader.readFile("Advent-Of-Code-2020/day2/input.txt")
        val num_valid = list.count(s => isValid(s))
        num_valid.toString
    }

    def isValid(pass_rule: String): Boolean = {
        val results = PASS_RULE_REGEX.findAllMatchIn(pass_rule).toList(0)
        val lb = results.group(1).toInt
        val ub = results.group(2).toInt
        val char = results.group(3)(0)
        val str = results.group(4)
        val count = str.count(_.equals(char))
        count >= lb && count <= ub
    }
}
