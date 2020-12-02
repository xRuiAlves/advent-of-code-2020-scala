package day2

import utils.FileReader

object Day2Part1 {
    final val PASS_RULE_REGEX = "(\\d+)-(\\d+) (\\w): (\\w+)".r

    def main(args: Array[String]): Unit = {
        val list = FileReader.readFile("Advent-Of-Code-2020/day2/input.txt")
        val num_valid = list.count(s => isValid(s))
        println(num_valid)
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
