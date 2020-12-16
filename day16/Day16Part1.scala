package day16

import utils.FileReader

object Day16Part1 {
    final val RULE_REGEX = "^([\\w\\s]+):\\s(\\d+)-(\\d+)\\sor\\s(\\d+)-(\\d+)$".r

    def main(args: Array[String]): Unit = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day16/input.txt").toArray
        val groups = lines.mkString("\n").split("\n\n")

        val rules = parseTicketRules(groups(0))
        val tickets = parseTickets(groups(2))

        val error_sum = tickets.map(ticket => {
            ticket.filterNot(number => matchesSomeRule(number, rules)).sum
        }).sum

        println(error_sum)
    }

    def parseTickets(raw: String): Array[Array[Int]] = raw
        .split("\n")
        .drop(1)
        .map(_.split(",").map(_.toInt))


    def parseTicketRules(raw: String): Array[TicketRule] = raw.split("\n").map(line => {
        val results = RULE_REGEX.findAllMatchIn(line).toList.head
        new TicketRule(
            results.group(1),
            results.group(2).toInt,
            results.group(3).toInt,
            results.group(4).toInt,
            results.group(5).toInt
        )
    })

    def matchesSomeRule(number: Int, rules: Array[TicketRule]): Boolean = {
        rules.exists(rule => rule.matches(number))
    }
}
