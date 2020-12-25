package days.day16

import utils.{DaySolution, FileReader}

object Day16Part1 extends DaySolution(16, 1) {
    final val RULE_REGEX = "^([\\w\\s]+):\\s(\\d+)-(\\d+)\\sor\\s(\\d+)-(\\d+)$".r

    override def calculate: String = {
        val groups = input.mkString("\n").split("\n\n")

        val rules = parseTicketRules(groups(0))
        val tickets = parseTickets(groups(2))

        val error_sum = tickets.map(ticket => {
            ticket.filterNot(number => matchesSomeRule(number, rules)).sum
        }).sum

        error_sum.toString
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
