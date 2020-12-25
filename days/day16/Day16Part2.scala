package days.day16

import utils.{DaySolution, FileReader}

import scala.collection.mutable

object Day16Part2 extends DaySolution(16, 2) {
    final val RULE_REGEX = "^([\\w\\s]+):\\s(\\d+)-(\\d+)\\sor\\s(\\d+)-(\\d+)$".r

    override def calculate: String = {
        val groups = input.mkString("\n").split("\n\n")

        val rules = parseTicketRules(groups(0))
        val my_ticket = parseTickets(groups(1)).last
        val other_valid_tickets = parseTickets(groups(2))
            .filter(ticket => isTicketValid(ticket, rules))

        val possibilities = rules.map(_ => {
            rules.indices.to(mutable.HashSet)
        })

        other_valid_tickets.foreach(numbers => {
            for (num_index <- numbers.indices) {
                for (rule_index <- rules.indices) {
                    if (!rules(rule_index).matches(numbers(num_index))) {
                        possibilities(num_index).remove(rule_index)
                    }
                }
            }
        })

        val index_rule_map = new mutable.HashMap[Int, Int]()
        val used = new mutable.HashSet[Int]()

        possibilities
            .zipWithIndex
            .sortWith(_._1.size < _._1.size)
            .foreach { case (possibility_set, index) => for (possibility <- possibility_set) {
                if (!used.contains(possibility)) {
                    used.add(possibility)
                    index_rule_map(index) = possibility
                }
            }
        }

        val prod = index_rule_map
            .filter(kv => rules(kv._2).id.contains("departure"))
            .keys
            .map(idx => my_ticket(idx).toLong)
            .product
        
        prod.toString
    }

    def isTicketValid(ticket: Array[Int], rules: Array[TicketRule]): Boolean = {
        ticket.forall(number => matchesSomeRule(number, rules))
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
