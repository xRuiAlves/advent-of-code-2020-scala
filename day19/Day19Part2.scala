package day19

import utils.{DaySolution, FileReader}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day19Part2 extends DaySolution(19, 2) {
    final val RULES_TO_UPDATE = Map(
        8 -> "8: 42 | 42 8",
        11 -> "11: 42 31 | 42 11 31"
    )

    override def calculate: String = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day19/input.txt").toArray

        val groups = lines.mkString("\n").split("\n\n")
        val rules_lines = groups(0).split("\n")
        val msg_lines = groups(1).split("\n")

        val rules = rules_lines.map(line => {
            val rule = new Rule(line)
            rule.id -> rule
        }).to(mutable.HashMap)

        RULES_TO_UPDATE.foreach(kv => rules(kv._1) = new Rule(kv._2))
        val valid_count = msg_lines.count(msg => getMatches(rules, rules(0), msg, 0).contains(msg.length))

        valid_count.toString
    }

    def getMatches(rules: mutable.HashMap[Int, Rule], rule: Rule, msg: String, msg_idx: Int): ArrayBuffer[Int] = {
        val indexes = new ArrayBuffer[Int]()

        rule.is_compound match {
            case false => if (msg_idx < msg.length && msg(msg_idx) == rule.char) indexes.addOne(msg_idx + 1)
            case true => for (i <- rule.child_rules.indices) {
                indexes.addAll(getMatches(rules, rule, i, msg, msg_idx))
            }
        }

        indexes
    }

    def getMatches(rules: mutable.HashMap[Int, Rule], rule: Rule, child_rule_idx: Int, str: String, str_idx: Int): ArrayBuffer[Int] = {
        var curr_indexes = new ArrayBuffer[Int]()
        curr_indexes.addOne(str_idx)

        for (rule_id <- rule.child_rules(child_rule_idx)) {
            val new_indexes = new ArrayBuffer[Int]()
            curr_indexes.foreach(new_str_idx => {
                new_indexes.addAll(getMatches(rules, rules(rule_id), str, new_str_idx))
            })
            curr_indexes = new_indexes
        }

        curr_indexes
    }
}
