package day19

import utils.FileReader

import scala.collection.mutable

object Day19Part1 {
    type MatchRes = (Boolean, Int)

    def main(args: Array[String]): Unit = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day19/input.txt").toArray

        val groups = lines.mkString("\n").split("\n\n")
        val rules_lines = groups(0).split("\n")
        val msg_lines = groups(1).split("\n")

        val rules = rules_lines.map(line => {
            val rule = new Rule(line)
            rule.id -> rule
        }).to(mutable.HashMap)

        val valid_count = msg_lines.count(msg => {
            val match_res = getMatch(rules, rules(0), msg, 0)
            match_res._1 && match_res._2 == msg.length
        })

        println(valid_count)
    }

    def getMatch(rules: mutable.HashMap[Int, Rule], rule: Rule, msg: String, msg_idx: Int): MatchRes = {
        rule.is_compound match {
            case false => (msg_idx < msg.length && msg(msg_idx) == rule.char, msg_idx + 1)
            case true => {
                for (i <- rule.child_rules.indices) {
                    val match_res = getMatch(rules, rule, i, msg, msg_idx)
                    if (match_res._1) return match_res
                }
                (false, -1)
            }
        }
    }

    def getMatch(rules: mutable.HashMap[Int, Rule], rule: Rule, child_rule_idx: Int, str: String, str_idx: Int): MatchRes = {
        var curr_idx = str_idx

        for (rule_id <- rule.child_rules(child_rule_idx)) {
            val match_res = getMatch(rules, rules(rule_id), str, curr_idx)
            if (!match_res._1) return (false, -1)
            curr_idx = match_res._2
        }
        (true, curr_idx)
    }
}
