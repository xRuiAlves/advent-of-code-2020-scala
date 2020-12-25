package days.day19

class Rule(val raw_rule: String) {
    final val RULE_MATCH_REGEX = "^(\\d+):\\s(.*)$".r

    private val regex_match = RULE_MATCH_REGEX.findAllMatchIn(raw_rule).toArray.last
    val id: Int = regex_match.group(1).toInt
    val rule_description: String = regex_match.group(2)

    val is_compound: Boolean = rule_description(0) != '"'
    val char: Char = if (is_compound) '_' else rule_description(1)

    val child_rules: Array[Array[Int]] = is_compound match {
        case false => null
        case true => rule_description
            .split("\\|")
            .map(_
                .trim()
                .split(" ")
                .map(_.toInt)
            )
    }
}
