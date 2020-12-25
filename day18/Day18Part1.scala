package day18

import utils.{DaySolution, FileReader}

object Day18Part1 extends DaySolution(18, 1) {
    final val NO_OP = ' '

    override def calculate: String = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day18/input.txt").toArray
        val res = lines
            .map(_.replaceAll("\\s", ""))
            .map(expression => eval(expression))
            .sum

        res.toString
    }

    def eval(expression: String): Long = eval(expression, 0, expression.length)

    def eval(expression: String, i0: Int, i1: Int): Long = {
        var sum = 0L
        var curr = 0L

        var curr_op = NO_OP
        var idx = i0

        while (idx < i1) {
            if (expression(idx).isDigit) {
                curr = expression(idx) - '0'
            } else if (expression(idx) == '(') {
                val closing_parenthesis_idx = findClosingParenthesis(expression, idx)
                curr = eval(expression, idx + 1, closing_parenthesis_idx)
                idx = closing_parenthesis_idx
            } else {
                sum = applyOp(sum, curr_op, curr)
                curr_op = expression(idx)
            }
            idx += 1
        }
        applyOp(sum, curr_op, curr)
    }

    def findClosingParenthesis(expression: String, idx: Int): Int = {
        var p_count = 0
        for (i <- idx until expression.length) expression(i) match {
            case '(' => p_count += 1
            case ')' => {
                p_count -= 1
                if (p_count == 0) return i
            }
            case _ =>
        }

        throw new Exception(s"Bad expression:\n$expression")
    }

    def applyOp(n1: Long, op: Char, n2: Long): Long = op match {
        case NO_OP => n2
        case '+' => n1 + n2
        case '*' => n1 * n2
    }
}
