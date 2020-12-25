package days.day18

import utils.{DaySolution, FileReader}

import scala.collection.mutable

object Day18Part2 extends DaySolution(18, 2) {
    final val NO_OP = ' '

    override def calculate: String = input
        .map(_.replaceAll("\\s", ""))
        .map(expression => eval(expression))
        .sum
        .toString

    def eval(expression: String): Long = eval(expression, 0, expression.length)

    def eval(expression: String, i0: Int, i1: Int): Long = {
        val partials = new mutable.Stack[Long]()
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
                applyOp(curr, curr_op, partials)
                curr_op = expression(idx)
            }
            idx += 1
        }
        applyOp(curr, curr_op, partials)
        partials.product
    }

    def applyOp(num: Long, op: Char, partials: mutable.Stack[Long]): Unit = op match {
        case '+' => partials.push(partials.pop() + num)
        case _ => partials.push(num)
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
}
