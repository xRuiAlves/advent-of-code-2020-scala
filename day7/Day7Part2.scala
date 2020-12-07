package day7

import utils.FileReader

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day7Part2 {
    final val LINE_REGEX = "^(\\w+ \\w+) bags contain ((?:\\d+ \\w+ \\w+ \\w+(?:,\\s|\\.))*|no other bags\\.)$".r
    final val SHINY = "shiny gold"

    val cache = new mutable.HashMap[String, Int]()
    val graph = new mutable.HashMap[String, Array[(String, Int)]]()

    def main(args: Array[String]): Unit = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day7/input.txt").toList

        lines.foreach(line => {
            val results = LINE_REGEX.findAllMatchIn(line).toList.head
            val parent = results.group(1)
            val children = parseChildren(results.group(2))
            graph(parent) = children
        })

        val amount = getAmount(SHINY)
        println(amount - 1)
    }

    def parseChildren(s: String): Array[(String, Int)] = {
        if (s.equals("no other bags.")) Array()
        else s.split(", ").map(item => {
            val tokens = item.split(" ")
            val count = tokens(0).toInt
            val id = s"${tokens(1)} ${tokens(2)}"
            (id, count)
        })
    }

    def getAmount(bag: String): Int = {
        if (cache.contains(bag)) return cache(bag)

        if (!graph.contains(bag) || graph(bag).isEmpty) return 1

        val amount = graph(bag).map(entry => entry._2 * getAmount(entry._1)).sum + 1
        cache(bag) = amount
        amount
    }
}