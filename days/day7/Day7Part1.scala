package days.day7

import utils.{DaySolution, FileReader}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day7Part1 extends DaySolution(7, 1) {
    final val LINE_REGEX = "^(\\w+ \\w+) bags contain ((?:\\d+ \\w+ \\w+ \\w+(?:,\\s|\\.))*|no other bags\\.)$".r
    final val SHINY = "shiny gold"

    override def calculate: String = {
        val graph = new mutable.HashMap[String, ArrayBuffer[String]]()

        input.foreach(line => {
            val results = LINE_REGEX.findAllMatchIn(line).toList.head
            val parent = results.group(1)
            val children = parseChildren(results.group(2))
            children.map(child => child._1).foreach(child => {
                if (!graph.contains(child)) graph(child) = new ArrayBuffer[String]()
                graph(child).addOne(parent)
            })
        })

        val visited = new mutable.HashSet[String]()
        dfs(graph, SHINY, visited)

        (visited.size - 1).toString
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

    def dfs(graph: mutable.HashMap[String, ArrayBuffer[String]], node: String, visited: mutable.HashSet[String]): Unit = {
        visited.add(node)
        if (!graph.contains(node)) return

        graph(node).foreach(child => dfs(graph, child, visited))
    }
}