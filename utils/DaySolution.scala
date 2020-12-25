package utils

abstract class DaySolution(val day: Int, val part: Int = 1) {
    def calculate: String
    override def toString: String = s"Day $day, part $part: $calculate"
    def input: Array[String] = FileReader.readFile(s"Advent-Of-Code-2020/inputs/day$day.in").toArray
}
