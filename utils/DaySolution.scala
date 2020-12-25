package utils

abstract class DaySolution(val day: Int, val part: Int = 1) {
    def calculate: String
    override def toString: String = s"Day $day, part $part: $calculate"
}
