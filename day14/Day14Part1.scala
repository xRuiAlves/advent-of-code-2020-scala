package day14

import utils.FileReader

import scala.collection.mutable

object Day14Part1 {
    final val MEM_ACCESS_REGEX = "^mem\\[(\\d+)\\]\\s=\\s(\\d+)$".r
    final val MASK_SIZE = 36

    def main(args: Array[String]): Unit = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day14/input.txt").toArray
        val memory = new mutable.HashMap[Long, Long]()
        var mask: String = null

        lines.foreach(line => line.substring(0, 4) match {
            case "mask" => {
                mask = line.split(" = ")(1).reverse
            }
            case _ => {
                val results = MEM_ACCESS_REGEX.findAllMatchIn(line).toList.head
                val position = results.group(1).toLong
                val value = results.group(2).toLong

                memory(position) = crossValueMask(value, mask)
            }
        })

        val sum = memory.values.sum
        println(sum)
    }

    def crossValueMask(value: Long, mask: String): Long = {
        val value_str = valueToBitString(value)
        val sb = new StringBuilder()

        for (i <- 0 until math.max(value_str.length, mask.length)) mask(i) match {
            case 'X' => sb.append(value_str(i))
            case bit => sb.append(bit)
        }

        bitStringToValue(sb.toString)
    }

    def valueToBitString(value: Long): String = {
        val sb = new StringBuilder()

        for (i <- 0 until MASK_SIZE) {
            sb.append((value & (1 << i)) >> i)
        }

        sb.toString
    }

    def bitStringToValue(bitString: String): Long = {
        var value = 0L

        for (i <- bitString.indices) {
            value |= ((bitString(i) - '0').toLong << i)
        }

        value
    }
}
