package day14

import utils.{DaySolution, FileReader}

import scala.collection.mutable

object Day14Part2 extends DaySolution(14, 2) {
    final val MEM_ACCESS_REGEX = "^mem\\[(\\d+)\\]\\s=\\s(\\d+)$".r
    final val MASK_SIZE = 36

    override def calculate: String = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day14/input.txt").toArray
        val memory = new mutable.HashMap[Long, Long]()
        var mask: String = null

        lines.foreach(line => line.substring(0, 4) match {
            case "mask" => {
                mask = line.split(" = ")(1).reverse
            }
            case _ => {
                val results = MEM_ACCESS_REGEX.findAllMatchIn(line).toList.head
                val raw_position = results.group(1).toLong
                val value = results.group(2).toLong

                findMemPositions(raw_position, mask).foreach(position => memory(position) = value)
            }
        })

        memory.values.sum.toString
    }

    def findMemPositions(address: Long, mask: String): Array[Long] = {
        val num_combinations = mask.count(_ == 'X')
        val mem_positions = new Array[Long](math.pow(2, num_combinations).toInt)

        val crossed = crossValueMask(address, mask)

        for (i <- mem_positions.indices) {
            val combination_bits = valueToBitString(i)
            var curr_combination_bit = 0

            val mem_position = new StringBuilder()

            for (bit <- crossed) bit match {
                case 'X' => {
                    mem_position.append(combination_bits(curr_combination_bit))
                    curr_combination_bit += 1
                }
                case any => mem_position.append(any)
            }

            mem_positions(i) = bitStringToValue(mem_position.toString)
        }

        mem_positions
    }

    def crossValueMask(value: Long, mask: String): String = {
        val value_str = valueToBitString(value)
        val sb = new StringBuilder()

        for (i <- 0 until math.max(value_str.length, mask.length)) mask(i) match {
            case '0' => sb.append(value_str(i))
            case any => sb.append(any)
        }

        sb.toString
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
