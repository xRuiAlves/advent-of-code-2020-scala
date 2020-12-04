package day4
import utils.FileReader

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day4Part2 {
    final val REQUIRED_FIELDS = Seq("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

    final val FOUR_DIGIT_REGEX = "^\\d{4}$"
    final val BYR_BOUNDS = (1920, 2002)
    final val IYR_BOUNDS = (2010, 2020)
    final val EYR_BOUNDS = (2020, 2030)
    final val HGT_REGEX = "^\\d+(cm|in)$"
    final val HGT_CM_BOUNDS = (150, 193)
    final val HGT_IN_BOUNDS = (59, 76)
    final val HCL_REGEX = "^#[0-9a-f]{6}$"
    final val VALID_ECL = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    final val PID_REGEX = "^\\d{9}$"


    def main(args: Array[String]): Unit = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day4/input.txt").toList
        val passports = new ArrayBuffer[mutable.HashMap[String, String]]()
        passports.addOne(new mutable.HashMap[String, String]())

        lines.foreach(line => {
            if (line.isEmpty) passports.addOne(new mutable.HashMap[String, String]())
            else {
                val tokens = line.split(" ")
                tokens.foreach(token => {
                    val pair = token.split(":")
                    passports.last(pair(0)) = pair(1)
                })
            }
        })

        val num_valid = passports.count(passport => isPassportValid(passport))
        println(num_valid)
    }

    def isPassportValid(passport: mutable.HashMap[String, String]): Boolean = {
        if (!hasAllFields(passport)) return false

        val byr = passport("byr")
        if (!(byr.matches(FOUR_DIGIT_REGEX) && byr.toInt >= BYR_BOUNDS._1 && byr.toInt <= BYR_BOUNDS._2)) return false

        val iyr = passport("iyr")
        if (!(iyr.matches(FOUR_DIGIT_REGEX) && iyr.toInt >= IYR_BOUNDS._1 && iyr.toInt <= IYR_BOUNDS._2)) return false

        val eyr = passport("eyr")
        if (!(eyr.matches(FOUR_DIGIT_REGEX) && eyr.toInt >= EYR_BOUNDS._1 && eyr.toInt <= EYR_BOUNDS._2)) return false

        val hgt = passport("hgt")
        if (!hgt.matches(HGT_REGEX)) return false
        val hgt_value = hgt.substring(0, hgt.length - 2).toInt
        val hgt_type = hgt.substring(hgt.length - 2)
        if (hgt_type == "cm" && !(hgt_value >= HGT_CM_BOUNDS._1 && hgt_value <= HGT_CM_BOUNDS._2)) return false
        else if (hgt_type == "in" && !(hgt_value >= HGT_IN_BOUNDS._1 && hgt_value <= HGT_IN_BOUNDS._2)) return false

        if (!passport("hcl").matches(HCL_REGEX)) return false

        if (!VALID_ECL.contains(passport("ecl"))) return false

        if (!passport("pid").matches(PID_REGEX)) return false

        true
    }

    def hasAllFields(passport: mutable.HashMap[String, String]): Boolean = {
        for (field <- REQUIRED_FIELDS) {
            if (!passport.contains(field)) {
                return false
            }
        }
        true
    }
}