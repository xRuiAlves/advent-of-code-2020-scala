package day4

import utils.FileReader

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day4Part1 {
    final val REQUIRED_FIELDS = Seq("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

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
        for (field <- REQUIRED_FIELDS) {
            if (!passport.contains(field)) {
                return false
            }
        }
        true
    }
}