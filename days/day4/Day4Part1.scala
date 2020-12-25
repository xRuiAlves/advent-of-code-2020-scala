package days.day4

import utils.{DaySolution, FileReader}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day4Part1 extends DaySolution(4, 1) {
    final val REQUIRED_FIELDS = Seq("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

    override def calculate: String = {
        val passports = new ArrayBuffer[mutable.HashMap[String, String]]()
        passports.addOne(new mutable.HashMap[String, String]())

        input.foreach(line => {
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
        num_valid.toString
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