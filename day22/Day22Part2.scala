package day22

import utils.{DaySolution, FileReader}

import scala.collection.mutable

object Day22Part2 extends DaySolution(22, 2) {
    type Round = (String, String)

    override def calculate: String = {
        val lines = FileReader.readFile("Advent-Of-Code-2020/day22/input.txt").toArray
        val raw_hands = lines.mkString("\n").split("\n\n")

        val h1 = parseHand(raw_hands(0))
        val h2 = parseHand(raw_hands(1))
        playGame(h1, h2)

        val score = if (h1.nonEmpty) calcScore(h1) else calcScore(h2)
        score.toString
    }

    def playGame(h1: mutable.Queue[Int], h2: mutable.Queue[Int]): Boolean = {
        val round_history = new mutable.HashSet[Round]()

        while (h1.nonEmpty && h2.nonEmpty) {
            val round = (h1.mkString(""), h2.mkString(""))
            var player1_wins = true

            if (round_history.contains(round)) {
                return player1_wins
            }
            round_history.add(round)

            val c1 = h1.dequeue()
            val c2 = h2.dequeue()

            if (h1.length >= c1 && h2.length >= c2) {
                player1_wins = playGame(
                    h1.slice(0, c1).clone(),
                    h2.slice(0, c2).clone()
                )
            } else {
                player1_wins = c1 > c2
            }

            if (player1_wins) {
                h1.enqueue(c1)
                h1.enqueue(c2)
            } else {
                h2.enqueue(c2)
                h2.enqueue(c1)
            }
        }
        h1.nonEmpty
    }

    def calcScore(hand: mutable.Queue[Int]): Int = hand
        .reverse
        .zipWithIndex
        .map{ case(card, idx) => card * (idx + 1) }
        .sum

    def parseHand(raw_hand: String): mutable.Queue[Int] = raw_hand
        .split("\n")
        .drop(1)
        .map(_.toInt)
        .to(mutable.Queue)
}
