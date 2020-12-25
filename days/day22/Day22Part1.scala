package days.day22

import utils.{DaySolution, FileReader}

import scala.collection.mutable

object Day22Part1 extends DaySolution(22, 1) {
    override def calculate: String = {
        val raw_hands = input.mkString("\n").split("\n\n")

        val h1 = parseHand(raw_hands(0))
        val h2 = parseHand(raw_hands(1))
        playGame(h1, h2)

        val score = if (h1.nonEmpty) calcScore(h1) else calcScore(h2)
        score.toString
    }

    def playGame(h1: mutable.Queue[Int], h2: mutable.Queue[Int]): Boolean = {
        while (h1.nonEmpty && h2.nonEmpty) {
            val c1 = h1.dequeue()
            val c2 = h2.dequeue()

            if (c1 > c2) {
                h1.enqueue(c1)
                h1.enqueue(c2)
            } else if (c2 > c1) {
                h2.enqueue(c2)
                h2.enqueue(c1)
            } else {
                throw new Exception("Repeated card")
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
