package days.day16

class TicketRule(
    val id: String,
    val from1: Int,
    val to1: Int,
    val from2: Int,
    val to2: Int
) {
    def matches(number: Int): Boolean = {
        (number >= from1 && number <= to1) ||
        (number >= from2 && number <= to2)
    }

    override def toString: String = s"$id: $from1-$to1 or $from2-$to2"
}
