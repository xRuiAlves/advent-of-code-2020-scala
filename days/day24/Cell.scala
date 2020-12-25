package days.day24

class Cell(val x: Int, val y: Int) {
    val value: (Int, Int) = (x, y)

    def +(that: Cell) = new Cell(this.x + that.x, this.y + that.y)
}
