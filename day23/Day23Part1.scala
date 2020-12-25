package day23

import utils.{DaySolution, FileReader}

import scala.collection.mutable

object Day23Part1 extends DaySolution(23, 1) {
    var list: ListNode = null
    final val NUM_NODES = 9
    final val NUM_MOVES = 100
    final val nodes_map = new Array[ListNode](NUM_NODES + 1)

    override def calculate: String = {
        val nums = FileReader
            .readFile("Advent-Of-Code-2020/day23/input.txt")
            .toArray
            .head
            .split("")
            .map(_.toInt)

        list = buildList(nums)
        var curr = list

        for (_ <- 0 until NUM_MOVES) {
            curr = move(curr)
        }

        buildLabelString()
    }

    def move(curr: ListNode): ListNode = {
        val trio_head = curr.next
        val removed = new mutable.HashSet[Int]()

        var after_trio_old = trio_head
        for (_ <- 0 until 3) {
            removed.add(after_trio_old.value)
            after_trio_old = after_trio_old.next
        }
        connectNodes(curr, after_trio_old)

        var insertion_id = getPrevId(curr.value)
        while (removed.contains(insertion_id)) {
            insertion_id = getPrevId(insertion_id)
        }

        val insertion_node = nodes_map(insertion_id)
        val after_trio_new = insertion_node.next
        connectNodes(insertion_node, trio_head)
        connectNodes(insertion_node.next.next.next, after_trio_new)

        curr.next
    }

    def getPrevId(id: Int): Int = (id - 2 + NUM_NODES) % NUM_NODES + 1

    def buildList(nums: Array[Int]): ListNode = {
        val head = new ListNode(nums.head)
        nodes_map(head.value) = head
        var curr = head

        nums.drop(1).foreach(num => {
            val new_node = new ListNode(num)
            nodes_map(new_node.value) = new_node
            connectNodes(curr, new_node)
            curr = new_node
        })

        connectNodes(curr, head)
        head
    }

    def connectNodes(n1: ListNode, n2: ListNode): Unit = {
        n1.next = n2
        n2.prev = n1
    }

    def buildLabelString(): String = {
        var curr = nodes_map(1).next
        val sb = new StringBuilder()
        for (_ <- 0 until NUM_NODES - 1) {
            sb.append(curr.value)
            curr = curr.next
        }
        sb.toString()
    }
}
