package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.collection.immutable.Queue
import scala.util.chaining.*

@scala.annotation.experimental
object MinimumCosttoMakeatLeastOneValidPathinaGrid1368 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def minCost(grid: Array[Array[Int]]): Int = {
      val rMax = 1 << 7
      val cMax = 1 << 7
      val rows = grid.size
      val cols = grid.head.size
      type Pos = Int
      extension (p: Pos) {
        def r: Int = p % rMax
        def c: Int = (p / rMax) % cMax
        def +(o: Pos) = (p.r + o.r, p.c + o.c)
        def isInBounds: Boolean = {
          0 <= r && r < rows &&
          0 <= c && c < cols
        }
        def manhattan(o: Pos): Int =
          Math.abs(p.r - o.r) + Math.abs(p.c - o.c)
        def move(d: Int): Pos = {
          p + Array(0, rMax, -rMax, 1, -1)(d)
        }
        def toPosString: String =
          s"($r, $c)"
      }
      extension (board: Array[Array[Int]]) {
        def at(p: Pos): Int = board(p.r)(p.c)
        def updateAt(pos: Pos, value: Int): Unit = board(pos.r)(pos.c) = value
      }

      val target = (rows - 1) + (cols - 1) * rMax
      import scala.collection.mutable
      var queue =
        new mutable.Queue[Pos]

      def shiftAmt = 14
      def storeAnswer(p: Pos, v: Int): Unit = {
        val gv = getValue(p)
        grid.updateAt(p, gv ^ ((v + 1) << shiftAmt))
      }
      def getAnswer(p: Pos): Int = {
        val a1 = (grid.at(p) >> shiftAmt) - 1
        if (a1 == -1)
          Int.MaxValue
        else a1
      }
      def getValue(p: Pos): Int = {
        grid.at(p) & ((1 << shiftAmt) - 1)
      }

      queue += 0
      storeAnswer(0, 0)
      while (queue.nonEmpty) {
        val pos = queue.dequeue()

        val basecost = getAnswer(pos)
        val freeSpace = pos.move(getValue(pos))
        List(1, 2, 3, 4)
          .map(d => pos.move(d))
          .filter(_.isInBounds)
          .foreach(newpos => {
            val tcost = if newpos == freeSpace then 0 else 1
            val cost = basecost + tcost
            if (cost < getAnswer(newpos)) {
              queue += newpos
              storeAnswer(newpos, cost)
            }
          })
      }
      getAnswer(target)
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .minCost(
        parseArrayArrayInt(
          "[[1,1,3],[3,2,2],[1,1,4]]"
        )
      )
      .pipe(println)
  }

}
