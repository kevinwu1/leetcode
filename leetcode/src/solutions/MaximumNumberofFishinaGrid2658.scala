package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MaximumNumberofFishinaGrid2658 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def findMaxFish(grid: Array[Array[Int]]): Int = {
      val rows = grid.length
      val cols = grid.head.length

      type Pos = (Int, Int)
      extension (p: Pos) {
        def r: Int = p._1
        def c: Int = p._2
        def +(o: Pos): Pos = (r + o.r, c + o.c)
        def unary_- : Pos = (-r, -c)
        def -(o: Pos): Pos = p + -o
        def isInBounds: Boolean =
          0 <= r && r < rows &&
            0 <= c && c < cols
        def value: Int = grid(r)(c)
      }
      val down = (1, 0)
      val right = (0, 1)
      val up = -down
      val left = -right
      val directions = List(down, right, up, left)
      extension (p: Pos) {
        def neighbors: List[Pos] = directions.map(p + _).filter(_.isInBounds)
      }

      extension [T](arr: Array[Array[T]]) {
        def apply(p: Pos): T = arr(p.r)(p.c)
        def update(p: Pos, value: T): Unit = arr(p.r)(p.c) = value
      }

      extension (p: Pos) {
        def visited: Boolean = grid(p) == 0
        def setVisited(): Unit = grid(p) = 0
      }

      var best = 0
      for (
        r <- 0 until rows;
        c <- 0 until cols;
        pos = (r, c) if pos.value != 0 && !pos.visited
      ) {
        import scala.collection.mutable.Queue
        var total = 0
        val q = new Queue[Pos]()
        q += pos
        while (q.nonEmpty) {
          val p = q.dequeue()
          if (!p.visited) {
            total += p.value
            p.neighbors.filter(_.value != 0).foreach(q += _)
            p.setVisited()
          }
        }
        best = Math.max(best, total)
      }
      best
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .findMaxFish(
        parseArrayArrayInt(
          "[[0,2,1,0],[4,0,0,3],[1,0,0,4],[0,3,2,0]]"
        )
      )
      .pipe(println)
    Solution
      .findMaxFish(
        parseArrayArrayInt(
          "[[1,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,1]]"
        )
      )
      .pipe(println)
  }

}
