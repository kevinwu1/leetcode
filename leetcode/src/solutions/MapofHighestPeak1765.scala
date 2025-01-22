package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*
import sourcecode.Pkg

import scala.util.chaining.*

@scala.annotation.experimental
object MapofHighestPeak1765 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def highestPeak(isWater: Array[Array[Int]]): Array[Array[Int]] = {
      val rows = isWater.length
      val cols = isWater.head.length

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
      }
      val down = (1, 0)
      val right = (0, 1)
      val up = -down
      val left = -right
      val directions = List(down, right, up, left)
      extension (p: Pos) {
        def neighbors: List[Pos] = directions.map(p + _).filter(_.isInBounds)
      }

      extension (arr: Array[Array[Int]]) {
        def apply(p: Pos): Int = arr(p.r)(p.c)
        def update(p: Pos, value: Int): Unit = arr(p.r)(p.c) = value
      }

      import scala.collection.mutable
      val queue = mutable.Queue[(Pos, Int)]()
      for (
        r <- 0 until rows;
        c <- 0 until cols;
        pos = (r, c)
      ) {
        if (isWater(pos) == 1) {
          queue += ((pos, 0))
        }
        isWater(pos) = -1
      }
      while (queue.nonEmpty) {
        val (pos, value) = queue.dequeue()
        if (isWater(pos) == -1) {
          isWater(pos) = value
          pos.neighbors.foreach(n => queue += ((n, value + 1)))
        }
      }
      isWater
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  def main(args: Array[String]): Unit = {
    Solution
      .highestPeak(
        parseArrayArrayInt(
          "[[0,0,1],[1,0,0],[0,0,0]]"
        )
      )
      .map(_.mkString(" "))
      .mkString("\n")
      .pipe(println)
  }

}
