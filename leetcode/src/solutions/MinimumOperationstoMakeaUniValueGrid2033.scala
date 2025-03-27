package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MinimumOperationstoMakeaUniValueGrid2033 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def minOperations(grid: Array[Array[Int]], x: Int): Int = {
      val items = grid.flatten
      if (items.toSet.map(_ % x).size >= 2)
        -1
      else {
        val divs = items.map(_ / x).sorted
        val csum = divs.scanLeft(0)(_ + _)
        val n = divs.length
        var best = Int.MaxValue
        var i = 0

        while (i < divs.length) {
          val sum1 = -csum(i)
          val sum2 = csum(n) - csum(i + 1)
          val c = divs(i) * (2 * i - n + 1)
          val attempt = sum1 + sum2 + c
          best = Math.min(best, attempt)
          i += 1
        }
        best
      }
    }
  }

  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def minOperations(grid: Array[Array[Int]], x: Int): Int = {
        val items = grid.flatten
        if (items.exists(_ % x != items.head % x))
          -1
        else {
          items.mapInPlace(_ / x).sortInPlace()
          var total = 0
          var i = 0
          val n = items.length
          while (i < n / 2) {
            total += items(n - 1 - i) - items(i)
            i += 1
          }
          total
        }
      }
    }
  }
  def main(args: Array[String]): Unit = {

    Solution
      .minOperations(
        parseArrayArrayInt(
          "[[2,4],[6,8]]"
        ),
        2
      )
      .pipe(pprintln(_))
    Solution
      .minOperations(
        parseArrayArrayInt(
          "[[1,5],[2,3]]"
        ),
        1
      )
      .pipe(pprintln(_))
    Solution
      .minOperations(
        parseArrayArrayInt(
          "[[1,2],[3,4]]"
        ),
        2
      )
      .pipe(pprintln(_))
  }

}
