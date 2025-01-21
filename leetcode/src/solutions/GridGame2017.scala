package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object GridGame2017 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def gridGame(grid: Array[Array[Int]]): Long = {
      val topSum = grid.head.map(_.toLong).sum - grid.head.head
      val botSum = 0L
      (1 until grid.head.length)
        .foldLeft((topSum, topSum, botSum))({
          case ((best, topSum, botSum), i) =>
            val ts = topSum - grid(0)(i)
            val bs = botSum + grid(1)(i - 1)
            (Math.min(best, Math.max(ts, bs)), ts, bs)
        })
        ._1
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def gridGame(grid: Array[Array[Int]]): Long = {
        var topSum = {
          var i = 1
          var sum = 0L
          while (i < grid.head.length) {
            sum += grid.head(i)
            i += 1
          }
          sum
        }
        var botSum = 0L

        var best = topSum
        var i = 1
        while (i < grid.head.length) {
          topSum -= grid(0)(i)
          botSum += grid(1)(i - 1)
          best = Math.min(best, Math.max(topSum, botSum))
          i += 1
        }
        best
      }
    }
  }
  def main(args: Array[String]): Unit = {

    Solution
      .gridGame(
        parseArrayArrayInt(
          "[[1,3,1,15],[1,3,3,1]]"
        )
      )
      .pipe(println)
  }

}
