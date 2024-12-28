package solutions

import leetcode.macros.Macros.logged
import solutions.Util._

import scala.util.chaining._

object BestSightseeingPair1014 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def maxScoreSightseeingPair(values: Array[Int]): Int = {
      val n = values.length
      var maxRight = values(n - 1) - (n - 1)
      var best = maxRight
      for (j <- (0 to n - 2).reverse) {
        val attempt = values(j) + j + maxRight
        if (attempt > best)
          best = attempt
        val newright = values(j) - (j)
        if (newright > maxRight)
          maxRight = newright
      }
      best
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def maxScoreSightseeingPair(values: Array[Int]): Int = {
        values.indices.tail
          .foldLeft((0, values.head))({ case ((best, left), ind) =>
            (
              Math.max(best, left + values(ind) - ind),
              Math.max(left, values(ind) + ind)
            )
          })
          ._1
      }
    }
  }
  def main(args: Array[String]): Unit = {

    Sol2.Solution
      .maxScoreSightseeingPair(
        parseArrayInt(
          "[8,1,5,2,6]"
        )
      )
      .pipe(println)
  }

}
