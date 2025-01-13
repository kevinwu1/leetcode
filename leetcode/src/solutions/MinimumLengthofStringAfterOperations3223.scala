package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MinimumLengthofStringAfterOperations3223 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def minimumLength(s: String): Int = {
      s.groupBy(identity)
        .map({
          case (k, v) => {
            val s = v.size
            if (s <= 2)
              s
            else {
              if (s % 2 == 1)
                1
              else
                2
            }
          }
        })
        .sum
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def minimumLength(s: String): Int = {
        val cnt = Array.fill(26)(0)
        var i = 0
        while (i < s.length()) {
          cnt(s(i) - 'a') += 1
          i += 1
        }
        var total = 0
        var j = 0;
        while (j < 26) {
          val s = cnt(j)
          if (s != 0) {
            total += (2 - (s % 2))
          }
          j += 1
        }
        total
      }
    }
  }
  def main(args: Array[String]): Unit = {

    Solution
      .minimumLength(
        "abaacbcbb"
      )
      .pipe(println)
  }

}
