package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object LongestStrictlyIncreasingorStrictlyDecreasingSubarray3105 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def longestMonotonicSubarray(nums: Array[Int]): Int = {
      var best = 1
      var prev = nums.head
      var incr = 1
      var decr = 1
      var i = 1
      while (i < nums.length) {
        val next = nums(i)
        if (next > prev) {
          incr += 1
          decr = 1
        } else if (next < prev) {
          decr += 1
          incr = 1
        } else {
          incr = 1
          decr = 1
        }
        best = Math.max(best, Math.max(incr, decr))
        prev = next
        i += 1
      }
      best
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .longestMonotonicSubarray(
        parseArrayInt(
          "[1,4,3,3,2]"
        )
      )
      .pipe(pprintln(_))
    Solution
      .longestMonotonicSubarray(
        parseArrayInt(
          "[3,3,3,3]"
        )
      )
      .pipe(pprintln(_))
    Solution
      .longestMonotonicSubarray(
        parseArrayInt(
          "[3,2,1]"
        )
      )
      .pipe(pprintln(_))
  }

}
