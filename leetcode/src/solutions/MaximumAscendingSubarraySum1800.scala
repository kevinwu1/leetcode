package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MaximumAscendingSubarraySum1800 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def maxAscendingSum(nums: Array[Int]): Int = {
      var prev = nums.head
      var csum = prev
      var best = csum
      var i = 1
      while (i < nums.length) {
        val next = nums(i)
        if (next > prev) {
          csum += next
        } else {
          csum = next
        }
        best = Math.max(best, csum)
        prev = next
        i += 1
      }
      best
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .maxAscendingSum(
        parseArrayInt(
          "[10,20,30,5,10,50]"
        )
      )
      .pipe(pprintln(_))
    Solution
      .maxAscendingSum(
        parseArrayInt(
          "[10,20,30,40,50]"
        )
      )
      .pipe(pprintln(_))
    Solution
      .maxAscendingSum(
        parseArrayInt(
          "[12,17,15,13,10,11,12]"
        )
      )
      .pipe(pprintln(_))
  }

}
