package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MinimumOperationstoMakeBinaryArrayElementsEqualtoOneI3191 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def minOperations(nums: Array[Int]): Int = {
      @scala.annotation.tailrec
      def rec(start: Int, total: Int): Int = {
        if (start + 2 == nums.size) {
          if (nums(start) == 1 && nums(start + 1) == 1)
            total
          else -1
        } else if (nums(start) == 0) {
          nums(start + 1) = 1 - nums(start + 1)
          nums(start + 2) = 1 - nums(start + 2)
          rec(start + 1, total + 1)
        } else {
          rec(start + 1, total)
        }
      }
      rec(0, 0)
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .minOperations(
        parseArrayInt(
          "[0,1,1,1,0,0]"
        )
      )
      .pipe(pprintln(_))
  }

}
