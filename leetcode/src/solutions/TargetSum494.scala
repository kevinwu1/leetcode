package solutions

import leetcode.macros.Macros.logged
import solutions.Util._

import scala.util.chaining._

object TargetSum494 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def findTargetSumWays(nums: Array[Int], target: Int): Int = {
      import scala.collection.mutable
      def memo = mutable.Map[(Int, Int), Int]()
      def helper(startInd: Int, target: Int): Int = {
        if (startInd == nums.size) {
          if (target == 0)
            1
          else
            0
        } else {
          memo.getOrElseUpdate(
            (startInd, target), {
              val item = nums(startInd)
              helper(startInd + 1, target + item) +
                helper(startInd + 1, target - item)
            }
          )
        }
      }
      helper(0, target)
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .findTargetSumWays(
        parseArrayInt(
          "[1,1,1,1,1]"
        ),
        3
      )
      .pipe(println)
  }

}
