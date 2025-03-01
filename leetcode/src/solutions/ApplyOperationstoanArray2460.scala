package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object ApplyOperationstoanArray2460 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def applyOperations(nums: Array[Int]): Array[Int] = {
      val n = nums.length
      for (i <- 0 until n - 1)
        if (nums(i) == nums(i + 1)) {
          nums(i) *= 2
          nums(i + 1) = 0
        }
      var writeInd = 0
      for (i <- 0 until n) {
        if (nums(i) != 0) {
          nums(writeInd) = nums(i)
          writeInd += 1
        }
      }
      while (writeInd < n) {
        nums(writeInd) = 0
        writeInd += 1
      }
      nums
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .applyOperations(
        parseArrayInt(
          "[1,2,2,1,1,0]"
        )
      )
      .pipe(pprintln(_))
    Solution
      .applyOperations(
        parseArrayInt(
          "[0,1]"
        )
      )
      .pipe(pprintln(_))
  }

}
