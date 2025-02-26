package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MaximumAbsoluteSumofAnySubarray1749 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def maxAbsoluteSum(nums: Array[Int]): Int = {
      var ans = 0
      var pMin = 0
      var pMax = 0
      var i = 0
      while (i < nums.length) {
        val num = nums(i)
        pMax = Math.max(pMax + num, num)
        pMin = Math.min(pMin + num, num)
        ans = Math.max(ans, Math.max(pMax, -pMin))
        i += 1
      }
      ans
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def maxAbsoluteSum(nums: Array[Int]): Int = {
        nums
          .foldLeft((0, 0, 0))({ case ((ans, pMin, pMax), num) =>
            val pMax2 = Math.max(pMax + num, num)
            val pMin2 = Math.min(pMin + num, num)
            val ans2 = Math.max(ans, Math.max(pMax2, -pMin2))
            (ans2, pMin2, pMax2)
          })
          ._1
      }
    }
  }
  def main(args: Array[String]): Unit = {

    // Solution
    //   .maxAbsoluteSum(
    //     parseArrayInt(
    //       "[1,-3,2,3,-4]"
    //     )
    //   )
    //   .pipe(pprintln(_))
    // Solution
    //   .maxAbsoluteSum(
    //     parseArrayInt(
    //       "[2,-5,1,-4,3,-2]"
    //     )
    //   )
    //   .pipe(pprintln(_))
    Solution
      .maxAbsoluteSum(
        parseArrayInt(
          "[-3,-5,-3,-2,-6,3,10,-10,-8,-3,0,10,3,-5,8,7,-9,-9,5,-8]"
        )
      )
      .pipe(pprintln(_))
  }

}
