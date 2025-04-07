package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object PartitionEqualSubsetSum416 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    var doLog = false
    def println(a: Any): Unit =
      if (doLog)
        Console.out.println(a)
    def canPartition(nums: Array[Int]): Boolean = {
      val total = nums.sum
      val n = nums.length
      val dp = Array.ofDim[Int](total / 2 + 1, n)
      (0 until n).foreach(ind => dp(0)(ind) = 1)
      // pprintln(dp)
      // @logged
      def cansubsetsum(sum: Int, from: Int): Boolean = {
        sum >= 0 && from < n && {
          if (dp(sum)(from) == 0) {
            dp(sum)(from) =
              if (
                cansubsetsum(sum, from + 1) ||
                cansubsetsum(sum - nums(from), from + 1)
              ) 1
              else 2
          }
          dp(sum)(from) == 1
        }
      }
      total % 2 == 0 && {
        cansubsetsum(total / 2, 0)
      }
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      var doLog = false
      def println(a: Any): Unit =
        if (doLog)
          Console.out.println(a)
      def canPartition(nums: Array[Int]): Boolean = {
        val total = nums.sum
        val n = nums.length
        val dp = Array.ofDim[Boolean](total / 2 + 1, n)
        (0 until n).foreach(ind => dp(0)(ind) = true)
        // pprintln(dp)
        // @logged
        var i = n - 2
        while (i >= 0) {
          var j = 0
          while (j < total / 2 + 1) {
            dp(j)(i) = (dp(j)(i + 1)) ||
              (j - nums(i) >= 0 && dp(j - nums(i))(i + 1))
            j += 1
          }
          i -= 1
        }
        total % 2 == 0 && {
          dp(total / 2)(0)
        }
      }
    }
  }
  def main(args: Array[String]): Unit = {
    Solution.doLog = true
    Solution
      .canPartition(
        parseArrayInt(
          "[1,5,11,5]"
        )
      )
      .pipe(pprintln(_))
    Solution
      .canPartition(
        parseArrayInt(
          "[1,2,3,5]"
        )
      )
      .pipe(pprintln(_))
  }

}
