package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object SumofAllSubsetXORTotals1863 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    var doLog = false
    def println(a: Any): Unit =
      if (doLog)
        Console.out.println(a)
    def subsetXORSum(nums: Array[Int]): Int = {
      Set(0 until nums.length: _*)
        .subsets()
        .map(
          _.iterator
            .map(nums)
            .foldLeft(0)(_ ^ _)
        )
        .sum
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {
    Solution.doLog = true
    // Solution
    //   .subsetXORSum(
    //     parseArrayInt(
    //       "[1,3]"
    //     )
    //   )
    //   .pipe(pprintln(_))
    Solution
      .subsetXORSum(
        parseArrayInt(
          "[1,1,1]"
        )
      )
      .pipe(pprintln(_))
  }

}
