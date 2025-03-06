package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object FindMissingandRepeatedValues2965 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def findMissingAndRepeatedValues(grid: Array[Array[Int]]): Array[Int] = {
      val n = grid.length
      val flat = grid.flatten
      val a = flat.groupBy(identity).find({ case (k, v) => v.size == 2 }).get._1
      val axb = flat.foldLeft((1 to n * n).reduce(_ ^ _))(_ ^ _)
      Array(a, a ^ axb)
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .findMissingAndRepeatedValues(
        parseArrayArrayInt(
          "[[1,3],[2,2]]"
        )
      )
      .pipe(pprintln(_))
  }

}
