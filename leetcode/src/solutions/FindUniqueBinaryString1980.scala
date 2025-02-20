package solutions

import leetcode.macros.Macros.logged
import solutions.Util._
import pprint.pprintln
import scala.util.chaining._

@scala.annotation.experimental
object FindUniqueBinaryString1980 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def findDifferentBinaryString(nums: Array[String]): String = {
      nums.zipWithIndex.map({ case (s, ind) => 1 - (s(ind) - '0') }).mkString
    }
  }

  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .findDifferentBinaryString(
        parseArrayString(
          """["01","10"]"""
        )
      )
      .pipe(pprintln(_))
    Solution
      .findDifferentBinaryString(
        parseArrayString(
          """["00","01"]"""
        )
      )
      .pipe(pprintln(_))
    Solution
      .findDifferentBinaryString(
        parseArrayString(
          """["111","011","001"]"""
        )
      )
      .pipe(pprintln(_))
  }

}
