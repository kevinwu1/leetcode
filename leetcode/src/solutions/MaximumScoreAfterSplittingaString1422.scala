package solutions

import leetcode.macros.Macros.logged
import solutions.Util._

import scala.util.chaining._

object MaximumScoreAfterSplittingaString1422 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def maxScore(s: String): Int = {
      (1 until s.length())
        .map(i =>
          s.slice(0, i).count(_ == '0') + s.slice(i, s.length()).count(_ == '1')
        )
        .max
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .maxScore(
        "00111"
      )
      .pipe(println)
  }

}
