package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object SolvingQuestionsWithBrainpower2140 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    var doLog = false
    def println(a: Any): Unit =
      if (doLog)
        Console.out.println(a)
    def mostPoints(questions: Array[Array[Int]]): Long = {
      val n = questions.length
      val dp = new Array[Long](n)
      inline def getBest(i: Int): Long = {
        if (i >= n) 0 else dp(i)
      }
      var i = n - 1
      while (i >= 0) {
        dp(i) = Math.max(
          questions(i)(0) + getBest(i + questions(i)(1) + 1),
          getBest(i + 1)
        )
        i -= 1
      }
      dp(0)
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  def main(args: Array[String]): Unit = {
    Solution.doLog = true
    Solution
      .mostPoints(
        parseArrayArrayInt(
          "[[3,2],[4,3],[4,4],[2,5]]"
        )
      )
      .pipe(pprintln(_))
  }

}
