package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object ConstructSmallestNumberFromDIString2375 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def smallestNumber(pattern: String): String = {
      var patt = pattern + "I"
      var ans = ""
      var lowest = 1
      while (patt.size != 0) {
        val incInd = patt.indexOf("I")
        val answerPart = (lowest until lowest + incInd + 1).reverse.mkString
        lowest += answerPart.size
        patt = patt.slice(incInd + 1, patt.size)
        ans += answerPart
      }
      ans
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .smallestNumber(
        "IIIDIDDD"
      )
      .pipe(pprintln(_))
    Solution
      .smallestNumber(
        "DDD"
      )
      .pipe(pprintln(_))
  }

}
