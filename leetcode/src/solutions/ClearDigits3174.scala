package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object ClearDigits3174 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def clearDigits(s: String): String = {
      import scala.collection.mutable
      val sb = mutable.StringBuilder()
      var i = s.length() - 1
      var digitsSeen = 0
      while (i >= 0) {
        val char = s.charAt(i)
        val isDigit = char.isDigit
        (isDigit, digitsSeen) match {
          case (true, _) =>
            digitsSeen += 1
          case (false, 0) =>
            sb += char
          case (false, _) =>
            digitsSeen -= 1
        }
        i -= 1
      }
      sb.reverseInPlace().toString
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .clearDigits(
        "abc"
      )
      .pipe(pprintln(_))
    Solution
      .clearDigits(
        "cb34"
      )
      .pipe(pprintln(_))
  }

}
