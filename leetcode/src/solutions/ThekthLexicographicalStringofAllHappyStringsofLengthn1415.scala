package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object ThekthLexicographicalStringofAllHappyStringsofLengthn1415 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def getHappyString(n: Int, k_ : Int): String = {
      val k = k_ - 1
      val parts = 1 << (n - 1)
      if (k >= parts * 3)
        ""
      else {
        val firstLetter =
          if (k < parts) "a" else if (k < parts + parts) "b" else "c"
        val remaining = k % parts
        var remainingBits = n - 1
        val ans = new StringBuilder()
        ans.append(firstLetter)
        while (remainingBits > 0) {
          val nextLetter = if ((remaining & (1 << (remainingBits - 1))) != 0) {
            ans.last match {
              case 'a' => 'c'
              case 'b' => 'c'
              case 'c' => 'b'
            }
          } else {
            ans.last match {
              case 'a' => 'b'
              case 'b' => 'a'
              case 'c' => 'a'
            }
          }
          ans.append(nextLetter)
          remainingBits -= 1
        }
        ans.toString
      }
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .getHappyString(
        1,
        3
      )
      .pipe(pprintln(_))
    Solution
      .getHappyString(
        1,
        4
      )
      .pipe(pprintln(_))
    Solution
      .getHappyString(
        3,
        9
      )
      .pipe(pprintln(_))
  }

}
