package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object CheckifNumberisaSumofPowersofThree1780 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def checkPowersOfThree(_n: Int): Boolean = {
      var n = _n
      while (n > 0) {
        if (n % 3 == 2)
          return false
        n /= 3
      }
      true
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      @scala.annotation.tailrec
      def checkPowersOfThree(n: Int): Boolean = {
        n == 0 || n % 3 != 2 && checkPowersOfThree(n / 3)
      }
    }
  }
  def main(args: Array[String]): Unit = {

    Solution
      .checkPowersOfThree(
        12
      )
      .pipe(pprintln(_))
    Solution
      .checkPowersOfThree(
        91
      )
      .pipe(pprintln(_))
    Solution
      .checkPowersOfThree(
        21
      )
      .pipe(pprintln(_))
  }

}
