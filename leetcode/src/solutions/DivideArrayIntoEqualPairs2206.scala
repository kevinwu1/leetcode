package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object DivideArrayIntoEqualPairs2206 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def divideArray(nums: Array[Int]): Boolean = {
      val seen = new Array[Boolean](501)
      var remaining = 0
      var i = 0
      while (i < nums.length) {
        val el = nums(i)
        seen(el) = !seen(el)
        remaining += (if (seen(el)) 1 else -1)
        i += 1
      }
      remaining == 0
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .divideArray(
        parseArrayInt(
          "[3,2,3,2,2,2]"
        )
      )
      .pipe(pprintln(_))
  }

}
