package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object SpecialArrayI3151 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def isArraySpecial(nums: Array[Int]): Boolean = {
      nums.length <= 1 || nums
        .sliding(2)
        .forall({ case Array(a, b) => (a & 1) != (b & 1) })
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def isArraySpecial(nums: Array[Int]): Boolean = {
        var i = 1
        while (i < nums.length) {
          if ((nums(i) & 1) == (nums(i - 1) & 1))
            return false
          i += 1
        }
        return true
      }
    }
  }
  def main(args: Array[String]): Unit = {

    Solution
      .isArraySpecial(
        parseArrayInt(
          "[1]"
        )
      )
      .pipe(pprintln(_))
    Solution
      .isArraySpecial(
        parseArrayInt(
          "[2,1,4]"
        )
      )
      .pipe(pprintln(_))
    Solution
      .isArraySpecial(
        parseArrayInt(
          "[4,3,1,6]"
        )
      )
      .pipe(pprintln(_))
  }

}
