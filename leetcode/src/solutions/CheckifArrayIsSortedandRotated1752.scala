package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object CheckifArrayIsSortedandRotated1752 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def check(nums: Array[Int]): Boolean = {
      (0 until nums.length).count(i =>
        nums(i) > nums((i + 1) % nums.length)
      ) < 2
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def check(nums: Array[Int]): Boolean = {
        var found1 = false
        var i = 0
        while (i < nums.length) {
          if (nums(i) > nums((i + 1) % nums.length)) {
            if (found1)
              return false
            else
              found1 = true
          }
          i += 1
        }
        true
      }
    }
  }
  def main(args: Array[String]): Unit = {

    Solution
      .check(
        parseArrayInt(
          "[3,4,5,1,2]"
        )
      )
      .pipe(pprintln(_))
  }

}
