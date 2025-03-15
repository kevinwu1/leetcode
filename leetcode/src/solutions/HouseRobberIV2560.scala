package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object HouseRobberIV2560 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def minCapability(nums: Array[Int], k: Int): Int = {
      val lo = nums.min
      val hi = nums.max

      def canMake(limit: Int): Boolean = {
        var i = 0
        var needed = k
        while (needed > 0 && i < nums.size) {
          if (nums(i) <= limit) {
            needed -= 1
            i += 2
          } else {
            i += 1
          }
        }
        needed == 0
      }
      // @logged

      import scala.annotation.tailrec
      // @logged
      @tailrec
      def bsearchRange(lo: Int, hi: Int): Int = {
        if (lo + 1 >= hi)
          hi
        else {
          val mid = (lo + hi) / 2
          if (canMake(mid))
            bsearchRange(lo, mid)
          else
            bsearchRange(mid, hi)
        }
      }
      bsearchRange(lo - 1, hi)
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .minCapability(
        parseArrayInt(
          "[2,3,5,9]"
        ),
        2
      )
      .pipe(pprintln(_))
    Solution
      .minCapability(
        parseArrayInt(
          "[2,7,9,3,1]"
        ),
        2
      )
      .pipe(pprintln(_))
    Solution
      .minCapability(
        parseArrayInt(
          "[2,2]"
        ),
        1
      )
      .pipe(pprintln(_))
  }

}
