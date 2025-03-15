package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MaximumCandiesAllocatedtoKChildren2226 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def maximumCandies(_candies: Array[Int], k: Long): Int = {
      val candies = _candies.map(_.toLong)
      import scala.annotation.tailrec
      // @logged
      @tailrec
      def bsearchRange(lo: Int, hi: Int, trueIfHigher: Int => Boolean): Int = {
        if (lo + 1 >= hi)
          lo
        else {
          val mid = (lo + hi) / 2
          if (trueIfHigher(mid))
            bsearchRange(mid, hi, trueIfHigher)
          else
            bsearchRange(lo, mid, trueIfHigher)
        }
      }
      bsearchRange(
        0,
        (candies.sum / k).toInt + 1,
        x => x == 0 || candies.map(_ / x).sum >= k
      )
    }
  }

  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .maximumCandies(
        parseArrayInt(
          "[5,8,6]"
        ),
        3
      )
      .pipe(pprintln(_))
    Solution
      .maximumCandies(
        parseArrayInt(
          "[2,5]"
        ),
        11
      )
      .pipe(pprintln(_))
  }

}
