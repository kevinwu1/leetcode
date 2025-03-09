package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object AlternatingGroupsII3208 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def numberOfAlternatingGroups(colors: Array[Int], k: Int): Int = {
      val n = colors.size
      implicit class IntImpls(i: Int) {
        def %+(v: Int): Int = (((i + v) % n) + n) % n
      }
      val dif =
        colors.zipWithIndex.map((c, i) => if (c != colors(i %+ 1)) 1 else 0)
      val initial = (0 until k - 1).map(dif).sum
      (k - 1 until n + k - 2)
        .foldLeft((if (initial == k - 1) 1 else 0, initial))({
          case ((total, prev), ind) =>
            val newSum = prev + dif(ind % n) - dif(ind %+ -(k - 1))
            (total + (if (newSum == k - 1) 1 else 0), newSum)
        })
        ._1

    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .numberOfAlternatingGroups(
        parseArrayInt(
          "[0,1,0,1,0]"
        ),
        3
      )
      .pipe(pprintln(_))
    Solution
      .numberOfAlternatingGroups(
        parseArrayInt(
          "[0,1,0,0,1,0,1]"
        ),
        6
      )
      .pipe(pprintln(_))
    Solution
      .numberOfAlternatingGroups(
        parseArrayInt(
          "[1,1,0,1]"
        ),
        4
      )
      .pipe(pprintln(_))
    Solution
      .numberOfAlternatingGroups(
        parseArrayInt(
          "[0,0,1]"
        ),
        3
      )
      .pipe(pprintln(_))
  }

}
