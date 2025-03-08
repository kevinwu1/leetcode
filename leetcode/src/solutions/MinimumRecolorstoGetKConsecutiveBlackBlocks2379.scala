package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MinimumRecolorstoGetKConsecutiveBlackBlocks2379 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def minimumRecolors(blocks: String, k: Int): Int = {
      def isWhite(i: Int): Int = if (blocks(i) == 'W') 1 else 0
      val initial = (0 until k).map(isWhite).sum
      (k until blocks.size)
        .foldLeft((initial, initial))({ case ((min, prev), ind) =>
          val next = prev - isWhite(ind - k) + isWhite(ind)
          (Math.min(min, next), next)
        })
        ._1
    }
  }

  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .minimumRecolors(
        "WBBWWBBWBW",
        7
      )
      .pipe(pprintln(_))
    Solution
      .minimumRecolors(
        "WBWBBBW",
        2
      )
      .pipe(pprintln(_))
  }

}
