package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object LetterTilePossibilities1079 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def numTilePossibilities(tiles: String): Int = {
      val p1 = tiles.permutations.toSet
      (p1 ++ (1 until tiles.size)
        .map(n => p1.map(_.drop(n)))
        .reduceOption(_ ++ _)
        .getOrElse(Set())).size
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .numTilePossibilities(
        "AAB"
      )
      .pipe(pprintln(_))
    Solution
      .numTilePossibilities(
        "AAABBC"
      )
      .pipe(pprintln(_))
    Solution
      .numTilePossibilities(
        "V"
      )
      .pipe(pprintln(_))
  }

}
