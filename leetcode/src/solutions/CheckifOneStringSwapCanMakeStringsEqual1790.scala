package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object CheckifOneStringSwapCanMakeStringsEqual1790 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def areAlmostEqual(s1: String, s2: String): Boolean = {
      s1 == s2 || {
        val mismatches = s1
          .zip(s2)
          .collect({
            case (a, b) if a != b => (a, b)
          })
          .toArray
        mismatches match {
          case Array((a, b), (c, d)) if a == d && b == c => true
          case _                                         => false
        }
      }
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .areAlmostEqual(
        "bank",
        "kanb"
      )
      .pipe(pprintln(_))
    Solution
      .areAlmostEqual(
        "attack",
        "defend"
      )
      .pipe(pprintln(_))
    Solution
      .areAlmostEqual(
        "kelb",
        "kelb"
      )
      .pipe(pprintln(_))
  }

}
