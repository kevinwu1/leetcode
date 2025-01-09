package solutions

import leetcode.macros.Macros.logged
import solutions.Util._

import scala.util.chaining._

@scala.annotation.experimental
object CountingWordsWithaGivenPrefix2185 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def prefixCount(words: Array[String], pref: String): Int = {
      words.count(_.startsWith(pref))
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .prefixCount(
        parseArrayString(
          """["pay","attention","practice","attend"]"""
        ),
        "at"
      )
      .pipe(println)
  }

}
