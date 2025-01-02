package solutions

import leetcode.macros.Macros.logged
import solutions.Util._

import scala.util.chaining._

object CountVowelStringsinRanges2559 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def vowelStrings(
        words: Array[String],
        queries: Array[Array[Int]]
    ): Array[Int] = {

      implicit class CharImpls(c: Char) {
        def isVowel: Boolean =
          c == 'a' ||
            c == 'e' ||
            c == 'i' ||
            c == 'o' ||
            c == 'u'
      }

      val csum = words
        .map(word =>
          if (word.head.isVowel && word.last.isVowel)
            1
          else
            0
        )
        .scanLeft(0)(_ + _)
      queries.map({ case Array(s, e) =>
        csum(e + 1) - csum(s)
      })
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .vowelStrings(
        parseArrayString(
          """["a","e","i"]"""
        ),
        parseArrayArrayInt(
          "[[0,2],[0,1],[2,2]]"
        )
      )
      .mkString(",")
      .pipe(println)
  }

}
