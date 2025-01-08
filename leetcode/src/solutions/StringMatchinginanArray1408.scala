package solutions

import fansi.Trie
import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object StringMatchinginanArray1408 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def stringMatching(words: Array[String]): List[String] = {
      val wzip = words.zipWithIndex
      wzip
        .collect({
          case (word, ind) if wzip.exists({ case (w, i) =>
                i != ind && w.contains(word)
              }) =>
            word

        })
        .toList
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def stringMatching(words: Array[String]): List[String] = {
        words.sortInPlaceBy(_.length)
        var ans = List[String]()
        var i = 0
        while (i < words.length) {
          var matched = false
          var j = i + 1
          while (j < words.length) {
            if (words(j).contains(words(i))) {
              matched = true
              j = words.length
            }
            j += 1
          }
          if (matched)
            ans :+= words(i)
          i += 1
        }
        ans
      }
    }
  }
  def main(args: Array[String]): Unit = {

    Solution
      .stringMatching(
        parseArrayString(
          """["mass","as","hero","superhero"]"""
        )
      )
      .pipe(println)
  }

}
