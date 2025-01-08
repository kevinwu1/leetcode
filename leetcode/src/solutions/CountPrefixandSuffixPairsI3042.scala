package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object CountPrefixandSuffixPairsI3042 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def countPrefixSuffixPairs(words: Array[String]): Int = {
      val n = words.length
      var ans = 0
      var i = 0
      while (i < n) {
        var j = i + 1
        while (j < n) {
          if (
            words(j).length() >= words(i).length() && words(j).startsWith(
              words(i)
            ) && words(j).endsWith(words(i))
          )
            ans += 1
          j += 1
        }
        i += 1
      }
      ans
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def countPrefixSuffixPairs(words: Array[String]): Int = {
        val n = words.length
        var ans = 0
        @scala.annotation.tailrec
        def reci(i: Int): Unit = {
          if (i < n) {
            @scala.annotation.tailrec
            def recj(j: Int): Unit = {
              if (j < n) {
                if (
                  words(j).length() >= words(i).length() && words(j)
                    .startsWith(
                      words(i)
                    ) && words(j).endsWith(words(i))
                ) {
                  ans += 1
                }
                recj(j + 1)
              }
            }
            recj(i + 1)
            reci(i + 1)
          }
        }
        reci(0)
        ans
      }
    }
  }
  def main(args: Array[String]): Unit = {

    Solution
      .countPrefixSuffixPairs(
        parseArrayString(
          """["a","aba","ababa","aa"]"""
        )
      )
      .pipe(println)
  }

}
