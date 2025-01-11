package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object ConstructKPalindromeStrings1400 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def canConstruct(s: String, k: Int): Boolean = {
      s.size >= k && s
        .groupBy(identity)
        .count({ case (k, v) => (v.size % 2) == 1 }) <= k
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def canConstruct(s: String, k: Int): Boolean = {
        if (s.size < k)
          return false
        var total = 0
        val cnt = Array.fill(26)(0)
        var i = 0
        while (i < s.length()) {
          val c = s(i) - 'a'
          cnt(c) += 1
          i += 1
        }
        var l = 0
        while (l < 26) {
          if (cnt(l) % 2 == 1)
            total += 1
          l += 1
        }
        total <= k
      }
    }
  }
  // object Solution {
  //   def canConstruct(s: String, k: Int): Boolean = {
  //     if (s.size < k)
  //       return false
  //     var total = 0
  //     var i = 0
  //     import scala.collection.mutable
  //     val bs = new mutable.BitSet()
  //     while (i < s.length()) {
  //       val c = s(i) - 'a'
  //       if (bs.contains(c)) {
  //         bs -= (c)
  //         total -= 1
  //       } else {
  //         bs += c
  //         total += 1
  //       }
  //       i += 1
  //     }
  //     total <= k
  //   }
  // }
  def main(args: Array[String]): Unit = {

    Solution
      .canConstruct(
        "annabelle",
        2
      )
      .pipe(println)
  }

}
