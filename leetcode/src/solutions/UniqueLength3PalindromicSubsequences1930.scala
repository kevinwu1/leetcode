package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object UniqueLength3PalindromicSubsequences1930 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def countPalindromicSubsequence(s: String): Int = {
      def countPalindromesFor(c: Char): Int = {
        val start = s.indexOf(c)
        val end = s.lastIndexOf(c)
        if (start + 1 >= end)
          0
        else
          s.substring(start + 1, end).toSet.size
      }
      (for (c <- 'a' to 'z')
        yield countPalindromesFor(c)).sum
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def countPalindromicSubsequence(s: String): Int = {
        val firstInd: Array[Int] = Array.fill(26)(-1)
        val lastInd: Array[Int] = Array.fill(26)(-1)
        for (i <- s.indices) {
          val letInd = s(i) - 'a'
          if (firstInd(letInd) == -1)
            firstInd.update(letInd, i)
        }
        for (i <- s.indices.reverse) {
          val letInd = s(i) - 'a'
          if (lastInd(letInd) == -1)
            lastInd.update(letInd, i)
        }
        def countPalindromesFor(c: Char): Int = {
          val start = firstInd(c - 'a')
          val end = lastInd(c - 'a')
          import scala.collection.mutable.BitSet
          val bs = new BitSet(26)
          for (i <- start + 1 until end) {
            bs.add(s(i) - 'a')
          }
          bs.size
        }
        ('a' to 'z').map(countPalindromesFor).sum
      }
    }
  }
  def main(args: Array[String]): Unit = {

    Solution
      .countPalindromicSubsequence(
        "aabca"
      )
      .pipe(println)
  }

}
