package solutions

import leetcode.macros.Macros.logged
import solutions.Util._

import scala.util.chaining._

object NumberofWaystoFormaTargetStringGivenaDictionary1639 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def numWays(words: Array[String], target: String): Int = {
      val M = 1000000007L
      implicit class modImpls(n: Long) {
        def |+|(o: Long): Long = ((n % M) + (o % M)) % M
        def |*|(o: Long): Long = ((n % M) * (o % M)) % M
      }

      val freqs: Map[(Char, Int), Int] = words
        .flatMap(_.zipWithIndex)
        .groupBy(identity)
        .map({ case (k, v) => k -> v.size })

      import scala.collection.mutable
      val memo = mutable.Map[(Int, Int), Long]()
      // @logged
      def countWays(fromIndex: Int, targetInd: Int): Long =
        memo.getOrElseUpdate(
          (fromIndex, targetInd), {
            if (targetInd == target.length())
              1
            else if (fromIndex == words.head.length())
              0
            else {
              val ways =
                freqs.getOrElse((target(targetInd), fromIndex), 0).toLong
              val waysIfUse = ways |*| countWays(fromIndex + 1, targetInd + 1)
              val waysIfNotUse = countWays(fromIndex + 1, targetInd)
              waysIfUse |+| waysIfNotUse
            }
          }
        )
      countWays(0, 0).toInt
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def numWays(words: Array[String], target: String): Int = {
        val M = 1000000007L
        import scala.collection.mutable.ArrayBuffer
        val dp =
          ArrayBuffer.fill(words.head.length() + 1, target.length() + 1)(0L)

        val freqs: Map[(Char, Int), Int] = words
          .flatMap(_.zipWithIndex)
          .groupBy(identity)
          .map({ case (k, v) => k -> v.size })
        for (
          targetInd <- (0 to target.length()).reverse;
          fromIndex <- (0 to words.head.length()).reverse
        ) {
          if (targetInd == target.length())
            dp(fromIndex)(targetInd) = 1
          else if (fromIndex == words.head.length())
            dp(fromIndex)(targetInd) = 0
          else {
            val dep = dp(fromIndex + 1)(targetInd + 1)
            val waysIfUse =
              if (dep == 0) 0
              else
                (dep * freqs
                  .getOrElse((target(targetInd), fromIndex), 0)
                  .toLong) % M
            val waysIfNotUse = dp(fromIndex + 1)(targetInd)
            dp(fromIndex)(targetInd) = (waysIfUse + waysIfNotUse) % M
          }
        }
        dp(0)(0).toInt
      }
    }
  }
  def main(args: Array[String]): Unit = {

    Sol2.Solution
      .numWays(
        parseArrayString(
          """["acca","bbbb","caca"]"""
        ),
        "aba"
      )
      .pipe(println)
  }

}
