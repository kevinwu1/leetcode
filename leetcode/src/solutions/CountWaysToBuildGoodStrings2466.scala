package solutions

import leetcode.macros.Macros.logged
import solutions.Util._

import scala.util.chaining._

object CountWaysToBuildGoodStrings2466 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  import scala.collection.mutable.ArrayDeque
  object Solution {
    def countGoodStrings(low: Int, high: Int, zero: Int, one: Int): Int = {
      val M = 1000000007
      var total = 0
      var ad = ArrayDeque[Int]()
      var lInd = 0
      val adLen = Math.max(zero, one)
      def getWays(i: Int): Int = {
        val p1Ind = i - zero
        val p2Ind = i - one
        val zeroWays =
          if (p1Ind < 0) 0 else if (p1Ind == 0) 1 else ad(p1Ind - lInd)
        val oneWays =
          if (p2Ind < 0) 0 else if (p2Ind == 0) 1 else ad(p2Ind - lInd)
        (zeroWays + oneWays) % M
      }
      for (i <- 0 until Math.min(low, adLen)) {
        ad += getWays(i)
      }
      println(lInd + ":: " + ad)
      while (lInd + ad.length < low) {
        // increase until low
        ad += getWays(lInd + ad.length)
        if (ad.size > adLen) {
          ad.removeHead()
          lInd += 1
        }
      }
      println(lInd + ":: " + ad)
      for (i <- low to high) {
        val ways = getWays(i)
        ad += ways
        if (ad.size > adLen) {
          ad.removeHead()
          lInd += 1
        }
        total = (total + ways) % M
      }
      total
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .countGoodStrings(
        2,
        3,
        1,
        2
      )
      .pipe(println)
  }

}
