package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object LengthofLongestFibonacciSubsequence873 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def lenLongestFibSubseq(arr: Array[Int]): Int = {
      val aset = arr.toSet
      @scala.annotation.tailrec
      def getLen(a: Int, b: Int, len: Int): Int = {
        if (aset.contains(a + b))
          getLen(b, a + b, len + 1)
        else
          len
      }

      val ans = (for (
        i <- arr.indices;
        j <- i + 1 until arr.length
      ) yield getLen(arr(i), arr(j), 2)).max
      if (ans == 2) 0 else ans
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .lenLongestFibSubseq(
        parseArrayInt(
          "[2,4,7,8,9,10,14,15,18,23,32,50]"
        )
      )
      .pipe(pprintln(_))
  }

}
