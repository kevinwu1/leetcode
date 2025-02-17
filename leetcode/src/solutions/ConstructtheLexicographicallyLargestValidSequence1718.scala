package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object ConstructtheLexicographicallyLargestValidSequence1718 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    import scala.collection.immutable.BitSet
    def constructDistancedSequence(n: Int): Array[Int] = {
      val maxLen = 2 * n - 1
      val ans = new Array[Int](maxLen)
      val used = new Array[Boolean](n + 1)
      extension (i: Int) {
        def canPlaceAt(ind: Int): Boolean =
          !used(i) && {
            if (i == 1)
              ans(ind) == 0
            else
              ans(ind) == 0 && ind + i < maxLen && ans(ind + i) == 0
          }

        def placeAt(ind: Int): Unit = {
          used(i) = true
          if (i == 1) {
            ans(ind) = i
          } else {
            ans(ind) = i
            ans(ind + i) = i
          }
        }

        def unPlaceAt(ind: Int): Unit = {
          used(i) = false
          if (i == 1) {
            ans(ind) = 0
          } else {
            ans(ind) = 0
            ans(ind + i) = 0
          }
        }
      }
      def fillIndex(ind: Int): Boolean = {
        if (ind == maxLen)
          true
        else if (ans(ind) != 0)
          fillIndex(ind + 1)
        else
          (1 to n).reverse
            .find(testVal => {
              testVal.canPlaceAt(ind) && {
                testVal.placeAt(ind)
                val wasFound = fillIndex(ind + 1)
                if (!wasFound)
                  testVal.unPlaceAt(ind)
                wasFound
              }
            })
            .nonEmpty
      }
      fillIndex(0)
      ans
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {
    (1 to 20).foreach(n => {
      Solution
        .constructDistancedSequence(n)
        .pipe(pprintln(_))
    })
  }

}
