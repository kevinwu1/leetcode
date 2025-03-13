package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object ZeroArrayTransformationII3356 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def minZeroArray(nums: Array[Int], queries: Array[Array[Int]]): Int = {
      var total = nums.map(_.toLong).sum
      if (total == 0)
        0
      else {
        import scala.collection.mutable
        val potentials = mutable.Map[(Int, Int), Int]().withDefaultValue(0)
        var totalPotential = 0L
        var k = 0
        while (k < queries.size) {
          val Array(l, r, dec) = queries(k)
          if (totalPotential < total) {
            val potent = (r - l + 1) * dec
            potentials((l, r)) += dec
            totalPotential += potent
            if (totalPotential >= total) {
              pprintln(potentials)
              potentials.foreach({ case ((l, r), dec) =>
                var it = l
                while (it <= r) {
                  val n = nums(it)
                  if (n < dec) {
                    nums(it) = 0
                    total -= n
                  } else {
                    nums(it) -= dec
                    total -= dec
                  }
                  it += 1
                }
              })
              pprintln(nums)
            }
          } else {
            var it = l
            while (it <= r) {
              val n = nums(it)
              if (n < dec) {
                nums(it) = 0
                total -= n
              } else {
                nums(it) -= dec
                total -= dec
              }
              it += 1
            }
          }
          k += 1
          if (total == 0)
            return k
        }
        -1
      }
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def minZeroArray(nums: Array[Int], queries: Array[Array[Int]]): Int = {
        val csum = new Array[Int](nums.length + 1)
        var notZeroInd = 0
        var currentSum = 0
        def isAllZero(): Boolean = {
          while (notZeroInd < nums.length && currentSum >= nums(notZeroInd)) {
            notZeroInd += 1
            currentSum += csum(notZeroInd)
          }
          notZeroInd == nums.length
        }

        var answer = 0
        while (answer < queries.length && !isAllZero()) {
          val Array(l, r, dec) = queries(answer)
          if (l <= notZeroInd && notZeroInd <= r) {
            currentSum += dec
          } else {
            csum(l) += dec
          }
          csum(r + 1) -= dec
          answer += 1
        }
        if (isAllZero())
          answer
        else
          -1
      }
    }
  }
  def main(args: Array[String]): Unit = {

    // Solution
    //   .minZeroArray(
    //     parseArrayInt(
    //       "[2,0,2]"
    //     ),
    //     parseArrayArrayInt(
    //       "[[0,2,1],[0,2,1],[1,1,3]]"
    //     )
    //   )
    //   .pipe(pprintln(_))
    // Solution
    //   .minZeroArray(
    //     parseArrayInt(
    //       "[4,3,2,1]"
    //     ),
    //     parseArrayArrayInt(
    //       "[[1,3,2],[0,2,1]]"
    //     )
    //   )
    //   .pipe(pprintln(_))
    Sol2.Solution
      .minZeroArray(
        parseArrayInt(
          "[7,6,8]"
        ),
        parseArrayArrayInt(
          "[[0,0,2],[0,1,5],[2,2,5],[0,2,4]]"
        )
      )
      .pipe(pprintln(_))
  }

}
