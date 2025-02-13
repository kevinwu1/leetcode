package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MaxSumofaPairWithEqualSumofDigits2342 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def maximumSum(nums: Array[Int]): Int = {
      class Top2() {
        var best2 = Array(-1, -1)
        def insert(i: Int): Int = {
          best2 = (best2 :+ i).sorted.slice(1, 3)
          if (best2(1) != -1)
            best2.sum
          else -1
        }
      }
      val top2s = scala.collection.mutable.Map[Int, Top2]()
      var ans = -1
      nums.foreach(i => {
        val digitSum = {
          var total = 0
          var j = i
          while (j > 0) {
            total += j % 10
            j /= 10
          }
          total
        }
        if (!top2s.contains(digitSum))
          top2s(digitSum) = new Top2()
        val insertResult = top2s(digitSum).insert(i)
        ans = Math.max(ans, insertResult)
      })
      ans
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .maximumSum(
        parseArrayInt(
          "[18,43,36,13,7]"
        )
      )
      .pipe(pprintln(_))
    Solution
      .maximumSum(
        parseArrayInt(
          "[10,12,19,14]"
        )
      )
      .pipe(pprintln(_))
  }

}
