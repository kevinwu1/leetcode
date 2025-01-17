package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object BitwiseXORofAllPairings2425 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def xorAllNums(nums1: Array[Int], nums2: Array[Int]): Int = {
      lazy val xor1 = nums1.reduce(_ ^ _)
      lazy val xor2 = nums2.reduce(_ ^ _)
      (if (nums1.size % 2 != 0) xor2 else 0) ^
        (if (nums2.size % 2 != 0) xor1 else 0)
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def xorAllNums(nums1: Array[Int], nums2: Array[Int]): Int = {
        var answer = 0
        if (nums1.length % 2 == 1) {
          var i = 0
          while (i < nums2.length) {
            answer ^= nums2(i)
            i += 1
          }
        }
        if (nums2.length % 2 == 1) {
          var i = 0
          while (i < nums1.length) {
            answer ^= nums1(i)
            i += 1
          }
        }
        answer
      }
    }
  }
  def main(args: Array[String]): Unit = {

    Solution
      .xorAllNums(
        parseArrayInt(
          "[2,1,3]"
        ),
        parseArrayInt(
          "[10,2,5,0]"
        )
      )
      .pipe(println)
  }

}
