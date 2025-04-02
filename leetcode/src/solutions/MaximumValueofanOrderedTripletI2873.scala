package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MaximumValueofanOrderedTripletI2873 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    var doLog = false
    def println(a: Any): Unit =
      if (doLog)
        Console.out.println(a)
    def maximumTripletValue(nums: Array[Int]): Long = {
      nums.indices
        .combinations(3)
        .map(_.map(nums(_).toLong).toArray)
        .map({ case Array(i, j, k) => k * (i - j) })
        .max max 0L
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {

      def maximumTripletValue(nums: Array[Int]): Long = {
        val n = nums.length
        var best = 0L
        var bi = 0
        var bj = Int.MaxValue
        var bk = 0
        var i = 0
        while (i < n - 2) {
          val ni = nums(i)
          if (ni >= bi) {
            var j = i + 1
            while (j < n - 1) {
              val nj = nums(j)
              if (ni > bi || nj <= bj) {
                var k = j + 1
                while (k < n) {
                  val nk = nums(k)
                  if (ni > bi || nj < bj || nk > bk) {
                    val v = nk.toLong * (ni.toLong - nj.toLong)
                    if (v > best) {
                      best = v
                      bi = ni
                      bj = nj
                      bk = nk
                    }
                  }
                  k += 1
                }
              }
              j += 1
            }
          }
          i += 1
        }
        best
      }
    }
  }

  def main(args: Array[String]): Unit = {
    Solution.doLog = true
    Sol2.Solution
      .maximumTripletValue(
        parseArrayInt(
          "[12,6,1,2,7]"
        )
      )
      .pipe(pprintln(_))
  }

}
