package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object LargestDivisibleSubset368 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    import scala.util.Sorting
    var doLog = false
    def println(a: Any): Unit =
      if (doLog)
        Console.out.println(a)
    def largestDivisibleSubset(nums: Array[Int]): List[Int] = {
      val n = nums.length
      Sorting.quickSort(nums)
      val dp = new Array[Int](n)
      val bt = Array.fill[Int](n)(-1)

      var i = n - 1
      while (i >= 0) {
        val ni = nums(i)
        dp(i) = 1
        var j = i + 1
        while (j < n) {
          if (nums(j) % ni == 0) {
            val temp = 1 + dp(j)
            if (temp > dp(i)) {
              dp(i) = temp
              bt(i) = j
            }
          }
          j += 1
        }
        i -= 1
      }

      // pprintln(nums)
      // pprintln(dp)
      // pprintln(bt)

      var mi = {
        var i = 0
        var mi = 0
        var mv = 0
        while (i < n) {
          if (dp(i) > mv) {
            mv = dp(i)
            mi = i
          }
          i += 1
        }
        mi
      }

      var ans = List[Int]()
      ans = nums(mi) :: ans
      while (bt(mi) != -1) {
        mi = bt(mi)
        ans = nums(mi) :: ans
      }
      ans.toList
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  object Sol2 {
    object Solution {
      import scala.util.Sorting
      var doLog = false
      def println(a: Any): Unit =
        if (doLog)
          Console.out.println(a)
      val mask = (1 << 16) - 1
      def largestDivisibleSubset(nums: Array[Int]): List[Int] = {
        val n = nums.length
        Sorting.quickSort(nums)
        val dp = new Array[Int](n)
        var i = n - 1
        while (i >= 0) {
          val ni = nums(i)
          dp(i) = 1
          var j = i + 1
          while (j < n) {
            if (nums(j) % ni == 0) {
              val temp = 1 + dp(j) & mask
              if (temp > (dp(i) & mask)) {
                dp(i) = temp ^ (j << 16)
              }
            }
            j += 1
          }
          i -= 1
        }

        var mi = {
          var i = 0
          var mi = 0
          var mv = 0
          while (i < n) {
            val dpi = dp(i) & mask
            if (dpi > mv) {
              mv = dpi
              mi = i
            }
            i += 1
          }
          mi
        }

        // pprintln(nums)
        // pprintln(dp.map(_ & mask))
        // pprintln(dp.map(_ >> 16))
        // pprintln(mi)

        var ans = List[Int]()
        if (mi == 0) {
          ans = nums(mi) :: ans
          mi = dp(mi) >> 16
        }
        while (mi != 0) {
          ans = nums(mi) :: ans
          mi = dp(mi) >> 16
        }
        ans.toList
      }
    }
  }

  def main(args: Array[String]): Unit = {
    Solution.doLog = true
    Sol2.Solution
      .largestDivisibleSubset(
        parseArrayInt(
          "[1,2,3]"
        )
      )
      .pipe(pprintln(_))
    // Solution
    //   .largestDivisibleSubset(
    //     parseArrayInt(
    //       "[1,2,4,8]"
    //     )
    //   )
    //   .pipe(pprintln(_))
    // Solution
    //   .largestDivisibleSubset(
    //     parseArrayInt(
    //       "[1,2,3,4,6,24]"
    //     )
    //   )
    //   .pipe(pprintln(_))
    // Sol2.Solution
    //   .largestDivisibleSubset(
    //     parseArrayInt(
    //       "[3,4,16,8]"
    //     )
    //   )
    //   .pipe(pprintln(_))

  }

}
