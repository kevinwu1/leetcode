package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MaximumValueofanOrderedTripletII2874 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    var doLog = false
    def println(a: Any): Unit =
      if (doLog)
        Console.out.println(a)
    def maximumTripletValue(nums: Array[Int]): Long = {
      val n = nums.length
      val maxBefore = nums.scanLeft(0)(Math.max)
      val maxDiff =
        nums.indices.scanLeft[Long](Long.MinValue)((previousMaxDiff, index) =>
          Math.max(
            previousMaxDiff,
            Math.max(
              maxBefore(index) - nums(index),
              if (index == 0) Long.MinValue
              else (nums(index - 1) - nums(index)).toLong
            )
          )
        )
      // println(nums.mkString(", "))
      // println(maxBefore.mkString(", "))
      // println(maxDiff.mkString(", "))
      (2 until n).map(ind => nums(ind).toLong * maxDiff(ind)).max max 0
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      var doLog = false
      def println(a: Any): Unit =
        if (doLog)
          Console.out.println(a)
      def maximumTripletValue(nums: Array[Int]): Long = {
        val n = nums.length
        var index = 2
        var maxBefore: Long = nums(0) max nums(1)
        var maxDiff: Long = nums(0) - nums(1)
        var ans = 0L
        while (index < n) {
          val numi = nums(index)
          val numi1 = nums(index - 1)
          ans = ans max (numi.toLong * maxDiff)
          maxBefore = maxBefore max numi1
          maxDiff =
            maxDiff max maxBefore - numi max (nums(index - 1) - numi).toLong
          index += 1
        }
        // println(nums.mkString(", "))
        // println(maxBefore.mkString(", "))
        // println(maxDiff.mkString(", "))
        ans
      }
    }
  }
  def main(args: Array[String]): Unit = {
    Solution.doLog = true
    val input = os.read(os.pwd / "input.txt")
    println(input.length())
    println(input.slice(0, 100))
    Sol2.Solution
      .maximumTripletValue(
        parseArrayInt(
          input
        )
      )
      .pipe(pprintln(_))
    Solution
      .maximumTripletValue(
        parseArrayInt(
          "[12,6,1,2,7]"
        )
      )
      .pipe(pprintln(_))
    Solution
      .maximumTripletValue(
        parseArrayInt(
          "[2,3,1]"
        )
      )
      .pipe(pprintln(_))
    Solution
      .maximumTripletValue(
        parseArrayInt(
          "[1000000,1,1000000]"
        )
      )
      .pipe(pprintln(_))
  }

}
