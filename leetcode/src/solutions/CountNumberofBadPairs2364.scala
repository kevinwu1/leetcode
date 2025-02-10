package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object CountNumberofBadPairs2364 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def countBadPairs(nums: Array[Int]): Long = {
      val n = nums.length
      extension (i: Long) {
        def choose2: Long = i.toLong * (i - 1) / 2
      }
      val counts = nums.zipWithIndex
        .map({ case (i, ind) => (i - ind) -> 1L })
        .groupMapReduce(_._1)(_._2)(_ + _)
      n.choose2 - counts.map(_._2).filter(_ >= 2).map(_.choose2).sum
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def countBadPairs(nums: Array[Int]): Long = {
        val n = nums.length
        import scala.collection.mutable
        val counts = mutable.Map[Int, Int]()
        var total = n.toLong * (n.toLong - 1) / 2
        var i = 0
        while (i < n) {
          val key = nums(i) - i
          if (!counts.contains(key))
            counts(key) = 0
          if (counts(key) >= 1)
            total -= counts(key)
          counts(key) += 1
          i += 1
        }
        total
      }
    }
  }
  def main(args: Array[String]): Unit = {

    Sol2.Solution
      .countBadPairs(
        parseArrayInt(
          "[4,1,3,3]"
        )
      )
      .pipe(pprintln(_))
    Sol2.Solution
      .countBadPairs(
        parseArrayInt(
          "[1,2,3,4,5]"
        )
      )
      .pipe(pprintln(_))
  }

}
