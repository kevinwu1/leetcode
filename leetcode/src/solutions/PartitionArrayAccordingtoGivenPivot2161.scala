package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object PartitionArrayAccordingtoGivenPivot2161 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def pivotArray(nums: Array[Int], pivot: Int): Array[Int] = {
      val (less, other) = nums.partition(_ < pivot)
      val (equal, higher) = other.partition(_ == pivot)
      less ++ equal ++ higher
    }
  }

  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .pivotArray(
        parseArrayInt(
          "[9,12,5,10,14,3,10]"
        ),
        10
      )
      .pipe(pprintln(_))
  }

}
