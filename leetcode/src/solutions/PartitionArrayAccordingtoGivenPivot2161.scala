package solutions

import leetcode.macros.Macros.logged
import solutions.Util._
import pprint.pprintln
import scala.util.chaining._

@scala.annotation.experimental
object PartitionArrayAccordingtoGivenPivot2161 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def pivotArray(nums: Array[Int], pivot: Int): Array[Int] = {
      val (less, other) = nums.partition(_ < pivot)
      val (equal, higher) = nums.partition(_ == pivot)
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
