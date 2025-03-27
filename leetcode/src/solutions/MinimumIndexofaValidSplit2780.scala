package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MinimumIndexofaValidSplit2780 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def minimumIndex(nums: List[Int]): Int = {
      val (dominant, times) =
        nums.groupMapReduce(identity)(_ => 1)(_ + _).maxBy(_._2)
      val n = nums.size
      @scala.annotation.tailrec
      def firstDominantSplit(
          ind: Int,
          timesBefore: Int,
          remaining: List[Int]
      ): Int = {
        if (ind == n)
          -1
        else {
          if (timesBefore > ind / 2 && (times - timesBefore) > (n - ind) / 2)
            ind - 1
          else
            firstDominantSplit(
              ind + 1,
              timesBefore + (if (remaining.head == dominant) 1 else 0),
              remaining.tail
            )
        }
      }
      firstDominantSplit(1, if (nums.head == dominant) 1 else 0, nums.tail)
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .minimumIndex(
        parseArrayInt(
          "[1,2,2,2]"
        ).toList
      )
      .pipe(pprintln(_))
    Solution
      .minimumIndex(
        parseArrayInt(
          "[2,1,3,1,1,1,7,1,2,1]"
        ).toList
      )
      .pipe(pprintln(_))
    Solution
      .minimumIndex(
        parseArrayInt(
          "[3,3,3,3,7,2,2]"
        ).toList
      )
      .pipe(pprintln(_))
  }

}
