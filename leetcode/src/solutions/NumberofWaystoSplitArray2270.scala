package solutions

import leetcode.macros.Macros.logged
import solutions.Util._

import scala.util.chaining._

object NumberofWaystoSplitArray2270 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def waysToSplitArray(nums: Array[Int]): Int = {
      val total = nums.map(_.toLong).sum
      nums.view
        .slice(0, nums.length - 1)
        .foldLeft((0, 0L))({ case ((count, psum), item) =>
          val newPsum = psum + item
          if (newPsum >= total - newPsum)
            (count + 1, newPsum)
          else
            (count, newPsum)
        })
        ._1
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {
    Solution
      .waysToSplitArray(
        parseArrayInt(
          "[10,4,-8,7]"
        )
      )
      .pipe(println)
  }

}
