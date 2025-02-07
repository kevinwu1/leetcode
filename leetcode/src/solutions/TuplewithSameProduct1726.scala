package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object TuplewithSameProduct1726 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def tupleSameProduct(nums: Array[Int]): Int = {
      import scala.collection.mutable
      val counts = mutable.Map[Int, Int]()
      for (
        i <- nums.indices;
        j <- i + 1 until nums.length
      ) {
        val prod = nums(i) * nums(j)
        if (!counts.contains(prod))
          counts(prod) = 1
        else counts(prod) += 1
      }
      4 * counts.values.map(n => n * (n - 1)).sum
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .tupleSameProduct(
        parseArrayInt(
          "[2,3,4,6]"
        )
      )
      .pipe(pprintln(_))
    Solution
      .tupleSameProduct(
        parseArrayInt(
          "[1,2,4,5,10]"
        )
      )
      .pipe(pprintln(_))
  }

}
