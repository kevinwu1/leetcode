package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MinimumOperationstoExceedThresholdValueII3066 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def minOperations(nums: Array[Int], k: Int): Int = {
      val pq = scala.collection.mutable
        .PriorityQueue[Long](nums.map(_.toLong): _*)(
          Ordering.fromLessThan[Long](_ < _).reverse
        )
      var ans = 0
      while (pq.head < k) {
        val e1 = pq.dequeue()
        val e2 = pq.dequeue()
        pq += (e1 << 1) + e2
        ans += 1
      }
      ans
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .minOperations(
        parseArrayInt(
          "[2,11,10,1,3]"
        ),
        10
      )
      .pipe(pprintln(_))
    Solution
      .minOperations(
        parseArrayInt(
          "[1,1,2,4,9]"
        ),
        20
      )
      .pipe(pprintln(_))
  }

}
