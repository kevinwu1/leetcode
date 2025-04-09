package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MinimumOperationstoMakeArrayValuesEqualtoK3375 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    import scala.collection.mutable.BitSet
    var doLog = false
    def println(a: Any): Unit =
      if (doLog)
        Console.out.println(a)
    def minOperations(nums: Array[Int], k: Int): Int = {
      val bs = new BitSet()
      var total = 0
      var i = 0
      while (i < nums.length) {
        if (nums(i) < k)
          return -1
        if (nums(i) > k) {
          if (!bs.contains(nums(i)))
            total += 1
          bs += nums(i)
        }
        i += 1
      }

      total
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {
    Solution.doLog = true
    Solution
      .minOperations(
        parseArrayInt(
          "[5,2,5,4,5]"
        ),
        2
      )
      .pipe(pprintln(_))
  }

}
