package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MergeTwo2DArraysbySummingValues2570 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def mergeArrays(
        nums1: Array[Array[Int]],
        nums2: Array[Array[Int]]
    ): Array[Array[Int]] = {
      import scala.collection.mutable
      val tm = mutable.TreeMap[Int, Int]().withDefaultValue(0)
      (nums1 ++ nums2).foreach({ case Array(id, value) =>
        tm(id) += value
      })
      tm.toArray.map({ case (a, b) => Array(a, b) })
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .mergeArrays(
        parseArrayArrayInt(
          "[[1,2],[2,3],[4,5]]"
        ),
        parseArrayArrayInt(
          "[[1,4],[3,2],[4,1]]"
        )
      )
      .pipe(pprintln(_))
    Solution
      .mergeArrays(
        parseArrayArrayInt(
          "[[2,4],[3,6],[5,5]]"
        ),
        parseArrayArrayInt(
          "[[1,3],[4,3]]"
        )
      )
      .pipe(pprintln(_))
  }

}
