package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object FirstCompletelyPaintedRoworColumn2661 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def firstCompleteIndex(arr: Array[Int], mat: Array[Array[Int]]): Int = {
      val rows = mat.length
      val cols = mat.head.length
      val lookup = new Array[(Int, Int)](rows * cols + 1)
      for (r <- 0 until rows; c <- 0 until cols) {
        lookup(mat(r)(c)) = (r, c)
      }

      val colSums = new Array[Int](cols)
      val rowSums = new Array[Int](rows)

      var i = 0
      while (i < arr.length) {
        val (ri, ci) = lookup(arr(i))
        colSums(ci) += 1
        rowSums(ri) += 1
        if (colSums(ci) == rows && rowSums(ri) == cols)
          return i
        i += 1
      }
      -1
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .firstCompleteIndex(
        parseArrayInt(
          "[1,3,4,2]"
        ),
        parseArrayArrayInt(
          "[[1,4],[2,3]]"
        )
      )
      .pipe(println)
  }

}
