package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object FindtheNumberofDistinctColorsAmongtheBalls3160 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def queryResults(limit: Int, queries: Array[Array[Int]]): Array[Int] = {
      import scala.collection.mutable
      val color_ = mutable.Map[Int, Int]()
      extension (b: Int) {
        def color: Int = color_.getOrElse(b, 0)
        def color_=(c: Int): Unit = color_(b) = c
      }
      val colorCount_ = mutable.Map[Int, Int]()
      var distinctColors = 0
      def addColor(c: Int): Unit = {
        if (!colorCount_.contains(c))
          colorCount_(c) = 0
        if (colorCount_(c) == 0)
          distinctColors += 1
        colorCount_(c) += 1
      }
      def removeColor(c: Int): Unit = {
        if (colorCount_(c) == 1)
          distinctColors -= 1
        colorCount_(c) -= 1
      }

      val ans = new Array[Int](queries.length)
      var i = 0
      while (i < queries.length) {
        val Array(ball, toColor) = queries(i)
        if (ball.color != 0) {
          removeColor(ball.color)
        }
        ball.color = toColor
        addColor(toColor)
        ans(i) = distinctColors
        i += 1
      }
      ans
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .queryResults(
        4,
        parseArrayArrayInt(
          "[[1,4],[2,5],[1,3],[3,4]]"
        )
      )
      .pipe(pprintln(_))
    Solution
      .queryResults(
        4,
        parseArrayArrayInt(
          "[[0,1],[1,2],[2,2],[3,4],[4,5]]"
        )
      )
      .pipe(pprintln(_))
  }

}
