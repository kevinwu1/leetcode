package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object CheckifGridcanbeCutintoSections3394 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def checkValidCuts(n: Int, rectangles: Array[Array[Int]]): Boolean = {
      def isPossible(
          start: Array[Int] => Int,
          end: Array[Int] => Int
      ): Boolean = {
        var total = 0
        var previousEnd = end(rectangles(0))
        var i = 1
        while (i < rectangles.length) {
          val nextRect = rectangles(i)
          val ns = start(nextRect)
          val ne = end(nextRect)
          if (previousEnd > ns)
            previousEnd = Math.max(previousEnd, ne)
          else {
            total += 1
            previousEnd = ne
            if (total == 2)
              return true
          }
          i += 1
        }
        return false
      }
      {
        rectangles.sortInPlaceBy(_(0))
        isPossible(_(0), _(2))
      } || {
        rectangles.sortInPlaceBy(_(1))
        isPossible(_(1), _(3))
      }
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def checkValidCuts(n: Int, rectangles: Array[Array[Int]]): Boolean = {
        var total = 0
        rectangles.sortInPlaceBy(_(0))
        var previousEnd = rectangles(0)(2)
        var i = 1
        while (i < rectangles.length) {
          val nextRect = rectangles(i)
          val ns = nextRect(0)
          val ne = nextRect(2)
          if (previousEnd > ns)
            previousEnd = Math.max(previousEnd, ne)
          else {
            total += 1
            previousEnd = ne
            if (total == 2)
              return true
          }
          i += 1
        }
        total = 0
        rectangles.sortInPlaceBy(_(1))
        {
          var previousEnd = rectangles(0)(3)
          var i = 1
          while (i < rectangles.length) {
            val nextRect = rectangles(i)
            val ns = nextRect(1)
            val ne = nextRect(3)
            if (previousEnd > ns)
              previousEnd = Math.max(previousEnd, ne)
            else {
              total += 1
              previousEnd = ne
              if (total == 2)
                return true
            }
            i += 1
          }
          return false
        }
      }
    }
  }
  def main(args: Array[String]): Unit = {

    Solution
      .checkValidCuts(
        5,
        parseArrayArrayInt(
          "[[1,0,5,2],[0,2,2,4],[3,2,5,3],[0,4,4,5]]"
        )
      )
      .pipe(pprintln(_))
    Solution
      .checkValidCuts(
        4,
        parseArrayArrayInt(
          "[[0,0,1,1],[2,0,3,4],[0,2,2,3],[3,0,4,3]]"
        )
      )
      .pipe(pprintln(_))
    Solution
      .checkValidCuts(
        4,
        parseArrayArrayInt(
          "[[0,2,2,4],[1,0,3,2],[2,2,3,4],[3,0,4,2],[3,2,4,4]]"
        )
      )
      .pipe(pprintln(_))
  }

}
