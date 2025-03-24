package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object CountDaysWithoutMeetings3169 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def countDays(days: Int, meetings: Array[Array[Int]]): Int = {
      import scala.collection.mutable
      val tm = mutable.TreeMap[Int, Int]()
      tm.put(days + 1, 0)
      meetings.foreach({ case Array(from, to) =>
        tm.put(from, tm.getOrElse(from, 0) + 1)
        tm.put(to, tm.getOrElse(to, 0) - 1)
      })
      tm.foldLeft((0, 0, 1))({
        case ((total, currentSum, previousDay), (day, change)) =>
          val newtotal = if (currentSum == 0) {
            total + day - previousDay
          } else total
          (newtotal, currentSum + change, day + 1)
      })._1
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .countDays(
        10,
        parseArrayArrayInt(
          "[[5,7],[1,3],[9,10]]"
        )
      )
      .pipe(pprintln(_))
    Solution
      .countDays(
        5,
        parseArrayArrayInt(
          "[[2,4],[1,3]]"
        )
      )
      .pipe(pprintln(_))
    Solution
      .countDays(
        6,
        parseArrayArrayInt(
          "[[1,6]]"
        )
      )
      .pipe(pprintln(_))
  }

}
