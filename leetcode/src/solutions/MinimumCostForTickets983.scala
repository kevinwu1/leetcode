package solutions

import leetcode.macros.Macros.logged
import solutions.Util._

import scala.util.chaining._

object MinimumCostForTickets983 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def mincostTickets(days: Array[Int], costs: Array[Int]): Int = {
      // @logged
      import scala.collection.mutable
      val memo = mutable.Map[(Int, Int), Int]()
      def getCost(d: Int, from: Int): Int = memo.getOrElseUpdate(
        (d, from), {
          if (from == days.size)
            0
          else if (d < days(from))
            getCost(days(from), from)
          else if (days(from) < d)
            getCost(d, from + 1)
          else {
            val c1 = costs(0) + getCost(d + 1, from + 1)
            val c2 = costs(1) + getCost(d + 7, from + 1)
            val c3 = costs(2) + getCost(d + 30, from + 1)
            c1 min c2 min c3
          }
        }
      )
      getCost(0, 0)
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .mincostTickets(
        parseArrayInt(
          "[6,9,10,14,15,16,17,18,20,22,23,24,29,30,31,33,35,37,38,40,41,46,47,51,54,57,59,65,70,76,77,81,85,87,90,91,93,94,95,97,98,100,103,104,105,106,107,111,112,113,114,116,117,118,120,124,128,129,135,137,139,145,146,151,152,153,157,165,166,173,174,179,181,182,185,187,188,190,191,192,195,196,204,205,206,208,210,214,218,219,221,225,229,231,233,235,239,240,245,247,249,251,252,258,261,263,268,270,273,274,275,276,280,283,285,286,288,289,290,291,292,293,296,298,299,301,303,307,313,314,319,323,325,327,329,334,339,340,341,342,344,346,349,352,354,355,356,357,358,359,363,364]"
        ),
        parseArrayInt(
          "[21,115,345]"
        )
      )
      .pipe(println)
  }

}
