package solutions

object ConstructStringWithRepeatLimit2182 {
  import scala.collection.mutable.Stack
  import scala.util.chaining._

  object Solution {
    def repeatLimitedString(s: String, repeatLimit: Int): String = {
      val charsWithCounts =
        s.groupBy(x => x)
          .view
          .mapValues(v => v.size)
          .toVector
          .sortBy(t => t._1)
          .reverse

      val st = new Stack[(Char, Int)]()
      st.addAll(charsWithCounts)

      val result = new StringBuilder()
      var debt = 0
      while (st.nonEmpty) {
        val (chr, cnt) = st.pop()
        var debt = Math.ceil(cnt.toDouble / repeatLimit).toInt - 1
        val extras =
          if (cnt % repeatLimit == 0) repeatLimit else cnt % repeatLimit

        while (debt > 0 && st.nonEmpty) {
          var (padchr, padcnt) = st.pop()
          if (padcnt > debt) {
            st.push((padchr, padcnt - debt))
            padcnt = debt
          }

          debt -= padcnt
          result ++= (chr.toString * repeatLimit + padchr) * padcnt
        }
        if (debt == 0)
          result ++= chr.toString * extras
        else
          result ++= chr.toString * repeatLimit
      }
      result.toString()
    }
  }

}
