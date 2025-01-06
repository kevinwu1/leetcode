package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object ShiftingLettersII2381 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def shiftingLetters(s: String, shifts: Array[Array[Int]]): String = {
      import scala.collection.mutable
      import scala.collection.mutable.PriorityQueue
      val pq = PriorityQueue[(Int, Int)]()(
        Ordering.fromLessThan[(Int, Int)]((t1, t2) => t1._1 < t2._1).reverse
      )
      shifts.foreach({
        case Array(s, e, dir) => {
          val shiftDir = if dir == 1 then 1 else -1
          pq += ((s, shiftDir))
          pq += ((e + 1, -shiftDir))
        }
      })
      // println(pq)
      val sb = new mutable.StringBuilder
      s.zipWithIndex.foldLeft(0)({ case (shiftAmt, (c, ind)) =>
        var newShiftAmt = {
          var ans = shiftAmt
          while (pq.nonEmpty && pq.head._1 == ind) {
            ans += pq.head._2
            // println("deqing")
            // println(pq.head)
            pq.dequeue()
          }
          ans % 26
        }
        val newchr = {
          val nc = (c + newShiftAmt).toChar
          if (nc < 'a')
            (nc + 26).toChar
          else if (nc > 'z')
            (nc - 26).toChar
          else nc
        }
        sb += newchr
        newShiftAmt
      })
      sb.toString
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def shiftingLetters(s: String, shifts: Array[Array[Int]]): String = {
        val shfts = Array.fill(s.length() + 1)(0)
        shifts.foreach({
          case Array(s, e, dir) => {
            val shiftDir = if dir == 1 then 1 else -1
            shfts(s) = shfts(s) + shiftDir
            shfts(e + 1) = shfts(e + 1) - shiftDir
          }
        })
        // println(pq)
        import scala.collection.mutable
        val ans = s.toCharArray().clone()
        var accum = 0
        for (i <- 0 until s.length()) {
          ans(i) =
            (((accum + shfts(i)) % 26 + 26 + ans(i) - 'a') % 26 + 'a').toChar
          accum += shfts(i)
        }
        new String(ans)
      }
    }
  }
  def main(args: Array[String]): Unit = {
    Sol2.Solution
      // .shiftingLetters(
      //   "abc",
      //   parseArrayArrayInt(
      //     "[[0,1,0],[1,2,1],[0,2,1]]"
      //   )
      // )
      .shiftingLetters(
        "dztz",
        parseArrayArrayInt(
          "[[0,0,0],[1,1,1]]"
        )
      )
      .pipe(println)
  }

}
