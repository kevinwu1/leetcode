package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MinimumNumberofOperationstoMoveAllBallstoEachBox1769 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def minOperations(boxes: String): Array[Int] = {
      val psum = boxes.scanLeft((0, 0))({ case ((accum, num), c) =>
        if (c == '0')
          (accum + num, num)
        else
          (accum + num, num + 1)
      })
      val ssum = boxes.scanRight((0, 0))({ case (c, (accum, num)) =>
        if (c == '0')
          (accum + num, num)
        else
          (accum + num, num + 1)
      })
      println(psum)
      println(ssum)
      boxes.indices
        .map(ind => {
          psum(ind + 1)._1 + ssum(ind)._1
        })
        .toArray
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def minOperations(boxes: String): Array[Int] = {
        val n = boxes.length()
        val ans = Array.fill(n)(0)
        var (la, lnum) = (0, 0)
        for (i <- boxes.indices) {
          ans(i) = la
          if (boxes(i) == '1') {
            lnum += 1
          }
          la += lnum
        }
        var (ra, rnum) = (0, 0)
        for (i <- boxes.indices.reverse) {
          ans(i) += ra
          if (boxes(i) == '1') {
            rnum += 1
          }
          ra += rnum
        }
        ans
      }
    }
  }
  def main(args: Array[String]): Unit = {
    Sol2.Solution
      .minOperations(
        "110"
      )
      .mkString(",")
      .pipe(println)
  }

}
