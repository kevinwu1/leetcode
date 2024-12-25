package solutions.contest.Contest429

import leetcode.macros.Macros.logged
import solutions.contest.Contest429.Problem3.Solution.minLength

import scala.util.chaining.scalaUtilChainingOps

object Problem3 {
  import scala.annotation.tailrec
  object Solution {
    def minLength(s: String, numOps: Int): Int = {
      import scala.collection.mutable.PriorityQueue
      val f01 = (s.zipWithIndex.count { case (c, i) =>
        c - '0' == (i % 2)
      })
      if (f01 <= numOps || (s.size - f01) <= numOps) {
        println("special")
        1
      } else {
        @logged
        def findLowestFalse(lo: Int, hi: Int, test: Int => Boolean): Int = {
          if (lo + 1 == hi)
            hi
          else {
            val mid = (lo + hi) / 2
            if (test(mid))
              findLowestFalse(mid, hi, test)
            else
              findLowestFalse(lo, mid, test)
          }
        }

        val (a, b, _) = s.tail
          .foldLeft((List[Int](), 1, s.head)) {
            case ((allcounts, curCount, prev), c) =>
              if (c == prev)
                (allcounts, curCount + 1, c)
              else
                (curCount :: allcounts, 1, c)
          }
        val counts = b :: a

        println(6)
        @logged
        def isPossible(i: Int): Boolean = {
          val r = counts
            .filter(_ > i)
            .map(x => {
              val num = (x - i)
              val den = (i + 1)
              val div = num / den
              if ((div * den) < num) div + 1 else div
            })
            .sum
          r <= numOps
        }
        findLowestFalse(1, s.size + 1, x => !isPossible(x))
      }
    }
  }

  def main(args: Array[String]): Unit = {
    minLength("00001", 1)
      .pipe(println)
  }
}
