package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MinimumCostWalkinWeightedGraph3108 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def minimumCost(
        n: Int,
        edges: Array[Array[Int]],
        query: Array[Array[Int]]
    ): Array[Int] = {
      class UnionFind(n: Int) {
        val rep_ = (0 until n).toArray
        val size = Array.fill(n)(1)
        val ands = Array.fill(n)(Integer.MAX_VALUE)

        def union(a: Int, b: Int, w: Int): Int = {
          val ra = find(a)
          val rb = find(b)
          if (ra != rb) {
            val sizea = size(ra)
            val sizeb = size(rb)
            val (from, to) = if (sizea > sizeb) {
              (rb, ra)
            } else (ra, rb)
            size(to) += size(from)
            rep_(from) = to
            ands(to) &= ands(from)
            ands(to) &= w
            to
          } else {
            ands(ra) &= w
            ra
          }
        }
        def find(a: Int): Int = {
          val r = rep_(a)
          if (a == r)
            r
          else {
            rep_(a) = find(r)
            rep_(a)
          }
        }
      }

      val uf = new UnionFind(n)
      edges.foreach({ case Array(f, t, w) =>
        uf.union(f, t, w)
      })
      query.map({ case Array(f, t) =>
        val fr = uf.find(f)
        val tr = uf.find(t)
        if (fr != tr) -1
        else uf.ands(fr)
      })
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .minimumCost(
        5,
        parseArrayArrayInt(
          "[[0,1,7],[1,3,7],[1,2,1]]"
        ),
        parseArrayArrayInt(
          "[[0,3],[3,4]]"
        )
      )
      .pipe(pprintln(_))
    Solution
      .minimumCost(
        3,
        parseArrayArrayInt(
          "[[0,2,7],[0,1,15],[1,2,6],[1,2,1]]"
        ),
        parseArrayArrayInt(
          "[[1,2]]"
        )
      )
      .pipe(pprintln(_))
  }

}
