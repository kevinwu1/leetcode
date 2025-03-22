package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object CounttheNumberofCompleteComponents2685 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def countCompleteComponents(n: Int, edges: Array[Array[Int]]): Int = {
      class UnionFind(n: Int) {
        val rep_ = (0 until n).toArray
        val size = Array.fill(n)(0)

        def union(a: Int, b: Int): Int = {
          val ra = find(a)
          val rb = find(b)
          if (ra != rb) {
            val sizea = size(ra)
            val sizeb = size(rb)
            val (from, to) = if (sizea > sizeb) {
              (rb, ra)
            } else (ra, rb)
            size(to) += size(from)
            size(to) += 1
            rep_(from) = to
            to
          } else {
            size(ra) += 1
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
      edges.foreach({ case Array(a, b) =>
        uf.union(a, b)
      })
      (0 until n)
        .map(uf.find)
        .groupBy(identity)
        .count({ case (rep, els) =>
          println((rep, els.size, uf.size(rep)))
          els.size * (els.size - 1) / 2 == uf.size(rep)
        })
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def countCompleteComponents(n: Int, edges: Array[Array[Int]]): Int = {
        val rep_ = new Array[Int](n)
        var h = 0
        while (h < n) {
          rep_(h) = h
          h += 1
        }
        val size = new Array[Int](n)

        def union(a: Int, b: Int): Int = {
          val ra = find(a)
          val rb = find(b)
          if (ra != rb) {
            val sizea = size(ra)
            val sizeb = size(rb)
            val (from, to) = if (sizea > sizeb) {
              (rb, ra)
            } else (ra, rb)
            size(to) += size(from)
            size(to) += 1
            rep_(from) = to
            to
          } else {
            size(ra) += 1
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
        var i = 0
        while (i < edges.length) {
          union(edges(i)(0), edges(i)(1))
          i += 1
        }

        val counts = new Array[Int](n)
        var j = 0
        while (j < n) {
          val rep = find(j)
          counts(rep) += 1
          j += 1
        }

        var total = 0
        var k = 0
        while (k < n) {
          if (counts(k) != 0)
            if (counts(k) * (counts(k) - 1) / 2 == size(k))
              total += 1
          k += 1
        }
        total
      }
    }
  }
  def main(args: Array[String]): Unit = {
    Sol2.Solution
      .countCompleteComponents(
        6,
        parseArrayArrayInt(
          "[[0,1],[0,2],[1,2],[3,4]]"
        )
      )
      .pipe(pprintln(_))
  }

}
