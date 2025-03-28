package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*
@scala.annotation.experimental
object MaximumNumberofPointsFromGridQueries2503 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {

    import scala.util.chaining.*
    import scala.collection.Searching.Found
    import scala.collection.Searching.InsertionPoint
    def maxPoints(grid: Array[Array[Int]], queries: Array[Int]): Array[Int] = {

      val rows = grid.length
      val cols = grid.head.length

      type Pos = (Int, Int)
      extension (p: Pos) {
        def r: Int = p._1
        def c: Int = p._2
        def +(o: Pos): Pos = (r + o.r, c + o.c)
        def unary_- : Pos = (-r, -c)
        def -(o: Pos): Pos = p + -o
        def isInBounds: Boolean =
          0 <= r && r < rows &&
            0 <= c && c < cols
        def linear: Int = r * cols + c
      }
      val down = (1, 0)
      val right = (0, 1)
      val up = -down
      val left = -right
      val directions = List(down, right, up, left)
      extension (p: Pos) {
        def neighbors: List[Pos] = directions.map(p + _).filter(_.isInBounds)
      }

      extension (arr: Array[Array[Int]]) {
        def apply(p: Pos): Int = arr(p.r)(p.c)
        def update(p: Pos, value: Int): Unit = arr(p.r)(p.c) = value
      }
      import scala.collection.mutable.TreeMap
      val tm = TreeMap[Int, Vector[Pos]]()

      class UnionFind(n: Int) {
        val rep_ = (0 until n).toArray
        val size = Array.fill(n)(1)

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
            rep_(from) = to
            to
          } else {
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
      val uf = new UnionFind(rows * cols)
      for (r <- 0 until rows; c <- 0 until cols) {
        val pos = (r, c)
        val value = grid(pos)
        tm.put(value, tm.getOrElse(value, Vector()) :+ pos)
      }
      val results = tm
        .map({ case (k, v) =>
          // println(s"k is ${k}")
          v.foreach(pos =>
            // println(s"mergin $pos")
            pos.neighbors
              .filter(grid(_) <= k)
              // .tap(println)
              .foreach(otherpos => uf.union(otherpos.linear, pos.linear))
          )
          k * 2 -> (
            if (grid((0, 0)) > k)
              0
            else
              (0, 0).linear.pipe(uf.find).pipe(uf.size)
          )
        })
        .toVector
      // println(results)
      queries.map(i => {
        results.search((i * 2 - 1, 0))(
          Ordering.fromLessThan((a, b) => a._1 < b._1)
        ) match {
          case InsertionPoint(i) =>
            if (i == 0) 0
            else
              results(i - 1)._2
          case Found(_) => ???
        }
      })
    }
  }

  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .maxPoints(
        parseArrayArrayInt(
          "[[1,2,3],[2,5,7],[3,5,1]]"
        ),
        parseArrayInt(
          "[5,6,2]"
        )
      )
      .pipe(pprintln(_))

    Solution
      .maxPoints(
        parseArrayArrayInt(
          "[[5,2,1],[1,1,2]]"
        ),
        parseArrayInt(
          "[3]"
        )
      )
      .pipe(pprintln(_))
  }

}
