package solutions

import leetcode.macros.Macros.logged
import os.size
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MakingALargeIsland827 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def largestIsland(grid: Array[Array[Int]]): Int = {

      def log2(i: Int): Int = {
        var ans = 1
        while (1 << (ans) <= i)
          ans += 1
        ans
      }
      val rows = grid.length
      val cols = grid.head.length
      val sizebits = log2(rows * cols)
      val rbits = log2(rows)
      val cbits = log2(cols)

      assert(sizebits + rbits + cbits < 32)
      extension (i: Int) {
        def toSizeBits: Int = i
        def toRowBits: Int = i << (sizebits)
        def toColBits: Int = i << (rbits + sizebits)
        def getSizeBits = i & ((1 << sizebits) - 1)
        def getRowBits = (i & ((1 << (rbits + sizebits)) - 1)) >> sizebits
        def getColBits =
          (i & ((1 << (cbits + rbits + sizebits)) - 1)) >> (rbits + sizebits)
      }
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
        def linear: Int = r.toRowBits | c.toColBits
        def gridValue: Int = grid(r)(c)
      }
      val down = (1, 0)
      val right = (0, 1)
      val up = -down
      val left = -right
      val directions = List(down, right, up, left)
      extension (p: Pos) {
        def neighbors: List[Pos] = directions.map(p + _).filter(_.isInBounds)
        def neighbors2: List[Pos] = List(right, down).filter(_.isInBounds)
      }

      extension (arr: Array[Array[Int]]) {
        def apply(p: Pos): Int = arr(p.r)(p.c)
        def update(p: Pos, value: Int): Unit = arr(p.r)(p.c) = value
      }

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
        def find(a: Pos): Int = find(a.linear)
        def union(a: Pos, b: Pos): Int = union(a.linear, b.linear)
      }

      val n = rows * cols
      val uf = new UnionFind(n)
      for (
        r <- 0 until rows;
        c <- 0 until cols;
        pos = (r, c)
        if pos.gridValue == 1
      ) pos.neighbors.filter(_.gridValue == 1).foreach(n => uf.union(pos, n))

      var found = false
      var best = 0
      for (
        r <- 0 until rows;
        c <- 0 until cols;
        pos = (r, c)
        if pos.gridValue == 0
      ) {
        found = true
        best = Math.max(
          best,
          pos.neighbors
            .filter(_.gridValue == 1)
            .map(uf.find)
            .distinct
            .map(uf.size)
            .sum + 1
        )
      }
      if (found) best else n
    }
  }

  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .largestIsland(
        parseArrayArrayInt(
          "[[1,0],[0,1]]"
        )
      )
      .pipe(println)
    Solution
      .largestIsland(
        parseArrayArrayInt(
          "[[1,1],[1,0]]"
        )
      )
      .pipe(println)
    Solution
      .largestIsland(
        parseArrayArrayInt(
          "[[1,1],[1,1]]"
        )
      )
      .pipe(println)
  }

}
