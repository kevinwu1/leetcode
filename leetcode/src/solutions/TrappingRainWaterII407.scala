package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.collection.mutable.PriorityQueue
import scala.util.chaining.*

@scala.annotation.experimental
object TrappingRainWaterII407 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def trapRainWater(heightMap: Array[Array[Int]]): Int = {
      println("Start")
      val rows = heightMap.length
      val cols = heightMap.head.length
      // println(rows + ", " + cols)
      val canExitArr = Array.fill(rows, cols)(0)

      type Pos = (Int, Int)
      extension (p: Pos) {
        def r: Int = p._1
        def c: Int = p._2
        def +(o: Pos): Pos = (r + o.r, c + o.c)
        def unary_- : Pos = (-r, -c)
        def -(o: Pos): Pos = p + -o
        def height: Int = heightMap(r)(c)
        def isInBounds: Boolean =
          0 <= r && r < rows &&
            0 <= c && c < cols
      }
      val down = (1, 0)
      val right = (0, 1)
      val up = -down
      val left = -right
      val directions = List(down, right, up, left)
      extension (p: Pos) {
        def neighbors: List[Pos] = directions.map(p + _).filter(_.isInBounds)
      }
      import scala.collection.mutable
      val pq = new mutable.PriorityQueue[Pos]()(
        Ordering.fromLessThan[Pos]((p1, p2) => p1.height < p2.height).reverse
      )
      for (r <- 0 until rows) {
        pq += ((r, 0))
        pq += ((r, cols - 1))
      }
      for (c <- 0 until cols) {
        pq += ((0, c))
        pq += ((rows - 1, c))
      }
      val seen = mutable.Set[Pos]()
      var waterLevel = 0
      var ans = 0
      while (pq.nonEmpty) {
        val pos = pq.dequeue()
        if (!seen.contains(pos)) {
          seen += pos
          if (pos.height > waterLevel)
            waterLevel = pos.height
          else {
            ans += waterLevel - pos.height
          }
          pos.neighbors
            .filter(!seen.contains(_))
            .foreach(n => {
              pq += n
            })
        }
      }
      ans
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {
    Solution
      .trapRainWater(
        parseArrayArrayInt(
          "[[3,3,3,3,3],[3,2,2,2,3],[3,2,1,2,3],[3,2,2,2,3],[3,3,3,3,3]]"
          // os.read(os.pwd / "input.txt")
          // "[[14,17,18,16,14,16],[17,3,10,2,3,8],[11,10,4,7,1,7],[13,7,2,9,8,10],[13,1,3,4,8,6],[20,3,3,9,10,8]]"
        )
      )
      .pipe(println)
  }

}
