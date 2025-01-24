package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object CountServersthatCommunicate1267 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def countServers(grid: Array[Array[Int]]): Int = {
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

      extension (p: Pos) {
        def isServer: Boolean = grid(p) != 0
      }

      var total = 0
      var ans = 0
      {
        var r = 0
        while (r < rows) {
          // println(s"On row $r")
          var foundfirst: Option[Pos] = None
          var ansContr = 0
          var c = 0
          while (c < cols) {
            val pos = (r, c)
            // println(s"Checking pos $pos our of $rows, $cols")
            if (pos.isServer) {
              total += 1
              foundfirst match {
                case Some(p) =>
                  grid(p) = 1
                  ansContr = 0
                case None =>
                  ansContr = 1
                  foundfirst = Some((r, c))
                  grid((r, c)) = 2
              }
            }
            c += 1
          }
          ans += ansContr
          r += 1
        }
      }
      // println(ans)
      {
        var c = 0
        while (c < cols) {
          var seen = 0
          var ansContr = 0
          var r = 0
          while (r < rows) {
            val pos = (r, c)
            grid(pos) match {
              case 2 =>
                ansContr += 1
                seen += 1
              case 1 =>
                seen += 1
              case _ =>
            }
            r += 1
          }
          if (seen >= 2)
            ans -= ansContr
          c += 1
        }
      }
      total - ans
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def countServers(grid: Array[Array[Int]]): Int = {
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

        extension (p: Pos) {
          def isServer: Boolean = grid(p) != 0
        }

        def soloInds(posIt: Iterable[Pos]): Option[Pos] =
          posIt
            .foldLeft((0, None: Option[Pos]))({ case ((found, foundAt), pos) =>
              if (pos.isServer) {
                (found, foundAt) match {
                  case (0, _) => (1, Some(pos))
                  case (1, _) => (2, None)
                  case x      => x
                }
              } else
                (found, foundAt)
            })
            ._2
        (0 until rows).flatMap(r => {
          soloInds((0 until cols).map((r, _)))
        })
        ???
      }
    }
  }
  def main(args: Array[String]): Unit = {

    Solution
      .countServers(
        parseArrayArrayInt(
          "[[1,1,0,0],[0,0,1,0],[0,0,1,0],[0,0,0,1]]"
        )
      )
      .pipe(println)
  }

}
