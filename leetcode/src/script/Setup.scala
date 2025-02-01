package script
object Setup {
  def main(args: Array[String]): Unit = {
    val name =
      (args.tail :+ args.head).mkString.replace(".", "").replace("-", "")
    os.write(
      os.pwd / "leetcode" / "src" / "solutions" / s"$name.scala",
      s"""package solutions

import leetcode.macros.Macros.logged
import solutions.Util._
import pprint.pprintln
import scala.util.chaining._

@scala.annotation.experimental
object $name {
  //vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
        // val rows = grid.length
        // val cols = grid.head.length

        // type Pos = (Int, Int)
        // extension (p: Pos) {
        //   def r: Int = p._1
        //   def c: Int = p._2
        //   def +(o: Pos): Pos = (r + o.r, c + o.c)
        //   def unary_- : Pos = (-r, -c)
        //   def -(o: Pos): Pos = p + -o
        //   def isInBounds: Boolean =
        //     0 <= r && r < rows &&
        //       0 <= c && c < cols
        // }
        // val down = (1, 0)
        // val right = (0, 1)
        // val up = -down
        // val left = -right
        // val directions = List(down, right, up, left)
        // extension (p: Pos) {
        //   def neighbors: List[Pos] = directions.map(p + _).filter(_.isInBounds)
        // }

        // extension (arr: Array[Array[Int]]) {
        //   def apply(p: Pos): Int = arr(p.r)(p.c)
        //   def update(p: Pos, value: Int): Unit = arr(p.r)(p.c) = value
        // }

  //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
  def main(args: Array[String]): Unit = {

    Solution
      .___(
        parseArrayInt(
          ""
        ),
        parseArrayArrayInt(
          ""
        ),
        parseArrayString(
          \"\"\"\"\"\"
        )
      )
      .pipe(pprintln(_))
  }

}
"""
    )
  }
}
