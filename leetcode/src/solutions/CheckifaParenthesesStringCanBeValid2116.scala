package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object CheckifaParenthesesStringCanBeValid2116 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def canBeValid(s: String, locked: String): Boolean = {
      val res = s
        .zip(locked)
        .foldLeft((0, 0, true))({
          case ((opn, opx, possible), (parenC, lockedC)) => {
            // println(s"$opn, $opx, $possible, $parenC, $lockedC")
            if (!possible)
              (opn, opx, false)
            else {
              val locked = lockedC == '1'
              val paren = if parenC == '(' then 1 else -1
              if (locked) {
                val newOpn = if (opn + paren == -1) 1 else opn + paren
                (newOpn, opx + paren, opx + paren >= 0)
              } else {
                val newOpn = if (opn == 0) 1 else opn - 1
                val newOpx = opx + 1
                (newOpn, newOpx, possible)
              }
            }

          }
        })
      res._1 == 0 && res._3
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def canBeValid(s: String, lockedS: String): Boolean = {
        var (opn, opx) = (0, 0)
        var i = 0
        while (i < s.length) {
          val paren = if (s(i) == '(') 1 else -1
          if (lockedS(i) == '1') {
            opn = if (opn + paren == -1) 1 else opn + paren
            opx += paren
            if (opx < 0)
              return false
          } else {
            opn = if (opn == 0) 1 else opn - 1
            opx = opx + 1
          }
          i += 1
        }
        opn == 0
      }
    }
  }
  def main(args: Array[String]): Unit = {
    Sol2.Solution
      .canBeValid(
        "((()(()()))()((()()))))()((()(()",
        "10111100100101001110100010001001"
      )
      .pipe(println)
  }

}
