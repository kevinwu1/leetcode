package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object NeighboringBitwiseXOR2683 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def doesValidArrayExist(derived: Array[Int]): Boolean = {
      derived.reduce(_ ^ _) == 0
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def doesValidArrayExist(derived: Array[Int]): Boolean = {
        var r = 0
        var i = 0
        while (i < derived.length) {
          r ^= derived(i)
          i += 1
        }
        r == 0
      }
    }
  }
  def main(args: Array[String]): Unit = {

    Solution
      .doesValidArrayExist(
        parseArrayInt(
          "[1,1,0]"
        )
      )
      .pipe(println)
  }

}
