package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MinimizeXOR2429 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def minimizeXor(num1: Int, num2: Int): Int = {
      import java.lang.Integer.bitCount
      val nbits2 = bitCount(num2)
      val nbits1 = bitCount(num1)
      if (nbits2 < nbits1) {
        var i = 0
        while (i < 32) {
          val mask = num1 & ((1 << i) - 1)
          if (bitCount(mask) == nbits1 - nbits2)
            return num1 ^ mask
          i += 1
        }
      } else if (nbits1 == nbits2) {
        return num1
      } else {
        var i = 0
        while (i < 32) {
          val res = num1 | ((1 << i) - 1)
          if (bitCount(res) == nbits2)
            return res
          i += 1
        }
      }
      return -1
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .minimizeXor(
        3,
        5
      )
      .pipe(println)
  }

}
