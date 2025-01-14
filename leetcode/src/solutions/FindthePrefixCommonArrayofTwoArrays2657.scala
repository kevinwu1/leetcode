package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object FindthePrefixCommonArrayofTwoArrays2657 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def findThePrefixCommonArray(A: Array[Int], B: Array[Int]): Array[Int] = {
      (0 until A.length).toArray
        .scanLeft((Set[Int](), Set[Int]()))({ case ((seta, setb), i) =>
          (seta + A(i), setb + B(i))
        })
        .tail
        .map({ case (ar, br) => (ar & br).size })
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def findThePrefixCommonArray(A: Array[Int], B: Array[Int]): Array[Int] = {
        val n = A.length
        val ans = Array.fill(n)(0)
        var i = 0
        import scala.collection.mutable.BitSet
        val seta = BitSet()
        val setb = BitSet()
        while (i < n) {
          seta += A(i)
          setb += B(i)
          ans(i) = seta.intersect(setb).size
          i += 1
        }
        ans
      }
    }
  }

  object Sol3 {
    object Solution {
      def findThePrefixCommonArray(A: Array[Int], B: Array[Int]): Array[Int] = {
        val n = A.length
        val ans = new Array[Int](n)
        val lu = new Array[Int](n)

        {
          var i = 0
          while (i < n) {
            lu(B(i) - 1) = i
            i += 1
          }
        }
        var i = 0
        while (i < n) {
          var j = 0
          var cnt = 0
          while (j <= i) {
            if (lu(A(j) - 1) <= i)
              cnt += 1
            j += 1
          }
          ans(i) = cnt
          i += 1
        }
        ans
      }
    }
  }

  object Sol4 {
    object Solution {
      def findThePrefixCommonArray(A: Array[Int], B: Array[Int]): Array[Int] = {
        var n = A.length
        var loop = 0
        while (loop < n) {
          if (B(loop) >= 0) {
            var i = loop
            var nx = B(i) - 1
            while (nx != loop) {
              val nnx = B(nx) - 1
              B(nx) = -i - 1
              i = nx
              nx = nnx
            }
            B(nx) = -i - 1
          }
          loop += 1
        }
        {
          var i = 0
          while (i < n) {
            A(i) = -B(A(i) - 1) - 1
            i += 1
          }
        }
        var i = 0
        while (i < n) {
          var j = 0
          var cnt = 0
          while (j <= i) {
            if (A(j) <= i)
              cnt += 1
            j += 1
          }
          B(i) = cnt
          i += 1
        }
        B
      }
    }
  }

  object Sol6 {
    object Solution {
      def findThePrefixCommonArray(A: Array[Int], B: Array[Int]): Array[Int] = {
        val n = A.length
        var i = 0
        var seta = 0L
        var setb = 0L
        while (i < n) {
          seta |= 1L << A(i)
          setb |= 1L << B(i)
          B(i) = java.lang.Long.bitCount(seta & setb)
          i += 1
        }
        B
      }
    }
  }
  def main(args: Array[String]): Unit = {
    Sol4.Solution
      .findThePrefixCommonArray(
        parseArrayInt(
          "[1,3,2,4]"
        ),
        parseArrayInt(
          "[3,1,2,4]"
        )
      )
      .mkString(",")
      .pipe(println)
  }

}
