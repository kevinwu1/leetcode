package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object ApplyOperationstoMaximizeScore2818 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    var doLog = false
    def println(a: Any): Unit =
      if (doLog)
        Console.out.println(a)
    val mod = 1000000007
    implicit class LongImpls(i: Long) {
      def *%(o: Long): Long =
        (i * o) % mod
    }

    import scala.reflect.ClassTag
    class SparseTableMin[T: Ordering: ClassTag](items: Array[T]) {
      val min = implicitly[Ordering[T]].min
      val n = items.length
      def log2(n: Int): Int = {
        var n_ = n
        var r = 0
        while (n_ > 1) {
          r += 1
          n_ >>= 1
        }
        r
      }
      val rows = log2(n) + 1
      val matrix = new Array[Array[T]](rows)
      matrix(0) = items
      (1 until rows).foreach(r => {
        matrix(r) = new Array[T](n)
        val off = 1 << (r - 1)
        (0 until n - off * 2 + 1).foreach(i => {
          matrix(r)(i) = min(matrix(r - 1)(i), matrix(r - 1)(i + off))
        })
      })
      // @logged
      def query(lo: Int, hi: Int): T = {
        val n = hi - lo
        val row = log2(n)
        min(matrix(row)(lo), matrix(row)(hi - (1 << row)))
      }

      println(matrix.map(_.mkString(",")).mkString("\n"))
    }

    def maximumScore(nums: List[Int], k: Int): Int =
      maximumScore(nums.toArray.map(_.toLong), k)

    def maximumScore(nums: Array[Long], k: Long): Int = {
      println(nums.mkString(", "))
      val primeScores = nums.map(primeScore)
      val st = new SparseTableMin(primeScores)(
        Ordering.Int.reverse,
        implicitly[ClassTag[Int]]
      )
      val n = nums.length
      import scala.collection.mutable
      val pq = mutable.PriorityQueue[Int]()(
        Ordering.fromLessThan[Int]((t1, t2) => nums(t1) < nums(t2))
      )
      (0 until n).foreach(pq += _)
      println(primeScores.mkString(", "))
      // initizliae Seg Tree

      var total = 1L
      var ops = 0L
      while (ops < k) {
        val index = pq.dequeue()
        val ps = primeScores(index)
        println(s"Checking ${nums(index)} at index: $index, ps=${ps}")
        val lInd: Int = index - bsearch(
          0,
          index + 1,
          offset => {
            val testlInd = index - offset
            testlInd >= 0 && st.query(testlInd, index) < ps
          }
        )
        val rInd: Int = index + bsearch(
          0,
          n - index,
          offset => {
            val testrInd = index + offset
            testrInd < n && st.query(index, testrInd + 1) <= ps
          }
        )
        //  segtree find smallest lInd < index, for lInd <= ind < ind , ps[lInd] < ps
        //  segtree find largets rInd > index, for ind < ind < rInd , ps[rInd] <= ps
        println(s"lind: $lInd, rind: $rInd")
        val subarrays = (index + 1 - lInd).toLong * (rInd + 1 - index).toLong
        val toAdd = Math.min(k - ops, subarrays)
        val toMult = power(nums(index), toAdd)
        println(s"sa: $subarrays, ta: $toAdd, tm: $toMult")
        ops += toAdd
        total *%= toMult
        println(s"ops: $ops, total: $total")
      }
      (total % mod).toInt
    }

    // @logged
    def bsearch(
        lo: Int,
        hi: Int,
        test: Int => Boolean
    ): Int = {
      if (lo + 1 >= hi)
        lo
      else {
        val mid = (lo + hi) / 2
        if (test(mid))
          bsearch(mid, hi, test)
        else
          bsearch(lo, mid, test)
      }
    }

    def power(base: Long, amt: Long): Long = {
      @scala.annotation.tailrec
      def helper(v: Long, bitSelector: Long): Long = {
        if (bitSelector == 0)
          v
        else {
          val v2 = (v *% v) *% (if ((amt & bitSelector) != 0) base else 1)
          helper(v2, bitSelector >>> 1)
        }
      }
      helper(1, 1L << 63) % mod
    }

    def primeScore(_n: Long): Int = {
      if (_n == 1) return 0
      var n = _n
      var total = 0
      if (n % 2 == 0) {
        total += 1
        while (n % 2 == 0) n /= 2
      }
      if (n % 3 == 0) {
        total += 1
        while (n % 3 == 0) n /= 3
      }
      var i = 6
      while ((i - 1) * (i - 1) <= _n) {
        if (n % (i - 1) == 0) {
          total += 1
          while (n % (i - 1) == 0)
            n /= (i - 1)
        }
        if (n % (i + 1) == 0) {
          total += 1
          while (n % (i + 1) == 0)
            n /= (i + 1)
        }
        i += 6
      }
      if (total == 0) 1
      else
        total + (if (n == 1) 0 else 1)
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {
    Solution.doLog = true
    // println(Solution.primeScore(300))
    // Solution
    //   .maximumScore(
    //     parseArrayInt(
    //       "[8,3,9,3,8]"
    //     ).toList,
    //     2
    //   )
    //   .pipe(pprintln(_))

    // (0 to 100).foreach(x => Solution.power(x, x))
    // println(Solution.power(3, 2))
    // println(Solution.power(2, 5))
    // Solution
    //   .maximumScore(
    //     parseArrayInt(
    //       "[19,12,14,6,10,18]"
    //     ).toList,
    //     3
    //   )
    //   .pipe(pprintln(_))
    // Solution
    //   .maximumScore(
    //     parseArrayInt(
    //       "[3289,2832,14858,22011]"
    //     ).toList,
    //     6
    //   )
    //   .pipe(pprintln(_))
    Solution
      .maximumScore(
        parseArrayInt(
          "[1,7,11,1,5]"
        ).toList,
        14
      )
      .pipe(pprintln(_))
  }

}
