package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object PutMarblesinBags2551 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    var doLog = false
    def println(a: Any): Unit =
      if (doLog)
        Console.out.println(a)
    def putMarbles(weights: Array[Int], k: Int): Long = {
      if (weights.length <= 2 || k == weights.length) {
        0
      } else {
        val pairsum = weights.sliding(2).map(_.map(_.toLong).sum).toVector
        import scala.collection.mutable
        val total = pairsum.sum
        val pq = new mutable.PriorityQueue[Long]().addAll(pairsum)
        val k1 = k - 1
        val k2 = pairsum.length - k1
        println(k1, k2)
        var mn = 0L
        var mx = 0L
        var csum = 0L
        var i = 0
        while (i < Math.max(k1, k2)) {
          csum += pq.dequeue()
          i += 1
          if (i == k1)
            mx = csum
          if (i == k2)
            mn = total - csum
        }
        mx - mn
      }
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      var doLog = false
      def println(a: Any): Unit =
        if (doLog)
          Console.out.println(a)
      def putMarbles(_weights: Array[Int], k: Int): Long = {
        val weights = _weights.map(_.toLong)
        val n = weights.length
        if (n <= 2 || k == n) {
          0
        } else {
          var total = 0L
          {
            var i = 0
            while (i < n - 1) {
              weights(i) += weights(i + 1)
              total += weights(i)
              i += 1
            }
            weights(n - 1) = 0
          }
          {
            def sink(i: Int): Unit = {
              val v = weights(i)
              val li = 2 * i + 1
              var ri = 2 * i + 2
              val l = if (li < n) weights(li) else 0
              val r = if (ri < n) weights(ri) else 0
              if (v < l || v < r) {
                if (l > r) {
                  weights(i) = l
                  weights(li) = v
                  sink(li)
                } else {
                  weights(i) = r
                  weights(ri) = v
                  sink(ri)
                }
              }
            }
            (0 to n / 2 - 1).reverse.foreach(i => {
              sink(i)
            })
          }
          def extract(i: Int): Long = {
            if (i >= n) {
              0
            } else {
              val ret = weights(i)
              val li = 2 * i + 1
              var ri = 2 * i + 2
              val l = if (li < n) weights(li) else 0
              val r = if (ri < n) weights(ri) else 0
              if (l > r) {
                weights(i) = extract(li)
              } else {
                weights(i) = extract(ri)
              }
              ret
            }
          }
          val k1 = k - 1
          val k2 = n - 1 - k1
          var mn = 0L
          var mx = 0L
          var csum = 0L
          var i = 0
          while (i < Math.max(k1, k2)) {
            csum += extract(0)
            i += 1
            if (i == k1)
              mx = csum
            if (i == k2)
              mn = total - csum
          }
          mx - mn
        }
      }
    }
  }
  def main(args: Array[String]): Unit = {
    Solution.doLog = true
    Sol2.Solution
      .putMarbles(
        parseArrayInt(
          "[1,3,5,1]"
        ),
        2
      )
      .pipe(pprintln(_))
    Sol2.Solution
      .putMarbles(
        parseArrayInt(
          "[1,3]"
        ),
        2
      )
      .pipe(pprintln(_))
  }

}
