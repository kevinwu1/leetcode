package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MinimumTimetoRepairCars2594 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def repairCars(ranks: Array[Int], cars: Int): Long = {
      import scala.collection.mutable.PriorityQueue
      def cost(tup: (Long, Long)): Long = {
        val (rank, car) = tup
        rank * car * car
      }
      val pq = new PriorityQueue[(Long, Long)]()(
        Ordering
          .fromLessThan[(Long, Long)]((c1, c2) => cost(c1) < cost(c2))
          .reverse
      )
      ranks.foreach(r => pq += ((r, 1)))
      (0 until cars).foreach(_ => {
        val (rank, cc) = pq.dequeue()
        pq += ((rank, cc + 1))
      })
      pq.map({ case (rank, cc) => rank * (cc - 1) * (cc - 1) }).max
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def repairCars(ranks: Array[Int], cars: Int): Long = {
        val rankGrp =
          ranks.groupBy(identity).map({ case (k, v) => k -> v.size })

        def canAchieve(cost: Long): Boolean = {
          rankGrp
            .map({ case (rank, cnt) =>
              cnt * Math.floor(Math.sqrt(cost / rank))
            })
            .sum >= cars
        }
        import scala.annotation.tailrec
        @tailrec
        def bsearch(lo: Long, hi: Long): Long = {
          if (lo + 1 >= hi)
            hi
          else {
            val mid = (lo + hi) / 2
            if (canAchieve(mid))
              bsearch(lo, mid)
            else
              bsearch(mid, hi)
          }
        }
        bsearch(1, Long.MaxValue)
      }
    }
  }
  def main(args: Array[String]): Unit = {

    Solution
      .repairCars(
        parseArrayInt(
          "[4,2,3,1]"
        ),
        10
      )
      .pipe(pprintln(_))
  }

}
