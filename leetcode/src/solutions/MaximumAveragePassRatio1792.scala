package solutions

object MaximumAveragePassRatio1792 {

  import scala.collection.mutable.PriorityQueue
  import scala.util.chaining._
  //

  object Solution {
    def maxAverageRatio(
        classes: Array[Array[Int]],
        extraStudents: Int
    ): Double = {

      type ClassRef = Array[Int]
      implicit class CRImpl(cr: ClassRef) {
        def ratio: Double = cr(0).toDouble / (cr(1))
        def gainIfInject(inj: Int): Double =
          (cr(0) + inj).toDouble / (cr(1) + inj) - ratio
      }

      // case class ClassRef(n: Int, d: Int) extends Ordered[ClassRef] {
      //   override def compare(that: ClassRef): Int =
      //     gainIfInject(1).compare(that.gainIfInject(1))
      // }

      implicit val ordering: Ordering[ClassRef] =
        Ordering.fromLessThan((thi, tha) =>
          thi.gainIfInject(1) < (tha.gainIfInject(1))
        )
      val pq = PriorityQueue[ClassRef]()
      pq ++= classes

      var studentsLeft = extraStudents
      while (studentsLeft > 0) {
        var cr = pq.dequeue()
        var injectAmount = 1
        val nextGain = pq.head.gainIfInject(1)

        import scala.annotation.tailrec
        @tailrec
        def bSearchRange(
            lo: Int,
            hi: Int,
            trueIfHigher: Int => Boolean
        ): Int = {
          if (lo + 1 == hi)
            hi
          else {
            val mid = (lo + hi) / 2
            if (trueIfHigher(mid))
              bSearchRange(mid, hi, trueIfHigher)
            else
              bSearchRange(lo, mid, trueIfHigher)
          }
        }

        val largestInjectAmount = bSearchRange(
          1,
          studentsLeft + 1,
          injectAmount =>
            injectAmount <= studentsLeft &&
              cr.gainIfInject(injectAmount + 1) <= nextGain
        ) - 1
        cr(0) += injectAmount
        cr(1) += injectAmount
        pq += cr
        studentsLeft -= 1
      }

      // println(pq.toVector.mkString("\n"))

      pq.iterator.map(_.ratio).sum / classes.length

    }
  }

}
