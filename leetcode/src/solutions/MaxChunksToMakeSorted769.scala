package solutions

import scala.util.chaining._

object MaxChunksToMakeSorted769 {

  import scala.collection.IndexedSeqView
  import scala.collection.immutable.TreeMap
  import scala.collection.mutable.PriorityQueue
  object Solution {
    def maxChunksToSorted(arr: Array[Int]): Int = {

      val n = arr.length
      case class Segment(start: Int, end: Int) {
        def len: Int = end - start
      }
      val segments = arr.zipWithIndex.map {
        case (i, ind) => {
          val start = Math.min(i, ind)
          val end = Math.max(i, ind) + 1
          Segment(start, end)
        }
      }

      val starts: IndexedSeqView[Segment] = segments.sortBy(_.start).view
      val ends: IndexedSeqView[Segment] = segments.sortBy(_.end).view
      val tm: TreeMap[Int, Int] = new TreeMap()

      (Iterator
        .iterate((0, starts, 0)) { case (totalSegments, starts, ind) =>
          def findFinalEndInd(
              starts: IndexedSeqView[Segment],
              endInd: Int,
              startsTaken: Int
          ): (Int, Int) = {
            val prefix = starts.takeWhile(_.start < endInd)
            if (prefix.isEmpty) {
              (endInd, startsTaken)
            } else {
              val newEndInd = Math.max(prefix.map(_.end).max, endInd)
              findFinalEndInd(
                starts.slice(prefix.size, starts.length),
                newEndInd,
                prefix.size + startsTaken
              )
            }
          }

          val (finalEnd, startsTaken) =
            findFinalEndInd(starts, starts.head.end, 0)
          val r =
            (
              totalSegments + 1,
              starts.slice(startsTaken, starts.length),
              finalEnd
            )
          r
        })
        .find(_._3 >= n)
        .get
        ._1
    }
  }

  def main(args: Array[String]): Unit = {
    Solution
      .maxChunksToSorted(Util.parseArrayInt("[0,1,3,5,4,2]"))
      .pipe(println)
  }

  object MaxChunksToMakeSorted769_2 {

    object Solution {
      def maxChunksToSorted(arr: Array[Int]): Int = {
        arr.indices.foldLeft(0) { case (total, i) =>
          val pe = if (i == 0) 0 else arr(i - 1)
          val nv = Math.max(arr(i), pe)
          arr.update(i, nv)
          if (nv == i)
            total + 1
          else
            total
        }
      }
    }

  }

}
