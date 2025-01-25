package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MakeLexicographicallySmallestArraybySwappingElements2948 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def lexicographicallySmallestArray(
        nums: Array[Int],
        limit: Int
    ): Array[Int] = {
      val n = nums.length
      type Item = (Int, Int)
      extension (i: Item) {
        def value: Int = i._1
        def ind: Int = i._2
      }
      val items: Array[Item] = nums.zipWithIndex
      items.sortInPlaceBy(_.value)
      case class Clump(start: Int, end: Int)
      val (clumps1, lastClumpStart, _) =
        items.zipWithIndex.tail.foldLeft((List[Clump](), 0, items.head))({
          case ((clumps, startInd, lastItem), (item, ind)) =>
            if (item.value - lastItem.value <= limit) { // same clump
              (clumps, startInd, item)
            } else {
              (Clump(startInd, ind - 1) :: clumps, ind, item)
            } // diff clump
        })
      val clumps = Clump(lastClumpStart, n - 1) :: clumps1
      // println(items.mkString(", "))
      // println(clumps)
      val ans = nums
      clumps.foreach({ case Clump(start, end) =>
        val section = items.view.slice(start, end + 1)
        val positions = section.map(_.ind).sorted
        section
          .zip(positions)
          .foreach({ case (item, targetInd) => ans(targetInd) = item.value })
      })
      ans
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def lexicographicallySmallestArray(
          nums: Array[Int],
          limit: Int
      ): Array[Int] = {
        if (nums.size == 1)
          return nums
        val n = nums.length
        type Item = (Int, Int)
        extension (i: Item) {
          def value: Int = i._1
          def ind: Int = i._2
        }
        val items: Array[Item] = nums.zipWithIndex
        items.sortInPlaceBy(_.value)
        case class Clump(start: Int, end: Int)
        val clumps =
          items.zipWithIndex
            .sliding(2)
            .foldLeft(Vector(Clump(0, 0)))({
              case ((clumps), Array((item1, ind1), (item2, ind2))) =>
                if (item2.value - item1.value <= limit) { // same clump
                  clumps.updated(clumps.size - 1, clumps.last.copy(end = ind2))
                } else {
                  clumps :+ Clump(ind2, ind2)
                } // diff clump
              case _ => ???
            })
        val ans = nums
        clumps.foreach({ case Clump(start, end) =>
          val section = items.view.slice(start, end + 1)
          val positions = section.map(_.ind).sorted
          section
            .zip(positions)
            .foreach({ case (item, targetInd) => ans(targetInd) = item.value })
        })
        ans
      }
    }
  }
  def main(args: Array[String]): Unit = {

    try {
      Solution
        .lexicographicallySmallestArray(
          parseArrayInt(
            "[1,5,3,9,8]"
          ),
          2
        )
        .mkString(", ")
        .pipe(println)
    } catch {
      case e: Error =>
        Console.err.println(e.getMessage())
        e.printStackTrace()
    }
    try {
      Solution
        .lexicographicallySmallestArray(
          parseArrayInt(
            "[1,7,6,18,2,1]"
          ),
          3
        )
        .mkString(", ")
        .pipe(println)
    } catch {
      case e: Error =>
        Console.err.println(e.getMessage())
        e.printStackTrace()
    }
    try {
      Solution
        .lexicographicallySmallestArray(
          parseArrayInt(
            "[1,7,28,19,10]"
          ),
          2
        )
        .mkString(", ")
        .pipe(println)
    } catch {
      case e: Error =>
        Console.err.println(e.getMessage())
        e.printStackTrace()
    }
  }

}
