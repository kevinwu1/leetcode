package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object CourseScheduleIV1462 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def checkIfPrerequisite(
        numCourses: Int,
        prerequisites: Array[Array[Int]],
        queries: Array[Array[Int]]
    ): List[Boolean] = {
      val outs_ = Array.fill(numCourses)(Set[Int]())
      val ins_ = Array.fill(numCourses)(Set[Int]())
      {
        var i = 0
        while (i < prerequisites.length) {
          val Array(from, to) = prerequisites(i)
          outs_(from) += to
          ins_(to) += from
          i += 1
        }
      }
      extension (i: Int) {
        def outs: Set[Int] = outs_(i)
        def ins: Set[Int] = ins_(i)
      }
      import scala.collection.mutable.BitSet
      val prereqs = Array.fill(numCourses)(new BitSet())

      val inDeg = (0 until numCourses).map(i => i.ins.size).toArray
      @scala.annotation.tailrec
      def topo(nodes: Set[Int]): Unit = {
        if (nodes.nonEmpty) {
          val nextLevel = nodes.flatMap(from =>
            from.outs.filter(to => {
              prereqs(to) += from
              prereqs(to) ++= prereqs(from)
              inDeg(to) -= 1
              inDeg(to) == 0
            })
          )
          topo(nextLevel)
        }
      }
      topo((0 until numCourses).filter(inDeg(_) == 0).toSet)

      var ans = List[Boolean]()
      var i = queries.length - 1
      while (i >= 0) {
        val Array(from, to) = queries(i)
        ans = prereqs(to).contains(from) :: ans
        i -= 1
      }
      ans
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .checkIfPrerequisite(
        2,
        parseArrayArrayInt(
          "[[1,0]]"
        ),
        parseArrayArrayInt(
          "[[0,1],[1,0]]"
        )
      )
      .pipe(println)
    Solution
      .checkIfPrerequisite(
        2,
        parseArrayArrayInt(
          "[]"
        ),
        parseArrayArrayInt(
          "[[1,0],[0,1]]"
        )
      )
      .pipe(println)
    Solution
      .checkIfPrerequisite(
        3,
        parseArrayArrayInt(
          "[[1,2],[1,0],[2,0]]"
        ),
        parseArrayArrayInt(
          "[[1,0],[1,2]]"
        )
      )
      .pipe(println)
  }

}
