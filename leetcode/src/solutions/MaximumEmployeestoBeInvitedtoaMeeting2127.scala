package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MaximumEmployeestoBeInvitedtoaMeeting2127 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def maximumInvitations(favorite: Array[Int]): Int = {
      val n = favorite.length
      val ins_ = (0 until n)
        .map(i => favorite(i) -> i)
        .groupMap(_._1)(_._2)
        .view
        .mapValues(_.toSet)
        .toMap
        .withDefaultValue(Set())
      extension (i: Int) {
        def fav: Int = favorite(i)
        def ins: Set[Int] = ins_(i)
      }
      def get2CycleMax(): Int = {
        val twoCycles =
          (0 until n)
            .filter(i => favorite(favorite(i)) == i)
            .toList
            .groupBy(i => Math.min(i, favorite(i)))
        if (twoCycles.nonEmpty) {
          def bfs(start: Int, exclude: Int): Int = {
            @scala.annotation.tailrec
            def helper(s: Set[Int], v: Int): Int = {
              val nextLayer = s.flatMap(_.ins)
              if nextLayer.isEmpty then v
              else helper(nextLayer, v + 1)
            }
            val ins1 = start.ins.filterNot(_ == exclude).toSet
            if ins1.isEmpty then 0 else helper(ins1, 1)
          }
          twoCycles.values
            .map({
              case List(a, b) =>
                bfs(a, b) + bfs(b, a) + 2
              case _ => ???
            })
            .sum
        } else 0
      }
      def getFullCycleMax(): Int = {
        val generation_ = Array.fill(n)(-1)
        val level_ = Array.fill(n)(-1)
        extension (i: Int) {
          def setGeneration(g: Int): Unit = generation_(i) = g
          def generation: Int = generation_(i)
          def setLevel(l: Int): Unit = level_(i) = l
          def level: Int = level_(i)
        }
        (0 until n)
          .map(i => {
            if (i.level == -1) {
              def explore(start: Int, level: Int): Int = {
                if (start.level != -1) {
                  if (start.generation == i)
                    level - start.level
                  else
                    0
                } else {
                  start.setLevel(level)
                  start.setGeneration(i)
                  val next = start.fav
                  explore(next, level + 1)
                }
              }
              explore(i, 0)
            } else 0
          })
          .max
      }
      Math.max(get2CycleMax(), getFullCycleMax())
    }

  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .maximumInvitations(
        parseArrayInt(
          "[2,2,1,2]"
        )
      )
      .pipe(println)
    Solution
      .maximumInvitations(
        parseArrayInt(
          "[1,2,0]"
        )
      )
      .pipe(println)
    Solution
      .maximumInvitations(
        parseArrayInt(
          "[3,0,1,4,1]"
        )
      )
      .pipe(println)
  }

}
