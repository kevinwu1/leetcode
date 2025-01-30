package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object DivideNodesIntotheMaximumNumberofGroups2493 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def magnificentSets(n: Int, edgesP: Array[Array[Int]]): Int = {

      val edges_ = edgesP
        .flatMap({ case Array(a_, b_) =>
          val a = a_ - 1
          val b = b_ - 1
          List(a -> b, b -> a)
        })
        .groupMap(_._1)(_._2)
        .withDefaultValue(Array[Int]())
      extension (i: Int) {
        def edges: Array[Int] = edges_(i)
      }
      var total = 0
      val visited = new Array[Boolean](n)
      def process(start: Int): Int = {
        def graphify(layer: Set[Int], seen: Set[Int]): Option[Set[Int]] = {
          if (layer.isEmpty) {
            Some(Set())
          } else {
            val children = layer.flatMap(_.edges).filter(!seen.contains(_))
            if (children.exists(layer.contains(_)))
              None
            else {
              graphify(children, seen ++ layer).map(_ ++ layer)
            }
          }
        }
        graphify(Set(start), Set()) match {
          case None => -1
          case Some(allNodes) =>
            def diameter(start: Int): Int = {
              val distance = Array.fill(n)(-1)
              import scala.collection.mutable

              var ans = 0
              val q = new mutable.Queue[(Int, Int)]()
              q += ((start, 0))
              while (q.nonEmpty) {
                val (node, dist) = q.dequeue()
                if (distance(node) == -1) {
                  distance(node) = dist
                  ans = Math.max(ans, dist)
                  node.edges.foreach(e => q += ((e, dist + 1)))
                }
              }
              ans
            }
            allNodes.foreach(visited(_) = true)
            allNodes.map(diameter).max + 1
        }
      }
      var i = 0
      while (i < n) {
        if (!visited(i)) {
          val p = process(i)
          if (p == -1)
            return -1
          total += p
        }
        i += 1
      }
      total
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .magnificentSets(
        6,
        parseArrayArrayInt(
          "[[1,2],[1,4],[1,5],[2,6],[2,3],[4,6]]"
        )
      )
      .pipe(println)
    Solution
      .magnificentSets(
        3,
        parseArrayArrayInt(
          "[[1,2],[2,3],[3,1]]"
        )
      )
      .pipe(println)
  }

}
