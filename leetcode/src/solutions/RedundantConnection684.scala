package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*
@scala.annotation.experimental
object RedundantConnection684 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def findRedundantConnection(edgesA: Array[Array[Int]]): Array[Int] = {
      val n = edgesA.length
      val edges_ = edgesA.zipWithIndex
        .flatMap({ case (Array(a, b), ind) => List((a, b, ind), (b, a, ind)) })
        .groupMap(_._1)(t => (t._2, t._3))
        .withDefaultValue(new Array[(Int, Int)](0))
      extension (i: Int) {
        def edges: Array[(Int, Int)] = edges_(i)
      }
      // pprintln(edges_)
      val visited = new Array[Boolean](n + 1)
      // pprintln(visited)
      // @logged
      def dfs(from: Int, edgeTaken: Int): Option[(Int, Int)] = {
        if (visited(from)) {
          Some(from, edgeTaken)
        } else {
          visited(from) = true
          // pprintln(from.edges)
          from.edges
            .filter(_._2 != edgeTaken)
            .foldLeft[Option[(Int, Int)]](None)({
              case (None, (to, edgeInd)) => dfs(to, edgeInd)
              case (o, _)                => o
            }) match {
            case None => None
            case Some((-1, maxEdge)) =>
              Some((-1, maxEdge))
            case Some((startInd, maxEdge)) =>
              if (startInd == from) {
                Some(-1, maxEdge)
              } else {
                Some(startInd, Math.max(maxEdge, edgeTaken))
              }
          }
        }
      }
      edgesA(dfs(1, -1).get._2)
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .findRedundantConnection(
        parseArrayArrayInt(
          "[[1,2],[1,3],[2,3]]"
        )
      )
      .pipe(a => pprintln(a))
    Solution
      .findRedundantConnection(
        parseArrayArrayInt(
          "[[1,2],[2,3],[3,4],[1,4],[1,5]]"
        )
      )
      .pipe(a => pprintln(a))
  }

}
