package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object MostProfitablePathinaTree2467 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def mostProfitablePath(
        edges: Array[Array[Int]],
        bob: Int,
        amount: Array[Int]
    ): Int = {
      val n = edges.length + 1
      val _parent = new Array[Int](n + 1)
      val _edges = edges
        .flatMap({ case Array(a, b) => List(a -> b, b -> a) })
        .groupMap(_._1)(_._2)
        .withDefaultValue(Array[Int]())
      extension (i: Int) {
        def edgeSet: Array[Int] = _edges(i)
        def amt: Int = amount(i)
      }
      var bobDepth = -1
      def setupParents(node: Int, par: Int, depth: Int): Unit = {
        if (node == bob) {
          bobDepth = depth
        }
        _parent(node) = par
        node.edgeSet
          .filter(_ != par)
          .foreach(child => setupParents(child, node, depth + 1))
      }
      setupParents(0, -1, 1)
      extension (i: Int) {
        def parent: Int = _parent(i)
      }

      var bd = bobDepth / 2
      var bobPt = bob
      while (bd > 0) {
        amount(bobPt) = 0
        bobPt = bobPt.parent
        bd -= 1
      }
      if (bobDepth % 2 == 1) {
        amount(bobPt) /= 2
      }

      def maxPathAmt(node: Int): Int = {
        node.amt +
          node.edgeSet
            .filter(_ != node.parent)
            .map(maxPathAmt)
            .maxOption
            .getOrElse(0)
      }
      maxPathAmt(0)
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .mostProfitablePath(
        parseArrayArrayInt(
          "[[0,1],[1,2],[1,3],[3,4]]"
        ),
        3,
        parseArrayInt(
          "[-2,4,2,-4,6]"
        )
      )
      .pipe(pprintln(_))
    Solution
      .mostProfitablePath(
        parseArrayArrayInt(
          "[[0,1]]"
        ),
        1,
        parseArrayInt(
          "[-7280,2350]"
        )
      )
      .pipe(pprintln(_))
  }

}
