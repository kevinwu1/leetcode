package solutions

import leetcode.macros.Macros.logged
import solutions.Util.parseArrayArrayInt

import scala.util.chaining._

object FindMinimumDiameterAfterMergingTwoTrees3203 {

  import scala.annotation.tailrec
  object Solution {
    def minimumDiameterAfterMerge(
        edges1: Array[Array[Int]],
        edges2: Array[Array[Int]]
    ): Int = {
      case class Tree(edgesArr: Array[Array[Int]]) {
        val len = edgesArr.length
        val edges: Map[Int, Set[Int]] =
          edgesArr
            .flatMap { case Array(a, b) =>
              List(a -> b, b -> a)
            }
            .foldLeft(Map[Int, Set[Int]]()) { case (accum, a -> b) =>
              accum + (a -> (accum.getOrElse(a, Set()) + b))
            }
      }

      @logged
      def getRadiusAndDiam(e: Array[Array[Int]]): (Int, Int) = {
        if (e.length == 0)
          (0, 0)
        else if (e.length == 1)
          (1, 1)
        else {
          val tree = Tree(e)
          val root = tree.edges.find(_._2.size >= 2).get._1
          @logged
          def getDiamAndDepth(root: Int, parent: Int): (Int, Int) = {
            implicit class nodeImpl(i: Int) {
              def edges: Set[Int] = tree.edges(i)
            }
            val children = root.edges - parent
            leetcode.macros.Macros.indentPrintln(children)
            if (children.isEmpty) {
              (0, 0)
            } else {
              val childN = children.size
              val (diams, depths) =
                children.toVector.map(c => getDiamAndDepth(c, root)).unzip
              if (children.size == 1) {
                (diams.max, depths.max + 1)
              } else {
                val sortedDepths = depths.toVector.sorted
                val crossDiam =
                  sortedDepths(childN - 2) + sortedDepths(childN - 1) + 2
                (Math.max(crossDiam, diams.max), depths.max + 1)
              }
            }
          }
          val diam = getDiamAndDepth(root, -1)._1
          (diam / 2 + diam % 2, diam)
        }
      }

      val (r1, d1) = getRadiusAndDiam(edges1)
      val (r2, d2) = getRadiusAndDiam(edges2)
      Math.max(r1 + r2 + 1, Math.max(d1, d2))
    }
  }
  def main(args: Array[String]): Unit = {

    Solution
      .minimumDiameterAfterMerge(
        parseArrayArrayInt(
          "[[0,1],[2,0],[3,2],[3,6],[8,7],[4,8],[5,4],[3,5],[3,9]]"
        ),
        parseArrayArrayInt("[[0,1],[0,2],[0,3]]")
      )
      .pipe(println)
  }

}
