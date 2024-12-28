package solutions

import scala.util.chaining._
import solutions.Util.TreeNode
import solutions.Util.treeify

object ReverseOddLevelsofBinaryTree2415 {

  import scala.annotation.tailrec
  object Solution {
    def reverseOddLevels(root: TreeNode): TreeNode = {
      implicit class tnimplicits(tn: TreeNode) {
        def children: Vector[TreeNode] = {
          if (tn.left == null)
            Vector()
          else
            Vector(tn.left, tn.right)
        }
      }

      @tailrec
      def traverse(
          nodes: Vector[TreeNode],
          level: Int,
          oddOrder: Map[Int, Vector[Int]]
      ): Map[Int, Vector[Int]] = {
        if (nodes.isEmpty)
          oddOrder
        else {
          val updatedOddOrder = if (level % 2 == 1) {
            oddOrder.updated(level, nodes.map(_.value))
          } else oddOrder
          val children = nodes.flatMap(_.children)
          traverse(children, level + 1, updatedOddOrder)
        }
      }

      val oddOrder = traverse(Vector(root), 0, Map())

      @tailrec
      def update(
          nodes: Vector[TreeNode],
          level: Int
      ): Unit = {
        if (nodes.nonEmpty) {
          if (level % 2 == 1) {
            nodes.zip(oddOrder(level).reverse).foreach { case (node, newval) =>
              node.value = newval
            }
          }
          val children = nodes.flatMap(_.children)
          update(children, level + 1)
        }
      }
      update(Vector(root), 0)
      root
    }
  }

  def main(args: Array[String]): Unit = {

    val data = Util.parseArrayInt("[2,3,5,8,13,21,34]")

    Solution
      .reverseOddLevels(treeify(data))
      .pretty
      .pipe(println)
  }

}
