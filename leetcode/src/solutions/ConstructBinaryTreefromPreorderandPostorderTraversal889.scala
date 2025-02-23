package solutions

import leetcode.macros.Macros.logged
import pprint.Tree
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object ConstructBinaryTreefromPreorderandPostorderTraversal889 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def constructFromPrePost(
        preorder: Array[Int],
        postorder: Array[Int]
    ): TreeNode = {

      class TN(
          _value: Int = 0,
          _left: TN = null,
          _right: TN = null,
          _parent: TN = null
      ) {
        var value: Int = _value
        var left: TN = _left
        var right: TN = _right
        var parent: TN = _parent
        def toTreeNode: TreeNode = {
          val ltree = if (left != null) left.toTreeNode else null
          val rtree = if (right != null) right.toTreeNode else null
          TreeNode(value, ltree, rtree)
        }
      }

      case class TreePointer(tn: TN, left: Boolean) {
        def setValue(tnv: TN): TreePointer = {
          tnv.parent = tn
          if (left) {
            tn.left = tnv
          } else {
            tn.right = tnv
          }
          TreePointer(tnv, true)
        }
        def moveNext: TreePointer = {
          if (left)
            TreePointer(tn, false)
          else {
            val parent = tn.parent
            val parentSide = parent.left == tn
            TreePointer(parent, parentSide).moveNext
          }

        }
      }

      val n = preorder.length

      val postorderLookup = new Array[Int](n + 1)
      (0 until n).foreach({ case ind =>
        val posV = postorder(ind)
        postorderLookup(posV) = ind
      })
      def canSubtree(i: Int, j: Int): Boolean = {
        postorderLookup(i) < postorderLookup(j)
      }

      val root = TN(preorder.head, null, null, null)
      var tp = TreePointer(root, true)
      preorder.tail.foreach(nodevalue => {
        val nodeTN = TN(nodevalue, null, null, null)
        while (!canSubtree(nodevalue, tp.tn.value)) {
          tp = tp.moveNext
        }
        tp = tp.setValue(nodeTN)
      })
      root.toTreeNode
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .constructFromPrePost(
        parseArrayInt(
          "[1,2,4,5,3,6,7]"
        ),
        parseArrayInt(
          "[4,5,2,6,7,3,1]"
        )
      )
      .pipe(printTree(_))
  }

}
