package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object FindElementsinaContaminatedBinaryTree1261 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  class TreeNode(
      _value: Int = 0,
      _left: TreeNode = null,
      _right: TreeNode = null
  ) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  class FindElements(_root: TreeNode) {
    import scala.collection.mutable.BitSet
    val answer = BitSet()
    def insert(tn: TreeNode, rootVal: Int): Unit = {
      answer += (rootVal)
      if (tn.left != null)
        insert(tn.left, rootVal * 2 + 1)
      if (tn.right != null)
        insert(tn.right, rootVal * 2 + 2)
    }
    insert(_root, 0)
    def find(target: Int): Boolean = {
      answer.contains(target)
    }

  }

  /** Your FindElements object will be instantiated and called as such: val obj
    * \= new FindElements(root) val param_1 = obj.find(target)
    */
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {
    {
      val root = new TreeNode(-1, null, new TreeNode(-1, null, null))
      val params = parseArrayArrayInt(
        "[[1],[2]]"
      )
      val queries = parseArrayString(
        """["find","find"]"""
      )
      val fe = new FindElements(root)
      queries
        .zip(params)
        .foreach({ case ("find", Array(p)) =>
          println(fe.find(p))
        })
    }
    {
      val root = new TreeNode(
        -1,
        new TreeNode(
          -1,
          new TreeNode(-1, null, null),
          new TreeNode(-1, null, null)
        ),
        new TreeNode(-1, null, null)
      )
      val params = parseArrayArrayInt(
        "[[1],[3],[5]]"
      )
      val queries = parseArrayString(
        """["find","find","find"]"""
      )
      val fe = new FindElements(root)
      queries
        .zip(params)
        .foreach({ case ("find", Array(p)) =>
          println(fe.find(p))
        })
    }
    {
      val root = new TreeNode(
        -1,
        null,
        new TreeNode(
          -1,
          new TreeNode(-1, new TreeNode(-1, null, null), null),
          null
        )
      )
      val params = parseArrayArrayInt(
        "[[2],[3],[4],[5]]"
      )
      val queries = parseArrayString(
        """["find","find","find","find"]"""
      )
      val fe = new FindElements(root)
      queries
        .zip(params)
        .foreach({ case ("find", Array(p)) =>
          println(fe.find(p))
        })
    }
  }

}
