package solutions

import scala.util.chaining._

object MinimumNumberofOperationstoSortaBinaryTreebyLevel2471 {

  class TreeNode(
      _value: Int = 0,
      _left: TreeNode = null,
      _right: TreeNode = null
  ) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  import scala.annotation.tailrec
  object Solution {
    def minimumOperations(root: TreeNode): Int = {
      import scala.collection.mutable.ArrayBuffer
      def countSwaps(l: ArrayBuffer[Int]): Int = {
        val sorted = l.sorted
        var total = 0
        for (i <- 0 until l.size) {
          while (l(i) != sorted(i)) {
            val swapInd = sorted.search(l(i)).insertionPoint
            val temp = l(i)
            l(i) = l(swapInd)
            l(swapInd) = temp
            total += 1
          }
        }
        total
      }
      implicit class TNImpl(n: TreeNode) {
        def children: Vector[TreeNode] = {
          (n.left, n.right) match {
            case (null, null) => Vector()
            case (l, null)    => Vector(l)
            case (null, r)    => Vector(r)
            case (l, r)       => Vector(l, r)
          }
        }
      }
      Iterator
        .iterate((Vector(root), 0)) {
          case ((nodes, total)) => {

            val nextNodes = nodes.flatMap(_.children)
            val ab = ArrayBuffer[Int]()
            nextNodes.foreach(n => ab += n.value)
            val swaps = countSwaps(ab)
            // println(nextNodes.map(_.value))
            // println(nextNodes.sortBy(_.value).map(_.value))
            // println(swaps)
            (nextNodes, total + swaps)
          }
        }
        .find(_._1.isEmpty)
        .get
        ._2
    }
  }

  def main(args: Array[String]): Unit = {

    val data =
      Util.parseArrayIntOrNull("[1,3,2,7,6,5,4]")
    def treeify(from: Int): TreeNode = {
      if (from >= data.length || data(from) == None)
        null
      else {
        val n = new TreeNode(_value = data(from).get)
        if (from * 2 + 1 < data.length) {
          n.left = treeify(from * 2 + 1)
          n.right = treeify(from * 2 + 2)
        }
        n
      }
    }
    implicit class tnimplicits2(tn: TreeNode) {
      def pretty: String = {
        if (tn.left == null) {
          tn.value.toString()
        } else
          s"[${tn.left.pretty}|${tn.value}|${tn.right.pretty}]"
      }
    }
    Solution
      .minimumOperations(treeify(0))
      .pipe(println)
  }

}
