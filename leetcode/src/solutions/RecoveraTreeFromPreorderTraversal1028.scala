package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object RecoveraTreeFromPreorderTraversal1028 {
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
  object Solution {
    def recoverFromPreorder(traversal: String): TreeNode = {
      case class Node(depth: Int, value: Int)
      class NodeIt(s: String) {
        var ind = s.size - 1
        def nextNode(): Node = {
          if (ind == -1)
            Node(-1, -1)
          else {
            var depth = 0
            var value = 0
            var digitMul = 1
            while (ind >= 0 && (depth == 0 || s(ind) == '-')) {
              val char = s(ind)
              if (char == '-') {
                depth += 1
              } else {
                value += digitMul * (char - '0')
                digitMul *= 10
              }
              ind -= 1
            }
            Node(depth, value)
          }
        }
        def allNodes: Vector[Node] =
          val v1 = Iterator
            .iterate(Vector[Node]())(v => v :+ nextNode())
            .find(v => v.size > 1 && v.last.depth == -1)
            .get
          v1.slice(0, v1.size - 1)
      }
      val nodes = new NodeIt(traversal).allNodes
      import scala.collection.mutable.Stack
      val stack = new Stack[(TreeNode, Int)]()
      stack.push((TreeNode(nodes.head.value, null, null), nodes.head.depth))
      nodes.tail.foreach(node => {
        val (topNode, topDepth) = stack.top
        if (node.depth >= topDepth) {
          stack.push((TreeNode(node.value, null, null), node.depth))
        } else {

          val (c1, dep1) = stack.pop()
          val c2 = {
            if (stack.nonEmpty && (stack.top._2 == dep1)) {
              stack.pop()._1
            } else null
          }
          stack.push((TreeNode(node.value, c1, c2), node.depth))
        }
      })
      stack.pop()._1
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {
    def printTree(tn: TreeNode, parent: Int, childSide: String): Unit = {
      println(s"$parent - $childSide - ${tn.value}")
      if (tn.left != null) {
        printTree(tn.left, tn.value, "left")
      }
      if (tn.right != null) {
        printTree(tn.right, tn.value, "right")
      }
    }
    Solution
      .recoverFromPreorder(
        "1-2--3--4-5--6--7"
      )
      .pipe(printTree(_, -1, "root"))
    println()
    Solution
      .recoverFromPreorder(
        "1-2--3---4-5--6---7"
      )
      .pipe(printTree(_, -1, "root"))
    println()
    Solution
      .recoverFromPreorder(
        "1-401--349---90--88"
      )
      .pipe(printTree(_, -1, "root"))
  }

}
