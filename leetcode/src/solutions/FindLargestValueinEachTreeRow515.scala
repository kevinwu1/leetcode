package solutions

import leetcode.macros.Macros.logged
import solutions.Util._

import scala.collection.immutable.Queue
import scala.util.chaining._

object FindLargestValueinEachTreeRow515 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def largestValues(root: TreeNode): List[Int] = {
      if (root == null)
        List()
      else {
        def bfs(nodes: Vector[TreeNode], answers: Vector[Int]): Vector[Int] = {
          if (nodes.isEmpty)
            answers
          else
            bfs(
              nodes
                .flatMap(n => Vector(Option(n.left), Option(n.right)))
                .flatten,
              answers :+ nodes.map(_.value).max
            )
        }
        bfs(Vector(root), Vector()).toList
      }
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Solution2 {
    // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
    object Solution {
      def largestValues(root: TreeNode): List[Int] = {
        if (root == null)
          List()
        else {
          import scala.collection.mutable
          val q = mutable.Queue(root)
          val answer = List.newBuilder[Int]
          while (q.nonEmpty) {
            var max = Int.MinValue
            var stop = false
            q += null
            while (!stop) {
              val n = q.dequeue()
              if (n == null) {
                stop = true
              } else {
                max = Math.max(max, n.value)
                if (n.left != null)
                  q += n.left
                if (n.right != null)
                  q += n.right
              }
            }
            answer += max
          }
          answer.result()
        }
      }
    }
    // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  }

  def main(args: Array[String]): Unit = {

    Solution2.Solution
      .largestValues(
        treeifyOpt(parseArrayIntOrNull("[1,3,2,5,3,null,9]"))
      )
      .pipe(println)
  }

}
