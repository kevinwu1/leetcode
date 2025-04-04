package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object LowestCommonAncestorofDeepestLeaves1123 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    var doLog = false
    def println(a: Any): Unit =
      if (doLog)
        Console.out.println(a)
    def lcaDeepestLeaves(root: TreeNode): TreeNode = {
      def helper(tn: TreeNode, depth: Int): (Int, TreeNode) = {
        val (ldepth, llca) =
          Option(tn.left).map(helper(_, depth + 1)).getOrElse(-1, tn)
        val (rdepth, rlca) =
          Option(tn.right).map(helper(_, depth + 1)).getOrElse(-1, tn)
        if (ldepth == rdepth && ldepth == -1) (depth, tn)
        else if (ldepth == rdepth) (ldepth, tn)
        else if (ldepth < rdepth) (rdepth, rlca)
        else (ldepth, llca)
      }
      helper(root, 0)._2
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {
    Solution.doLog = true
    Solution
      .lcaDeepestLeaves(
        treeifyOpt(
          parseArrayIntOrNull(
            "[3,5,1,6,2,0,8,null,null,7,4]"
          )
        )
      )
      .pipe(x => pprintln(x.value))
    Solution
      .lcaDeepestLeaves(
        treeifyOpt(
          parseArrayIntOrNull(
            "[1]"
          )
        )
      )
      .pipe(x => pprintln(x.value))
    Solution
      .lcaDeepestLeaves(
        treeifyOpt(
          parseArrayIntOrNull(
            "[0,1,3,null,2]"
          )
        )
      )
      .pipe(x => pprintln(x.value))
  }

}
