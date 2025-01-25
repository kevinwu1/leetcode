package solutions

import leetcode.macros.Macros.logged
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object FindEventualSafeStates802 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def eventualSafeNodes(graph: Array[Array[Int]]): List[Int] = {
      import scala.collection.mutable
      val inEdges =
        Array.fill(graph.size)(mutable.ArrayBuffer[Int]())
      var from = 0
      while (from < graph.size) {
        val tos = graph(from)
        var i = 0
        while (i < tos.size) {
          inEdges(tos(i)) += from
          i += 1
        }
        from += 1
      }
      val outCounts = graph.map(_.size)
      val queue = new mutable.Queue[Int]()
      var ind = 0
      while (ind < graph.size) {
        val outs = graph(ind)
        if outs.isEmpty then queue += ind
        ind += 1
      }
      val ans = new mutable.ArrayBuffer[Int]()
      while (queue.nonEmpty) {
        val item = queue.dequeue()
        ans += item
        val ins = inEdges(item)
        var i = 0
        while (i < ins.size) {
          val in = ins(i)
          outCounts(in) -= 1
          if (outCounts(in) == 0)
            queue += in
          i += 1
        }
      }
      ans.sortInPlace()
      ans.toList
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def eventualSafeNodes(graph: Array[Array[Int]]): List[Int] = {
        import scala.collection.mutable
        val isSafeArr = Array.fill(graph.size)(-1)
        def isSafe(i: Int): Boolean = {
          if (isSafeArr(i) != -1)
            isSafeArr(i) != 0
          else {
            isSafeArr(i) = 0
            if (graph(i).forall(isSafe(_)))
              isSafeArr(i) = 1
              true
            else {
              isSafeArr(i) = 0
              false
            }
          }
        }
        var ans = List[Int]()
        var i = graph.size - 1
        while (i >= 0) {
          if (isSafe(i))
            ans = i :: ans
          i -= 1
        }
        ans
      }
    }
  }
  def main(args: Array[String]): Unit = {
    Sol2.Solution
      .eventualSafeNodes(
        parseArrayArrayInt(
          "[[1,2,3,4],[1,2],[3,4],[0,4],[]]"
        )
      )
      .pipe(println)
  }

}
