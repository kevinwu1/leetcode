package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object NumberofWaystoArriveatDestination1976 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    import scala.collection.mutable.PriorityQueue
    def countPaths(n: Int, roads: Array[Array[Int]]): Int = {
      if (n == 1) return 1
      val MOD = 1000000007
      case class Con(from: Int, to: Int, weight: Int)
      val connections = new Array[Vector[Con]](n)
      roads
        .flatMap({ case Array(a, b, w) =>
          List(Con(a, b, w), Con(b, a, w))
        })
        .groupBy(_.from)
        .foreach({ case (k, cons) =>
          connections(k) = cons.toVector
        })
      var done = false
      val ways = new Array[Int](n)
      val cost = Array.fill(n)(Int.MaxValue)
      ways(0) = 1
      import scala.collection.mutable
      case class Node(node: Int, weight: Int, ways: Int)
      val costMap = mutable.Map[Int, mutable.ArrayBuffer[Node]]()
      costMap(0) = mutable.ArrayBuffer(Node(0, 0, 1))
      val pq = new PriorityQueue[(Int)]()(Ordering.Int.reverse)
      pq.enqueue(0)
      val visited = mutable.Set[Int]()
      while (pq.nonEmpty) {
        val icost = pq.dequeue()
        if (costMap.contains(icost)) {
          val nodes = costMap(icost)
          costMap.remove(icost)
          val compressedNodes: Map[Int, Int] =
            nodes.groupMapReduce(_.node)(_.ways)((x, y) => (x + y) % MOD)
          compressedNodes.foreach({ case (node, iways) =>
            if (!visited.contains(node)) {
              visited += node
              if (cost(node) < icost) {
                // do nothing
              } else {
                // println(s"Exploring node ${node} cost ${icost}, ways: ${iways}")
                if (icost < cost(node)) {
                  cost(node) = icost
                  ways(node) = iways
                } else if (icost == cost(node)) {
                  ways(node) += iways
                  ways(node) %= MOD
                }
                connections(node)
                  .filter(c => !visited.contains(c.to))
                  .foreach({ case Con(_, to, weight) =>
                    val totalCost = icost + weight
                    // println(s"Connection to ${to} costing ${totalCost}")
                    if (!costMap.contains(totalCost))
                      costMap(totalCost) = mutable.ArrayBuffer()
                    costMap(totalCost) += Node(to, totalCost, iways)
                    pq.enqueue(totalCost)
                  })
              }
            }
          })
        }
      }
      // println(cost.mkString(","))
      // println(ways.mkString(","))
      ways(n - 1)
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      import scala.collection.mutable.PriorityQueue
      def countPaths(n: Int, roads: Array[Array[Int]]): Int = {
        if (n == 1) return 1
        val MOD = 1000000007
        case class Con(from: Int, to: Int, weight: Int)
        val connections = new Array[Vector[Con]](n)
        roads
          .flatMap({ case Array(a, b, w) =>
            List(Con(a, b, w), Con(b, a, w))
          })
          .groupBy(_.from)
          .foreach({ case (k, cons) =>
            connections(k) = cons.toVector
          })
        var done = false
        val ways = new Array[Int](n)
        val cost = Array.fill(n)(Int.MaxValue)
        ways(0) = 1
        import scala.collection.mutable
        case class Node(node: Int, weight: Int, ways: Int)
        val pq = new PriorityQueue[Node]()(
          Ordering.fromLessThan[Node]((n1, n2) => n1.weight < n2.weight).reverse
        )
        val visited = mutable.Set[Int]()
        pq.enqueue(Node(0, 0, 1))
        while (pq.nonEmpty) {
          val icost = pq.head.weight
          val nodes = new mutable.ArrayBuffer[Node]()
          while (pq.nonEmpty && pq.head.weight == icost)
            nodes += pq.dequeue()

          val compressedNodes: Map[Int, Int] =
            nodes.groupMapReduce(_.node)(_.ways)((x, y) => (x + y) % MOD)
          compressedNodes.foreach({ case (node, iways) =>
            if (!visited.contains(node)) {
              visited += node
              if (cost(node) < icost) {
                // do nothing
              } else {
                // println(s"Exploring node ${node} cost ${icost}, ways: ${iways}")
                if (icost < cost(node)) {
                  cost(node) = icost
                  ways(node) = iways
                } else if (icost == cost(node)) {
                  ways(node) += iways
                  ways(node) %= MOD
                }
                connections(node)
                  .filter(c => !visited.contains(c.to))
                  .foreach({ case Con(_, to, weight) =>
                    val totalCost = icost + weight
                    // println(s"Connection to ${to} costing ${totalCost}")
                    pq.enqueue(Node(to, totalCost, iways))
                  })
              }
            }
          })
        }
        // println(cost.mkString(","))
        // println(ways.mkString(","))
        ways(n - 1)
      }
    }
  }
  def main(args: Array[String]): Unit = {

    Sol2.Solution
      .countPaths(
        7,
        parseArrayArrayInt(
          "[[0,6,7],[0,1,2],[1,2,3],[1,3,3],[6,3,3],[3,5,1],[6,5,1],[2,5,1],[0,4,5],[4,6,2]]"
        )
      )
      .pipe(pprintln(_))
  }

}
