package solutions

import os.makeDir.all
import solutions.Util._

import scala.util.chaining._

object MaximumNumberofKDivisibleComponents2872 {
  object Solution {
    def maxKDivisibleComponents(
        n: Int,
        edgeArr: Array[Array[Int]],
        values: Array[Int],
        k: Int
    ): Int = {
      val allNodes = (0 until n).toVector
      val nodeToEdges: Array[Array[Int]] = {
        import scala.collection.mutable
        val ans = Array.fill(n)(mutable.ArrayBuffer[Int]())
        edgeArr.foreach { case Array(from, to) =>
          ans(from) += to
          ans(to) += from
        }
        ans.map(_.toArray)
      }
      implicit class nimplicits1(n: Int) {
        def edges: Array[Int] = nodeToEdges(n)
      }
      val levels: Array[Int] = {
        val leaves = Set(0)
        val visited: Set[Int] = Set()
        val answer: Array[Int] = Array.fill(n)(Int.MaxValue)
        def traverse(
            nodes: Set[Int],
            visited: Set[Int],
            level: Int
        ): Unit = {
          val newNodes = nodes.filter(!visited.contains(_))
          if (newNodes.nonEmpty) {
            newNodes
              .foreach(node => {
                answer.update(node, level)
              })
            val adjacents =
              nodes.flatMap(_.edges)
            traverse(adjacents, visited ++ newNodes, level + 1)
          }
        }
        traverse(leaves, Set(), 0)
        answer
      }
      implicit class nimplicits2(n: Int) {
        def level: Int = levels(n)
      }
      val levelSums: Array[Int] = {
        val answer: Array[Int] = Array.fill(n)(-1)
        allNodes
          .sortBy(-_.level)
          .foreach(n => {
            val nlvl = n.level
            val newanswer = values(n) % k + n.edges
              .collect({
                case e if e.level == nlvl + 1 => answer(e)
              })
              .reduceOption((a, b) => (a % k + b % k) % k)
              .getOrElse(0)
            answer.update(n, newanswer % k)
          })
        answer
      }

      implicit class nimplicits3(n: Int) {
        def levelSum: Int = levelSums(n)
      }

      edgeArr.count(e => {
        val Array(n1, n2) = e
        val subtree = if (n1.level < n2.level) n2 else n1
        subtree.levelSum % k == 0
      }) + 1
    }
  }

  object Sol2 {

    object Solution {
      def maxKDivisibleComponents(
          n: Int,
          edgeArr: Array[Array[Int]],
          values: Array[Int],
          k: Int
      ): Int = {
        val allNodes = (0 until n).toVector
        val nodeToEdges: Array[Array[Int]] = {
          import scala.collection.mutable
          val ans = Array.fill(n)(mutable.ArrayBuffer[Int]())
          edgeArr.foreach { case Array(from, to) =>
            ans(from) += to
            ans(to) += from
          }
          ans.map(_.toArray)
        }
        implicit class nimplicits1(n: Int) {
          def edges: Array[Int] = nodeToEdges(n)
        }
        def count(n: Int, parent: Int): (Int, Int) = {
          val childSum = n.edges
            .filter(_ != parent)
            .map(c => count(c, n))
            .reduceOption((a, b) => {
              val ((c1, t1), (c2, t2)) = (a, b)
              (c1 + c2, (t1 + t2) % k)
            })
            .getOrElse(0, 0)
          val total = (childSum._2 + values(n) % k) % k
          (childSum._1 + (if (total == 0) 1 else 0), total)
        }
        count(0, -1)._1
      }
    }
  }
  def main(args: Array[String]): Unit = {

    Sol2.Solution
      .maxKDivisibleComponents(
        5,
        parseArrayArrayInt(
          "[[0,2],[1,2],[1,3],[2,4]]"
        ),
        parseArrayInt(
          "[1,8,1,4,4]"
        ),
        6
      )
      .pipe(println)
  }
}
