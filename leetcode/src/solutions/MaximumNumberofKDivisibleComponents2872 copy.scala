package solutions

import os.makeDir.all
import solutions.Util._

import scala.util.chaining._

object FindBuildingWhereAliceandBobCanMeet2940 {
  object Solution {
    def leftmostBuildingQueries(
        heights: Array[Int],
        queries: Array[Array[Int]]
    ): Array[Int] = {
      implicit val IntMax: Monoid[Int] = new Monoid[Int]() {
        override val ident: Int = 0
        override def op(a: Int, b: Int): Int = Math.max(a, b)
      }
      val mt = new MyTree[Int](heights)
      queries.map(q => {
        val Array(query1, query2) = q
        val (qs, qe) =
          if (query1 < query2) (query1, query2) else (query2, query1)
        if (qs == qe)
          qe
        else if (heights(qs) < heights(qe)) {
          qe
        } else {
          mt.findLowestIndexFrom(qe, Math.max(heights(qs), heights(qe)))
        }
      })
    }

    trait Monoid[T] { outer =>
      val ident: T
      def op(a: T, b: T): T
    }
    implicit class MonExt[T: Monoid](t: T) {
      val mon = implicitly[Monoid[T]]
      def op(u: T): T = mon.op(t, u)
    }
    import scala.reflect.ClassTag
    class MyTree[T: Monoid: ClassTag](items: Array[T]) {
      val mon = implicitly[Monoid[T]]

      val originalLen = items.length
      val len = {
        var l = 1
        while (l < originalLen)
          l *= 2
        l
      }
      val bigLen = len * 2
      val heap = Array.fill(len)(mon.ident)
      implicit class indexImpl(ind: Int) {
        def leftChild: Int = ind * 2
        def rightChild: Int = ind * 2 + 1
        def isLeaf: Boolean = ind >= len
        def item: T = {
          if (ind >= len) {
            val oind = ind - len
            if (oind >= items.length)
              mon.ident
            else
              items(ind - len)
          } else
            heap(ind)
        }
        def segStart: Int = {
          var i = ind
          while (!i.isLeaf)
            i = i.leftChild
          i - len
        }
        def segEnd: Int = {
          var i = ind
          while (!i.isLeaf)
            i = i.rightChild
          i - len + 1
        }
        // 0 1 2 3 4 5 6 7
        //         a b c d
        //   a a c a b c d
        //   0 0 2 0 1 2 3
      }

      for (i <- (0 until len).reverse) {
        heap.update(i, i.leftChild.item op i.rightChild.item)
      }
      def findLowestIndexFrom(startInd: Int, el: T): Int = {
        // println(s"looking for $el from $startInd")
        def helper(node: Int): Option[(T, Int)] = {
          // println(
          //   s"Helper $node - [${node.segStart}, ${node.segEnd}) = ${node.item}"
          // )
          if (node.isLeaf) {
            if (node.segEnd <= startInd)
              None
            else {
              if ((node.item op el) != el) {
                Some((node.item, node - len))
              } else
                None
            }
          } else {
            if (node.segEnd <= startInd)
              None
            else {
              if ((node.item op el) != el) {
                val lr = helper(node.leftChild)
                if (lr.nonEmpty)
                  lr
                else
                  helper(node.rightChild)
              } else None
            }
          }
        }
        helper(1).map(_._2).getOrElse(-1)
      }
    }
  }
  def main(args: Array[String]): Unit = {
    Solution
      .leftmostBuildingQueries(
        parseArrayInt(
          "[6,4,8,5,2,7]"
        ),
        parseArrayArrayInt(
          "[[0,1],[0,3],[2,4],[3,4],[2,2]]"
        )
      )
      .mkString("\n")
      .pipe(println)
  }
}
