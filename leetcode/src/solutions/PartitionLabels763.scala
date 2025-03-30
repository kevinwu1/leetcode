package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object PartitionLabels763 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    var doLog = false
    def println(a: Any): Unit =
      if (doLog)
        Console.out.println(a)
    def partitionLabels(s: String): List[Int] = {
      val count = new Array[Int](26)
      s.foreach(c => count(c - 'a') += 1)
      s.foldLeft(
        (Vector[Int](), new Array[Int](26), 0)
      )({ case ((ans, letters, len), let) =>
        letters(let - 'a') += 1
        if ((0 until 26).forall(i => letters(i) == 0 || letters(i) == count(i)))
          (ans :+ len + 1, new Array[Int](26), 0)
        else
          (ans, letters, len + 1)
      })._1
        .toList
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def partitionLabels(s: String): List[Int] = {
        val count = new Array[Int](26)
        var i = 0
        while (i < s.length()) {
          count(s(i) - 'a') += 1
          i += 1
        }
        var ans = List[Int]()
        var j = s.length() - 1
        var letters = 0
        var len = 0
        var unfinished = 0
        while (j >= 0) {
          val letter = s(j) - 'a'
          if ((letters & (1 << letter)) == 0 && count(letter) != 0) {
            unfinished += 1
            letters |= (1 << letter)
          }
          if (count(letter) == 1)
            unfinished -= 1
          count(letter) -= 1
          if (unfinished == 0) {
            ans = (len + 1) :: ans
            len = 0
          } else {
            len += 1
          }
          j -= 1
        }
        ans
      }
    }
  }
  def main(args: Array[String]): Unit = {
    Solution.doLog = true
    Sol2.Solution
      .partitionLabels(
        "ababcbacadefegdehijhklij"
      )
      .pipe(pprintln(_))
  }

}
