package solutions

import leetcode.macros.Macros.logged
import os.makeDir.all
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object WordSubsets916 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def wordSubsets(
        words1: Array[String],
        words2: Array[String]
    ): List[String] = {
      def getLetterCount(word: String): Map[Char, Int] = {
        word
          .groupBy(identity)
          .map({ case (l, cs) => l -> cs.size })
          .withDefaultValue(0)
      }
      val allLetterCount = words2
        .map(getLetterCount)
        .reduce((m1, m2) => {
          val keys = m1.keySet ++ m2.keySet
          keys.map(k => k -> Math.max(m1(k), m2(k))).toMap.withDefaultValue(0)
        })
      words1
        .filter(word1 => {
          val wlc1 = getLetterCount(word1)
          allLetterCount.forall({ case (k, v) =>
            wlc1(k) >= v
          })
        })
        .toList
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol2 {
    object Solution {
      def wordSubsets(
          words1: Array[String],
          words2: Array[String]
      ): List[String] = {
        val nletters = 26

        inline def getLetterCount(word: String, tmp: Array[Int]): Unit = {
          var j = 0
          while (j < word.length) {
            tmp(word(j) - 'a') += 1
            j += 1
          }
        }
        val allLetterCount = {
          var i = 0
          import scala.collection.mutable
          var ans = Array.fill(nletters)(0)
          var tmp = Array.fill(nletters)(0)
          while (i < words2.length) {
            getLetterCount(words2(i), tmp)
            var tind = 0
            while (tind < nletters) {
              ans(tind) = Math.max(ans(tind), tmp(tind))
              tmp(tind) = 0
              tind += 1
            }
            i += 1
          }
          ans
        }

        var finalAns = List[String]()
        var i = 0
        while (i < words1.length) {
          val word = words1(i)
          val tmp = Array.fill(nletters)(0)
          getLetterCount(word, tmp)

          var wasGood = true
          var letInd = 0
          while (letInd < nletters) {
            if (tmp(letInd) < allLetterCount(letInd)) {
              wasGood = false
              letInd = nletters
            }
            letInd += 1
          }
          if (wasGood) {
            finalAns = word :: finalAns
          }
          i += 1
        }
        finalAns
      }
    }
  }
  def main(args: Array[String]): Unit = {

    Sol2.Solution
      .wordSubsets(
        parseArrayString(
          """["acaac","cccbb","aacbb","caacc","bcbbb"]"""
        ),
        parseArrayString(
          """["c","cc","b"]"""
        )
      )
      .pipe(println)
  }

}
