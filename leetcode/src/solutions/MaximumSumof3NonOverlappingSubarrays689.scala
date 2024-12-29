package solutions

import leetcode.macros.Macros.logged
import solutions.Util._

import scala.util.chaining._

object MaximumSumof3NonOverlappingSubarrays689 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def maxSumOfThreeSubarrays(nums: Array[Int], k: Int): Array[Int] = {
      val n = nums.length
      val subarraySums: Array[Int] = {
        import scala.collection.mutable
        val answer = mutable.ArrayBuffer.fill(n + 1)(0)
        for (i <- 0 until n) {
          answer(i + 1) = answer(i) + nums(i)
        }
        answer.toArray
      }
      // println(subarraySums.mkString(","))

      import scala.collection.mutable
      val memo = mutable.Map[(Int, Int), (Int, Set[Int])]()

      // @logged
      def getBest(
          fromIndex: Int,
          remainingChoices: Int
      ): (Int, Set[Int]) = {
        memo.getOrElseUpdate(
          (fromIndex, remainingChoices), {
            // println("Updating")
            if (fromIndex + k > n || remainingChoices == 0) {
              (0, Set())
            } else {
              val (chooseStart, chosenInds) =
                getBest(fromIndex + k, remainingChoices - 1)
              val valueFromChoose = chooseStart + subarraySums(fromIndex + k) -
                subarraySums(fromIndex)
              val (dontChooseStart, dontChooseStartInds) =
                getBest(fromIndex + 1, remainingChoices)
              if (valueFromChoose >= dontChooseStart) {
                (
                  valueFromChoose,
                  chosenInds + fromIndex
                )
              } else
                (dontChooseStart, dontChooseStartInds)
            }

          }
        )
      }
      getBest(0, 3)._2.toArray.sorted
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  object Sol2 {
    object Solution {
      def maxSumOfThreeSubarrays(nums: Array[Int], k: Int): Array[Int] = {
        val n = nums.length
        val subarraySums: Array[Int] = {
          import scala.collection.mutable
          val answer = mutable.ArrayBuffer.fill(n + 1)(0)
          for (i <- 0 until n) {
            answer(i + 1) = answer(i) + nums(i)
          }
          answer.toArray
        }
        // println(subarraySums.mkString(","))

        import scala.collection.mutable
        val dp = mutable.ArrayBuffer.fill(n + 1, 4)((0, Set[Int]()))

        for (
          fromIndex <- (0 until n).reverse;
          remainingChoices <- (1 to 3)
        ) {
          dp(fromIndex)(remainingChoices) = {
            if (fromIndex + k > n || remainingChoices == 0) {
              (0, Set())
            } else {
              val (chooseStart, chosenInds) =
                dp(fromIndex + k)(remainingChoices - 1)
              val valueFromChoose =
                chooseStart + subarraySums(fromIndex + k) -
                  subarraySums(fromIndex)
              val (dontChooseStart, dontChooseStartInds) =
                dp(fromIndex + 1)(remainingChoices)
              if (valueFromChoose >= dontChooseStart) {
                (
                  valueFromChoose,
                  chosenInds + fromIndex
                )
              } else
                (dontChooseStart, dontChooseStartInds)
            }
          }
        }
        println(dp.map(_.mkString(",")).mkString("\n"))
        dp(0)(3)._2.toArray.sorted
      }
    }
  }
// ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  object Sol3 {
    object Solution {
      def maxSumOfThreeSubarrays(nums: Array[Int], k: Int): Array[Int] = {
        var (indexes0, indexes1, indexes2) = (0, k, k * 2)
        var (sums0, sums1, sums2) = (
          nums.view.slice(0, 0 + k).sum,
          nums.view.slice(k, k + k).sum,
          nums.view.slice(2 * k, 2 * k + k).sum
        )
        var bestIndexes0 = indexes0
        var bestIndexes1 = Array(indexes0, indexes1)
        var bestIndexes2 = Array(indexes0, indexes1, indexes2)
        var (bestSums0, bestSums1, bestSums2) =
          (sums0, sums0 + sums1, sums0 + sums1 + sums2)
        indexes0 += 1
        indexes1 += 1
        indexes2 += 1
        while (indexes2 + k <= nums.length) {
          sums0 += nums(indexes0 + k - 1) - nums(indexes0 - 1)
          sums1 += nums(indexes1 + k - 1) - nums(indexes1 - 1)
          sums2 += nums(indexes2 + k - 1) - nums(indexes2 - 1)
          if (sums0 > bestSums0) {
            bestSums0 = sums0
            bestIndexes0 = indexes0
          }
          if (bestSums0 + sums1 > bestSums1) {
            bestSums1 = bestSums0 + sums1
            bestIndexes1 = Array(bestIndexes0, indexes1)
          }
          if (bestSums1 + sums2 > bestSums2) {
            bestSums2 = bestSums1 + sums2
            bestIndexes2 = bestIndexes1 :+ indexes2
          }
          indexes0 += 1
          indexes1 += 1
          indexes2 += 1
        }
        bestIndexes2
      }
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  def main(args: Array[String]): Unit = {

    Sol3.Solution
      .maxSumOfThreeSubarrays(
        parseArrayInt(
          "[1,2,1,2,6,7,5,1]"
        ),
        2
      )
      .mkString(",")
      .pipe(println)
  }

}
