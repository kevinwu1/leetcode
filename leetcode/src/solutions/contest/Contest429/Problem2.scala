package solutions.contest.`429`

object Problem2 {
  object Solution {
    def maxDistinctElements(numsP: Array[Int], k: Int): Int = {
      val nums = numsP.sorted
      var nextMin = nums.head - k
      var extras = 0
      nums.foreach(n => {
        var toput = Math.max(nextMin, n - k)
        if (toput > n + k) {
          extras += 1
        } else {
          nextMin = toput + 1
        }
      })
      numsP.size - extras

    }
  }
}
