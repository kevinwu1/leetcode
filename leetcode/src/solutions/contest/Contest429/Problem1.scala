package solutions.contest.Contest429

object Problem1 {
  object Solution {
    def minimumOperations(nums: Array[Int]): Int = {
      var t = 0
      var n = nums.clone()
      while (n.toSet.size != n.size) {
        t += 1
        n = n.drop(3)
      }
      t
    }
  }
}
