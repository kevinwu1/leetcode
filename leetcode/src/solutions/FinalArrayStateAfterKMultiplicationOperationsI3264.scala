package solutions

object FinalArrayStateAfterKMultiplicationOperationsI3264 {

  object Solution {
    def finalPrices(prices: Array[Int]): Array[Int] = {
      for (i <- 0 until prices.length - 1) {
        val nextLowestInd = prices.indexWhere(j => j <= prices(i), i + 1)
        if (nextLowestInd != -1)
          prices.update(i, prices(i) - prices(nextLowestInd))
      }
      prices
    }
  }

}
