package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object ProductoftheLastKNumbers1352 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  class ProductOfNumbers() {
    var ad = scala.collection.mutable.ArrayBuffer[(Int, Int)]()
    var sinceZero = Int.MaxValue / 2
    def add(num: Int): Unit = {
      if (num == 0) {
        sinceZero = 0
        ad = scala.collection.mutable.ArrayBuffer[(Int, Int)]()
      } else if (num == 1 && ad.nonEmpty && ad.last._1 == 1) {
        ad.update(ad.size - 1, (1, ad.last._2 + 1))
        sinceZero += 1
      } else {
        ad += ((num, 1))
        sinceZero += 1
      }

    }

    def getProduct(k: Int): Int = {
      if (k > sinceZero)
        0
      else {
        var ans = 1
        var left = k
        var ind = ad.size - 1
        while (left > 0) {
          val (value, count) = ad(ind)
          ans *= value
          left -= count
          ind -= 1
        }
        ans
      }
    }

  }

  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {
    val queries = Array(
      "add",
      "getProduct",
      "getProduct",
      "getProduct",
      "add",
      "add",
      "add"
    )
    val values = parseArrayArrayInt("[[1],[1],[1],[1],[7],[6],[7]]")
    val pon = new ProductOfNumbers()
    queries
      .zip(values)
      .map({
        case ("add", Array(v)) =>
          pon.add(v)
          null
        case ("getProduct", Array(v)) =>
          pon
            .getProduct(v)
            .tap(println)
      })
      .pipe(pprintln(_))
  }

}
