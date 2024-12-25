package solutions

object Util {
  def parseArrayIntOrNull(s: String): Array[Option[Int]] = {
    s.stripPrefix("[")
      .stripSuffix("]")
      .split(",")
      .map(x => if (x == "null") None else Some(x.toInt))
  }
  def parseArrayInt(s: String): Array[Int] = {
    s.stripPrefix("[").stripSuffix("]").split(",").map(_.toInt)
  }
  def parseArrayArrayInt(s: String): Array[Array[Int]] = {
    s.stripPrefix("[[")
      .stripSuffix("]]")
      .split(raw"],\[")
      .map(arr => parseArrayInt(arr))
  }

  import scala.annotation.tailrec
  @tailrec
  def bsearchRange(lo: Int, hi: Int, trueIfHigher: Int => Boolean): Int = {
    if (lo + 1 == hi)
      hi
    else {
      val mid = (lo + hi) / 2
      if (trueIfHigher(mid))
        bsearchRange(mid, hi, trueIfHigher)
      else
        bsearchRange(lo, mid, trueIfHigher)
    }
  }
}
