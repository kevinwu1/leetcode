package solutions

object Util {
  def parseArrayIntOrNull(s: String): Array[Option[Int]] = {
    s.stripPrefix("[")
      .stripSuffix("]")
      .split(",")
      .map(x => if (x == "null") None else Some(x.toInt))
  }
  def parseArrayInt(s: String): Array[Int] = {
    if (s.isEmpty())
      Array()
    else
      s.stripPrefix("[").stripSuffix("]").split(",").map(_.toInt)
  }
  def parseArrayString(s: String): Array[String] = {
    s.stripPrefix("[\"").stripSuffix("\"]").split("\",\"")
  }
  def parseArrayArrayInt(s: String): Array[Array[Int]] = {
    import util.chaining.scalaUtilChainingOps
    if (s == "[]")
      Array()
    else
      s.stripPrefix("[[")
        .stripSuffix("]]")
        .split(raw"],\[", -1)
        .map(arr => parseArrayInt(arr))
  }

  class TreeNode(
      _value: Int = 0,
      _left: TreeNode = null,
      _right: TreeNode = null
  ) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  def treeify(data: Array[Int], from: Int = 0): TreeNode = {
    val n = new TreeNode(_value = data(from))
    if (from * 2 + 1 < data.length) {
      n.left = treeify(data, from * 2 + 1)
      n.right = treeify(data, from * 2 + 2)
    }
    n
  }
  def treeifyOpt(data: Array[Option[Int]], from: Int = 0): TreeNode = {
    if (data(from) == None)
      null
    else {
      val n = new TreeNode(_value = data(from).get)
      if (from * 2 + 1 < data.length) {
        n.left = treeifyOpt(data, from * 2 + 1)
        n.right = treeifyOpt(data, from * 2 + 2)
      }
      n
    }
  }
  implicit class tnimplicits2(tn: TreeNode) {
    def pretty: String = {
      if (tn.left == null) {
        tn.value.toString()
      } else
        s"[${tn.left.pretty}|${tn.value}|${tn.right.pretty}]"
    }
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
