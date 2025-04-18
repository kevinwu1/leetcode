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
    s.stripPrefix("[\"")
      .stripSuffix("\"]")
      .split(",")
      .map(_.stripPrefix("\"").stripSuffix("\""))
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
  def parseArrayArrayString(s: String): Array[Array[String]] = {
    import util.chaining.scalaUtilChainingOps
    if (s == "[]")
      Array()
    else
      s.stripPrefix("[")
        .stripSuffix("]")
        .split(raw"],\[", -1)
        .map(arr => parseArrayString(arr))
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

  def printTree(
      tn: TreeNode,
      parent: Int = -1,
      childSide: String = "root"
  ): Unit = {
    println(s"$parent - $childSide - ${tn.value}")
    if (tn.left != null) {
      printTree(tn.left, tn.value, "left")
    }
    if (tn.right != null) {
      printTree(tn.right, tn.value, "right")
    }
  }

  import scala.annotation.tailrec
  @tailrec
  def bsearchRange(lo: Int, hi: Int, trueIfHigher: Int => Boolean): Int = {
    if (lo + 1 >= hi)
      hi
    else {
      val mid = (lo + hi) / 2
      if (trueIfHigher(mid))
        bsearchRange(mid, hi, trueIfHigher)
      else
        bsearchRange(lo, mid, trueIfHigher)
    }
  }

  class UnionFind(n: Int) {
    val rep_ = (0 until n).toArray
    val size = Array.fill(n)(1)

    def union(a: Int, b: Int): Int = {
      val ra = find(a)
      val rb = find(b)
      if (ra != rb) {
        val sizea = size(ra)
        val sizeb = size(rb)
        val (from, to) = if (sizea > sizeb) {
          (rb, ra)
        } else (ra, rb)
        size(to) += size(from)
        rep_(from) = to
        to
      } else {
        ra
      }
    }
    def find(a: Int): Int = {
      val r = rep_(a)
      if (a == r)
        r
      else {
        rep_(a) = find(r)
        rep_(a)
      }
    }
  }
}
