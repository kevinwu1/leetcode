package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object DesignaNumberContainerSystem2349 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  class NumberContainers() {
    import scala.collection.mutable
    val indexToNumber = mutable.Map[Int, Int]()
    val tm = mutable.Map[Int, mutable.SortedSet[Int]]()
    def change(index: Int, number: Int): Unit = {
      if (indexToNumber.contains(index)) {
        tm(indexToNumber(index)) -= index
      }
      indexToNumber(index) = number
      if (!tm.contains(number))
        tm(number) = mutable.SortedSet[Int]()
      tm(number) += index
    }

    def find(number: Int): Int = {
      tm.get(number).flatMap(_.headOption).getOrElse(-1)
    }
  }

  /** Your NumberContainers object will be instantiated and called as such: val
    * obj = new NumberContainers() obj.change(index,number) val param_2 =
    * obj.find(number)
    */
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {
    val nc = new NumberContainers()
    val queries = Array(
      "find",
      "change",
      "change",
      "change",
      "change",
      "find",
      "change",
      "find"
    )
    val params = parseArrayArrayInt(
      "[[10],[2,10],[1,10],[3,10],[5,10],[10],[1,20],[10]]"
    )
    queries
      .zip(params)
      .map({
        case ("find", Array(number)) => nc.find(number)
        case ("change", Array(index, number)) => {
          nc.change(index, number)
          null
        }
      })
      .pipe(pprintln(_))
  }

}
