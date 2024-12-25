package solutions

import leetcode.macros.Macros.logged

object TestingMacro {
  @logged
  def doStuff(a: Int): Int = {
    a + 2222
  }

  def main(args: Array[String]): Unit = {
    println(doStuff(2))
  }
}
