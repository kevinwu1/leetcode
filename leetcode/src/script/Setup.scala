package script
object Setup {
  def main(args: Array[String]): Unit = {
    val name =
      (args.tail :+ args.head).mkString.replace(".", "").replace("-", "")
    os.write(
      os.pwd / "leetcode" / "src" / "solutions" / s"$name.scala",
      s"""package solutions

import leetcode.macros.Macros.logged
import solutions.Util._

import scala.util.chaining._

@scala.annotation.experimental
object $name {
  //vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  
  //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
  def main(args: Array[String]): Unit = {

    Solution
      .___(
        parseArrayInt(
          ""
        ),
        parseArrayArrayInt(
          ""
        ),
        parseArrayString(
          \"\"\"\"\"\"
        )
      )
      .pipe(println)
  }

}
"""
    )
  }
}
