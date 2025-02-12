package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object RemoveAllOccurrencesofaSubstring1910 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def removeOccurrences(s: String, part: String): String = {
      Iterator.iterate(s)(_.replaceFirst(part, "")).find(!_.contains(part)).get
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .removeOccurrences(
        "hhvhvaahvahvhvaavhvaasshvahvaln",
        "hva"
      )
      .pipe(pprintln(_))
    // Solution
    //   .removeOccurrences(
    //     "axxxxyyyyb",
    //     "xy"
    //   )
    //   .pipe(pprintln(_))
  }

}
