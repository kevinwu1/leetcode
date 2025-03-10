package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object CountofSubstringsContainingEveryVowelandKConsonantsII3306 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    val letToInd: Array[Int] = new Array[Int](26)
    letToInd('a' - 'a') = 1
    letToInd('e' - 'a') = 2
    letToInd('i' - 'a') = 3
    letToInd('o' - 'a') = 4
    letToInd('u' - 'a') = 5
    def countOfSubstrings(word: String, k: Int): Long = {
      extension (counts: Array[Int]) {
        def addLetter(l: Char): Unit = {
          counts(letToInd(l - 'a')) += 1
        }
        def remLetter(l: Char): Unit = {
          counts(letToInd(l - 'a')) -= 1
        }
        def cons: Int = counts(0)
        def isKcons: Boolean = cons == k
        def isValid: Boolean = counts(0) == k &&
          counts(1) >= 1 &&
          counts(2) >= 1 &&
          counts(3) >= 1 &&
          counts(4) >= 1 &&
          counts(5) >= 1
      }

      class Window(_l: Int = 0) {
        var leftEdge = _l
        val counts: Array[Int] = new Array(6)
        def shrink(): Unit = {
          counts.remLetter(word(leftEdge))
          leftEdge += 1
        }
        def myclone(): Window = {
          val w = new Window(leftEdge)
          w.copyOf(this)
          w
        }
        def copyOf(w: Window): Unit = {
          leftEdge = w.leftEdge
          counts(0) = w.counts(0)
          counts(1) = w.counts(1)
          counts(2) = w.counts(2)
          counts(3) = w.counts(3)
          counts(4) = w.counts(4)
          counts(5) = w.counts(5)
        }
        def isValid: Boolean = counts.isValid
        def cons: Int = counts.cons
      }

      var r = 0
      var tot: Long = 0L
      var ll = new Window() // for a given r, ll is the largest valid window
      var lr =
        new Window() // for a given r, lr is the smallest valid window, but with index + 1 to make calculation easier

      while (r < word.size) {
        // grow the windows
        ll.counts.addLetter(word(r))
        lr.counts.addLetter(word(r))
        r += 1

        // Then count the # of valid substrings
        if (!ll.isValid) {
          if (ll.cons > k) { // added a consonant
            // shrink until we have the right # of consonants
            while (ll.cons > k)
              ll.shrink()
            // setup lr so that if the next grow makes the window valid, we can start using it
            lr.copyOf(ll)
            // if shrinking ll caused it to be valid, then setup lr and count
            if (ll.isValid) {
              lr.copyOf(ll)
              while (lr.isValid) {
                lr.shrink()
              }
              tot += lr.leftEdge - ll.leftEdge
            }
          }
        } else { // ll is valid
          // don't want to do lr.copyOf(ll) here so that we can reuse the previous lr for when there are many vowels in a row
          while (lr.isValid) {
            lr.shrink()
          }
          tot += lr.leftEdge - ll.leftEdge
        }
      }

      tot
    }
  }
  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    Solution
      .countOfSubstrings(
        os.read(os.pwd / "input.txt"),
        0
      )
      .pipe(pprintln(_))
    // Solution
    //   .countOfSubstrings(
    //     "aeioqq",
    //     1
    //   )
    //   .pipe(pprintln(_))
    // Solution
    //   .countOfSubstrings(
    //     "aeiou",
    //     0
    //   )
    //   .pipe(pprintln(_))
    // Solution
    //   .countOfSubstrings(
    //     "ieaouqqieaouqq",
    //     1
    //   )
    //   .pipe(pprintln(_))
    Solution
      .countOfSubstrings(
        "buoeia",
        0
      )
      .pipe(pprintln(_))
    Solution
      .countOfSubstrings(
        "aoaiuefi",
        1
      )
      .pipe(pprintln(_))
  }

}
