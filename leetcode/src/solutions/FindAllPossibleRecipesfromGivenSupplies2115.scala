package solutions

import leetcode.macros.Macros.logged
import pprint.pprintln
import solutions.Util.*

import scala.util.chaining.*

@scala.annotation.experimental
object FindAllPossibleRecipesfromGivenSupplies2115 {
  // vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
  object Solution {
    def findAllRecipes(
        recipes: Array[String],
        ingredients: List[List[String]],
        supplies: Array[String]
    ): List[String] = {
      val ingrMap = ingredients
        .zip(recipes)
        .map({ case (values, key) => key -> values.toSet })
        .toMap
      val supSet = supplies.toSet
      import scala.collection.mutable
      val mem = mutable.Map[String, Boolean]()
      def isMakeable(ing: String, calls: Set[String] = Set()): Boolean = {
        mem.getOrElseUpdate(
          ing,
          supSet.contains(ing) ||
            !calls.contains(ing) &&
            ingrMap
              .get(ing)
              .map(_.forall(x => isMakeable(x, calls + ing)))
              .getOrElse(supSet.contains(ing))
        )
      }
      recipes.filter(r => isMakeable(r)).toList
    }
  }

  // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  def main(args: Array[String]): Unit = {

    // Solution
    //   .findAllRecipes(
    //     parseArrayString(
    //       """["bread"]"""
    //     ),
    //     parseArrayArrayString(
    //       """[["yeast","flour"]]"""
    //     ).map(_.toList).toList,
    //     parseArrayString(
    //       """["yeast","flour","corn"]"""
    //     )
    //   )
    //   .pipe(pprintln(_))

    // Solution
    //   .findAllRecipes(
    //     parseArrayString(
    //       """["bread","sandwich"]"""
    //     ),
    //     parseArrayArrayString(
    //       """[["yeast","flour"],["bread","meat"]]"""
    //     ).map(_.toList).toList,
    //     parseArrayString(
    //       """["yeast","flour","meat"]"""
    //     )
    //   )
    //   .pipe(pprintln(_))

    // Solution
    //   .findAllRecipes(
    //     parseArrayString(
    //       """["bread","sandwich","burger"]"""
    //     ),
    //     parseArrayArrayString(
    //       """[["yeast","flour"],["bread","meat"],["sandwich","meat","bread"]]"""
    //     ).map(_.toList).toList,
    //     parseArrayString(
    //       """["yeast","flour","meat"]"""
    //     )
    //   )
    //   .pipe(pprintln(_))
    // Solution
    //   .findAllRecipes(
    //     parseArrayString(
    //       """["ju","fzjnm","x","e","zpmcz","h","q"]"""
    //     ),
    //     parseArrayArrayString(
    //       """[["d"],["hveml","f","cpivl"],["cpivl","zpmcz","h","e","fzjnm","ju"],["cpivl","hveml","zpmcz","ju","h"],["h","fzjnm","e","q","x"],["d","hveml","cpivl","q","zpmcz","ju","e","x"],["f","hveml","cpivl"]]"""
    //     ).map(_.toList).toList,
    //     parseArrayString(
    //       """["f","hveml","cpivl","d"]"""
    //     )
    //   )
    //   .pipe(pprintln(_))
    Solution
      .findAllRecipes(
        parseArrayString(
          """["xevvq","izcad","p","we","bxgnm","vpio","i","hjvu","igi","anp","tokfq","z","kwdmb","g","qb","q","b","hthy"]"""
        ),
        parseArrayArrayString(
          """[["wbjr"],["otr","fzr","g"],["fzr","wi","otr","xgp","wbjr","igi","b"],["fzr","xgp","wi","otr","tokfq","izcad","igi","xevvq","i","anp"],["wi","xgp","wbjr"],["wbjr","bxgnm","i","b","hjvu","izcad","igi","z","g"],["xgp","otr","wbjr"],["wbjr","otr"],["wbjr","otr","fzr","wi","xgp","hjvu","tokfq","z","kwdmb"],["xgp","wi","wbjr","bxgnm","izcad","p","xevvq"],["bxgnm"],["wi","fzr","otr","wbjr"],["wbjr","wi","fzr","xgp","otr","g","b","p"],["otr","fzr","xgp","wbjr"],["xgp","wbjr","q","vpio","tokfq","we"],["wbjr","wi","xgp","we"],["wbjr"],["wi"]]"""
        ).map(_.toList).toList,
        parseArrayString(
          """["wi","otr","wbjr","fzr","xgp"]"""
        )
      )
      .pipe(pprintln(_))
  }

}
