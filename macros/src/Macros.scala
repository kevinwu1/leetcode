package leetcode.macros

import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object Macros {
  @compileTimeOnly("enable macro paradise to expand macro annotations")
  class logged extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro loggedMacro.impl
  }

  object loggedMacro {
    def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._
      val inputs = annottees.map(_.tree).toList
      val tree = annottees
      val transformed = inputs match {
        case (q"$mods def $name[..$tparams](...$paramss): $tpt = $rhs") :: _ => {
          val params = paramss.flatten.map(vd => {
            vd.name.toString
          })
          val printParam =
            s"println(leetcode.macros.Macros.indent + s\"${name.toString}(${params
                .map(p => p + "=${pprint.apply(" + p + ").render}")
                .mkString(", ")})\")"

          val printParam2 =
            s"println(leetcode.macros.Macros.indent + s\"${name.toString}(${params
                .map(p => p + "=${pprint.apply(" + p + ").render}")
                .mkString(", ")}) = \" + macro_finalanswer)"

          val fname = s"\"${name.toString}\""
          val res = q"""$mods def $name[..$tparams](...$paramss): $tpt = {
            ${c.parse(printParam)}
            leetcode.macros.Macros.indentation += 1
            val macro_finalanswer: $tpt={${rhs}}
            leetcode.macros.Macros.indentation -= 1
            ${c.parse(printParam2)}
            macro_finalanswer
        }"""
          println("Macro applied")
          println(show(res))
          c.Expr(res)
        }
        case x => {
          println(x.head.getClass())
          annottees.head
        }
      }
      transformed
    }
  }
  var indentation = 0
  def indent: String = "  " * indentation
  def indentPrintln(s: Any): Unit = println(indent + s)

  def a(s: Any): String = {
    pprint.apply(Array(Array(1, 2), Array(3, 4, 5, 6, 7))).render
  }

  def main(args: Array[String]): Unit = println(a(3))
}
