package build
import mill._

import scalalib._

object leetcode extends ScalaModule {
  def scalaVersion = "2.13.15"
  def ivyDeps = Agg(
    ivy"com.lihaoyi::scalatags:0.12.0",
    ivy"com.lihaoyi::mainargs:0.6.2",
    ivy"org.scala-lang::toolkit:0.6.0"
  )

  object macros extends ScalaModule {
    def scalaVersion = "2.13.15"
  }
}
