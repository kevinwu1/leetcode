package build
import mill._

import scalalib._

object Shared {
  val _scalaVersion = "3.3.1"
  val _ivyDeps = Agg(
    ivy"com.lihaoyi::scalatags:0.12.0",
    ivy"com.lihaoyi::mainargs:0.6.2",
    ivy"org.scala-lang::toolkit:0.6.0",
    ivy"com.lihaoyi::pprint:0.9.0"
  )
}
import Shared._

object leetcode extends ScalaModule {
  def scalaVersion = _scalaVersion
  def ivyDeps = _ivyDeps

  override def moduleDeps = Seq(macros)
}

object macros extends ScalaModule {
  def scalaVersion = _scalaVersion
  def ivyDeps = _ivyDeps
}
