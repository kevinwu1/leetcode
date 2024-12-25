package build
import mill._

import scalalib._

object Shared {
  val _scalaVersion = "2.13.15"
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
  def scalacOptions = Seq("-Ymacro-annotations")
}

object macros extends ScalaModule {
  def scalaVersion = _scalaVersion
  def ivyDeps = _ivyDeps ++
    Agg(
      ivy"org.scala-lang:scala-reflect:2.13.15"
    )
  def scalacOptions = Seq("-Ymacro-annotations")
}
