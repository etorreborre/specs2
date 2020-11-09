import sbt._
import Defaults._
import Keys._
import dotty.tools.sbtplugin.DottyPlugin.autoImport._

object depends {

  lazy val scalaCheckVersion = "1.15.1"

  lazy val reflect =
    libraryDependencies += (scalaOrganization.value % "scala-reflect" % scalaVersion.value).withDottyCompat(scalaVersion.value)

  lazy val jvmTest =
    libraryDependencies ++= Seq(
      "org.scala-sbt" % "test-interface" % "1.0"
      )

  lazy val scalaParser =
    libraryDependencies += ("org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2").withDottyCompat(scalaVersion.value)

  lazy val scalaParserNative =
      scalaParser

  lazy val scalaXML =
    libraryDependencies += ("org.scala-lang.modules" %% "scala-xml" % "1.2.0").withDottyCompat(scalaVersion.value)

  lazy val scalacheck =
    "org.scalacheck" %% "scalacheck" % scalaCheckVersion

  // java dependencies
  lazy val junit = "junit" % "junit" % "4.13"
  lazy val pegdown = "org.pegdown" % "pegdown" % "1.6.0"
  lazy val tagsoup = "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1"

  lazy val resolvers =
    Seq(sbt.Keys.resolvers ++= Seq(
      Resolver.sonatypeRepo("releases")))

  def scalaMinorVersionAtLeast(scalaVersion: String, n: Int): Boolean =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, minor)) if minor >= n =>
        true
      case _ =>
        false
    }

}
