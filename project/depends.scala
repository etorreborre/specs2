import sbt._
import Defaults._
import Keys._

object depends {

  lazy val scalaCheckVersion = "1.15.3"

  lazy val reflect =
    libraryDependencies += (scalaOrganization.value % "scala-reflect" % scalaVersion.value)

  lazy val jvmTest =
    libraryDependencies ++= Seq(
      "org.scala-sbt" % "test-interface" % "1.0"
      )

  lazy val scalaParser =
    libraryDependencies += ("org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0")

  lazy val scalaParserNative =
      scalaParser

  lazy val scalaXml =
    libraryDependencies += ("org.scala-lang.modules" %% "scala-xml" % "2.0.0")

  lazy val scalacheck =
    "org.scalacheck" %% "scalacheck" % scalaCheckVersion

  // java dependencies
  lazy val junit = "junit" % "junit" % "4.13.2"
  lazy val flexmark = "com.vladsch.flexmark" % "flexmark-all" % "0.62.2"

  lazy val tagsoup = "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1"

  def scalaMinorVersionAtLeast(scalaVersion: String, n: Int): Boolean =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, minor)) if minor >= n =>
        true
      case _ =>
        false
    }

}
