import sbt._
import Defaults._
import Keys._

object depends {

  // used in specs2-core for the sbt runner
  val sbt = "org.scala-sbt" % "test-interface" % "1.0"

  // used in specs2-scalacheck
  val scalacheck = "org.scalacheck" %% "scalacheck" % "1.15.3"

  // used in specs2-matcher-extra
  val scalaParser = "org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0"

  // used in specs2-xml, and transitively by specs2-junit, specs2-matcher-extra, specs2-markdown
  val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "2.0.0"

  // used in specs2-junit
  val junit = "org.junit.vintage" % "junit-vintage-engine" % "5.3.1"

  // used in specs2-markdown for the markdown parser
  val flexmark = "com.vladsch.flexmark" % "flexmark-all" % "0.62.2"

  // used in specs2-html
  val tagsoup = "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1"

  def scalaMinorVersionAtLeast(scalaVersion: String, n: Int): Boolean =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, minor)) if minor >= n =>
        true
      case _ =>
        false
    }
}
