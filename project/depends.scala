import sbt._
import Keys._
import Defaults._
// necessary for the %%% syntax
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
// necessary for accessing the scalaJSVersion property
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object depends {

  // used in specs2-core for the sbt runner
  val sbt = libraryDependencies += "org.scala-sbt" % "test-interface" % "1.0"

  // used in specs2-scalacheck
  val scalacheck = libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.17.0"
  val scalacheckTest = libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.17.0" % Test

  // used in specs2-matcher-extra
  val scalaParser = libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.1.1"

  // used in specs2-xml, and transitively by specs2-junit, specs2-matcher-extra, specs2-markdown
  val scalaXml = libraryDependencies += "org.scala-lang.modules" %%% "scala-xml" % "2.1.0"

  // used in specs2-junit
  val junitVintage = "org.junit.vintage" % "junit-vintage-engine" % "5.9.1"
  val junit = libraryDependencies ++= Seq(junitVintage, "org.junit.platform" % "junit-platform-engine" % "1.9.1")
  val junitTest = libraryDependencies += junitVintage % Test

  // used in specs2-markdown for the markdown parser
  val flexmark = "com.vladsch.flexmark" % "flexmark-all" % "0.64.0"

  // used in specs2-html
  val tagsoup = "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1"

  val Scala213 = "2.13.10"

  val isScala3 = Def.setting(CrossVersion.partialVersion(scalaVersion.value).exists(_._1 == 3))

  def sharedTest =
    Seq(
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % (if (isScala3.value) Scala213 else scalaVersion.value) % Provided
      )
    )

  def jvmTest =
    Seq(
      libraryDependencies ++= Seq(
        ("org.portable-scala" %%% "portable-scala-reflect" % "1.1.2").cross(CrossVersion.for3Use2_13),
        "org.scala-sbt" % "test-interface" % "1.0"
      )
    )

  def jsMacrotaskExecutor =
    Seq(libraryDependencies += "org.scala-js" %%% "scala-js-macrotask-executor" % "1.1.0")

  def jsTest =
    Seq(
      libraryDependencies ++=
        Seq(
          ("org.portable-scala" %%% "portable-scala-reflect" % "1.1.2").cross(CrossVersion.for3Use2_13),
          ("org.scala-js" %% "scalajs-test-interface" % scalaJSVersion).cross(CrossVersion.for3Use2_13)
        )
    )
}
