import sbt._
import Defaults._
import Keys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import scala.scalanative.sbtplugin.ScalaNativePlugin.autoImport._
import dotty.tools.sbtplugin.DottyPlugin.autoImport._

object depends {

  lazy val scalaCheckVersion = "1.14.3"

  lazy val reflect =
    libraryDependencies += (scalaOrganization.value % "scala-reflect" % scalaVersion.value).withDottyCompat(scalaVersion.value)

  lazy val jvmTest =
    libraryDependencies ++= Seq(
      "org.scala-sbt" % "test-interface" % "1.0",
      "org.portable-scala" %%% "portable-scala-reflect" % "1.0.0",
      "org.scala-js" %% "scalajs-stubs" % "1.0.0" % "provided").map(_.withDottyCompat(scalaVersion.value))

  lazy val jsTest =
    Seq(libraryDependencies ++= Seq(
          "org.scala-js" %% "scalajs-test-interface" % scalaJSVersion,
          "org.portable-scala" %%% "portable-scala-reflect" % "1.0.0").map(_.withDottyCompat(scalaVersion.value)),
        scalaJSStage in Test := FastOptStage)

  lazy val nativeTest =
    Seq(libraryDependencies ++= Seq(
          "org.scala-native" %%% "test-interface" % nativeVersion,
          "org.portable-scala" %%% "portable-scala-reflect" % "1.0.0").map(_.withDottyCompat(scalaVersion.value)))

  lazy val scalaParser =
    libraryDependencies += ("org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2").withDottyCompat(scalaVersion.value)

  lazy val scalaParserNative =
    if (nativeVersion == "0.4.0-M2")
      libraryDependencies += ("com.github.lolgab" %%% "scala-parser-combinators" % "1.1.2").withDottyCompat(scalaVersion.value)
    else
      scalaParser

  lazy val scalaXML =
    libraryDependencies += ("org.scala-lang.modules" %% "scala-xml" % "1.2.0").withDottyCompat(scalaVersion.value)

  lazy val scalacheck =
    libraryDependencies += ("org.scalacheck" %%% "scalacheck" % scalaCheckVersion).withDottyCompat(scalaVersion.value)

  lazy val scalacheckTest =
    libraryDependencies += ("org.scalacheck" %%% "scalacheck" % scalaCheckVersion).withDottyCompat(scalaVersion.value)

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
