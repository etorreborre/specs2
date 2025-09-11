import sbt._
import Keys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import scala.scalanative.sbtplugin.ScalaNativePlugin.autoImport._

object depends {

  lazy val classycle = Seq("org.specs2" % "classycle" % "1.4.3")

  def compiler(scalaOrganization: String, scalaVersion: String) = Seq(scalaOrganization % "scala-compiler" % scalaVersion)

  def reflect(scalaOrganization: String, scalaVersion: String) = scalaOrganization % "scala-reflect" % scalaVersion

  def scalaz(scalazVersion: String) =
    Seq("org.scalaz" %% "scalaz-core",
        "org.scalaz" %% "scalaz-effect").map(_ % scalazVersion)

  def scalazConcurrent(scalazVersion: String) =
    "org.scalaz" %% "scalaz-concurrent" % scalazVersion

  def jvmTest =
    libraryDependencies ++= Seq(
      "org.scala-sbt" % "test-interface" % "1.0",
      "org.portable-scala" %%% "portable-scala-reflect" % "1.1.3",
      "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided")

  def jsTest =
    Seq(libraryDependencies ++= Seq(
      "org.scala-js" %% "scalajs-test-interface" % scalaJSVersion,
      "org.portable-scala" %%% "portable-scala-reflect" % "1.1.3"),
        Test / scalaJSStage := FastOptStage) ++ jsMacrotaskExecutor

  def jsMacrotaskExecutor =
    Seq(libraryDependencies += "org.scala-js" %%% "scala-js-macrotask-executor" % "1.1.1")

  def nativeTest =
    Seq(libraryDependencies ++= Seq(
    "org.scala-native" %%% "test-interface" % nativeVersion,
    "org.portable-scala" %%% "portable-scala-reflect" % "1.1.3"
    ))

  def scalaParser = Def.setting {
    Seq("org.scala-lang.modules" %%% "scala-parser-combinators" % {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 12)) => "1.1.2"
        case _             => "2.4.0"
      }
    })
  }
  def scalaParserNative = Def.setting {
    Seq("org.scala-lang.modules" %%% "scala-parser-combinators" % "2.4.0")
  }

  def scalaXml = "org.scala-lang.modules" %% "scala-xml" % "2.4.0"

  lazy val mockito  = "org.mockito"  % "mockito-core"  % "5.19.0"
  lazy val junit    = "junit"        % "junit"         % "4.13.2"
  lazy val hamcrest = "org.hamcrest" % "hamcrest" % "3.0"

  lazy val pegdown = "org.pegdown" % "pegdown" % "1.6.0"

  lazy val tagsoup = "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1"

  lazy val scalacheck = Def.setting {
    "org.scalacheck" %%% "scalacheck" % "1.19.0"
  }

  lazy val resolvers =
    Seq(sbt.Keys.resolvers ++= Resolver.sonatypeOssRepos("releases"))

}
