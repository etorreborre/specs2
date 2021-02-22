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
      "org.portable-scala" %%% "portable-scala-reflect" % "1.0.0",
      "org.scala-js" %% "scalajs-stubs" % "1.0.0" % "provided")

  def jsTest =
    Seq(libraryDependencies ++= Seq(
      "org.scala-js" %% "scalajs-test-interface" % scalaJSVersion,
      "org.portable-scala" %%% "portable-scala-reflect" % "1.0.0"),
        scalaJSStage in Test := FastOptStage)

  def nativeTest =
    Seq(libraryDependencies += "org.scala-native" %%% "test-interface" % nativeVersion)

  def scalaParser = Def.setting {
    Seq("org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2")
  }
  def scalaParserNative = Def.setting {
    if(nativeVersion == "0.4.0-M2")
      Seq("com.github.lolgab" %%% "scala-parser-combinators" % "1.1.2")
    else
      scalaParser.value
  }

  def scalaXML = "org.scala-lang.modules" %% "scala-xml" % "1.3.0"

  lazy val mockito  = "org.mockito"  % "mockito-core"  % "3.8.0"
  lazy val junit    = "junit"        % "junit"         % "4.13.2"
  lazy val hamcrest = "org.hamcrest" % "hamcrest-core" % "2.2"

  lazy val pegdown = "org.pegdown" % "pegdown" % "1.6.0"

  lazy val tagsoup = "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1"

  lazy val scalacheck = Def.setting {
    "org.scalacheck" %%% "scalacheck" % "1.15.2"
  }

  def paradise(scalaVersion: String) =
    if (scalaMinorVersionAtLeast(scalaVersion, 11))
      Nil
    else
      Seq(compilerPlugin(
          "org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.patch),
          "org.scalamacros" %% "quasiquotes" % "2.1.0")

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
