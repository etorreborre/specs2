import sbt._
import Keys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import scala.scalanative.sbtplugin.ScalaNativePlugin.autoImport._

object depends {

  lazy val classycle = Seq("org.specs2" % "classycle" % "1.4.3")

  def compiler(scalaOrganization: String, scalaVersion: String) = Seq(scalaOrganization % "scala3-compiler_3" % scalaVersion)

  def reflect(scalaOrganization: String, scalaVersion: String) = scalaOrganization % "scala-reflect" % scalaVersion

  def scalaz(scalazVersion: String) =
    Seq("org.scalaz" %% "scalaz-core",
        "org.scalaz" %% "scalaz-effect").map(_ % scalazVersion)

  def scalazConcurrent(scalazVersion: String) =
    "org.scalaz" %% "scalaz-concurrent" % scalazVersion

  def jvmTest: Seq[Def.Setting[_]] =
    Seq(libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
         case Some((3, _)) =>
           Seq(
             ("org.portable-scala" %%% "portable-scala-reflect" % "1.1.3").cross(CrossVersion.for3Use2_13),
             "org.scala-sbt" % "test-interface" % "1.0")
         case _ =>
           Seq(("org.portable-scala" %%% "portable-scala-reflect" % "1.1.3").cross(CrossVersion.for3Use2_13),
                "org.scala-sbt" % "test-interface" % "1.0",
                "org.scala-js" %% "scalajs-stubs" % "1.0.0" % "provided")
      }
    })

  def jsTest =
    Seq(
      jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
      libraryDependencies ++=
        Seq(
          ("org.portable-scala" %%% "portable-scala-reflect" % "1.1.3").cross(CrossVersion.for3Use2_13),
          ("org.scala-js" %% "scalajs-test-interface" % scalaJSVersion).cross(CrossVersion.for3Use2_13)
        )
    )

  def jsMacrotaskExecutor =
    Seq(libraryDependencies += "org.scala-js" %%% "scala-js-macrotask-executor" % "1.0.0")

  def nativeTest =
    Seq(libraryDependencies ++= Seq(
    "org.scala-native" %%% "test-interface" % nativeVersion,
    ("org.portable-scala" %%% "portable-scala-reflect" % "1.1.3").cross(CrossVersion.for3Use2_13)
    ))

  // used in specs2-matcher-extra
  def scalaParser = libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.4.0"

  def scalaXml = libraryDependencies += "org.scala-lang.modules" %%% "scala-xml" % "2.3.0"

  lazy val mockito  = "org.mockito"  % "mockito-core"  % "4.11.0"
  lazy val junit    = "junit"        % "junit"         % "4.13.2"
  lazy val hamcrest = "org.hamcrest" % "hamcrest" % "2.2"

  lazy val pegdown = "org.pegdown" % "pegdown" % "1.6.0"

  lazy val tagsoup = "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1"

  lazy val scalacheck = Def.setting {
    "org.scalacheck" %%% "scalacheck" % "1.18.0"
  }

  lazy val resolvers =
    Seq(sbt.Keys.resolvers ++=
      Resolver.sonatypeOssRepos("release"))

  def scalaMinorVersionAtLeast(scalaVersion: String, n: Int): Boolean =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, minor)) if minor >= n =>
        true
      case _ =>
        false
    }

}
