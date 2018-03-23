import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object depends {

  lazy val classycle = Seq("org.specs2" % "classycle" % "1.4.3")

  def compiler(scalaOrganization: String, scalaVersion: String) = Seq(scalaOrganization % "scala-compiler" % scalaVersion)

  def reflect(scalaOrganization: String, scalaVersion: String) = Seq(scalaOrganization % "scala-reflect" % scalaVersion)

  def scalaz(scalazVersion: String) =
    Seq("org.scalaz" %% "scalaz-core",
        "org.scalaz" %% "scalaz-effect").map(_ % scalazVersion)

  def scalazConcurrent(scalazVersion: String) =
    Seq("org.scalaz" %% "scalaz-concurrent").map(_ % scalazVersion)

  def jvmTest =
    libraryDependencies ++= Seq(
      "org.scala-sbt" % "test-interface" % "1.0",
      "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided")

  def jsTest =
    Seq(libraryDependencies ++= Seq("org.scala-js" %% "scalajs-test-interface" % scalaJSVersion),
        scalaJSStage in Test := FastOptStage)

  def scalaParser = Def.setting {
    Seq("org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.0") 
  }

  def scalaXML = Def.setting {
    Seq("org.scala-lang.modules" %% "scala-xml" % "1.0.6")
  }

  def kindp(scalaVersion: String) =
    "org.spire-math" % "kind-projector" % "0.8.2" cross CrossVersion.binary

  lazy val mockito       = Seq("org.mockito"  % "mockito-core"  % "2.16.0")
  lazy val junit         = Seq("junit"        % "junit"         % "4.12")
  lazy val hamcrest      = Seq("org.hamcrest" % "hamcrest-core" % "1.3")

  lazy val pegdown = Seq("org.pegdown" % "pegdown" % "1.6.0")

  lazy val tagsoup = "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2"

  def sbtJvm(scalaJsVersion: String) = Seq(
    "org.scala-sbt" % "test-interface" % "1.0",
    "org.scala-js" %% "scalajs-stubs" % scalaJsVersion % "provided"
  )

  def sbtJs(scalaJsVersion: String) = Seq(
    "org.scala-js" %% "scalajs-test-interface" % scalaJsVersion
  )

  def paradise(scalaVersion: String) =
    if (scalaMinorVersionAtLeast(scalaVersion, 11))
      Nil
    else
      Seq(compilerPlugin(
          "org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.patch),
          "org.scalamacros" %% "quasiquotes" % "2.1.0")

  def scalaParallelCollections(scalaVersion: String) =
    if (scalaMinorVersionAtLeast(scalaVersion, 13))
      Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "0.1.2")
    else
      Seq()

  lazy val resolvers =
    Seq(sbt.Keys.resolvers ++= Seq(Resolver.sonatypeRepo("releases")))

  def scalaMinorVersionAtLeast(scalaVersion: String, n: Int): Boolean =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, minor)) if minor >= n =>
        true
      case _ =>
        false
    }

}
