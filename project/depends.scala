import sbt._
import Keys._

object depends {

  lazy val classycle = Seq("org.specs2" % "classycle" % "1.4.3")

  def compiler(scalaOrganization: String, scalaVersion: String) = Seq(scalaOrganization % "scala-compiler" % scalaVersion)

  def reflect(scalaOrganization: String, scalaVersion: String) = Seq(scalaOrganization % "scala-reflect" % scalaVersion)

  def scalaz(scalazVersion: String) =
    Seq("org.scalaz" %% "scalaz-core",
        "org.scalaz" %% "scalaz-effect").map(_ % scalazVersion)

  def scalazConcurrent(scalazVersion: String) =
    Seq("org.scalaz" %% "scalaz-concurrent").map(_ % scalazVersion)

  def scalaParser(scalaVersion: String) =
    if (scalaMinorVersionAtLeast(scalaVersion, 11))
      Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4")
    else
      Seq()

  def scalaXML(scalaVersion: String) =
    if (scalaMinorVersionAtLeast(scalaVersion, 11))
      Seq("org.scala-lang.modules" %% "scala-xml" % "1.0.5")
    else
      Seq()

  def kindp(scalaVersion: String) =
    "org.spire-math" % "kind-projector" % "0.8.2" cross CrossVersion.binary

  def scalacheck(scalaVersion: String) =
    Seq("org.scalacheck" %% "scalacheck" % "1.13.4")

  def si2712Dependency(scalaVersion: String) =
    if (CrossVersion.partialVersion(scalaVersion).exists(_._2 < 11))
      Seq(compilerPlugin("com.milessabin" % ("si2712fix-plugin_"+scalaVersion) % "1.2.0"))
    else
      Seq()

  lazy val mockito       = Seq("org.mockito"  %  "mockito-core"  % "1.9.5")
  lazy val junit         = Seq("junit"        %  "junit"         % "4.12")
  lazy val hamcrest      = Seq("org.hamcrest" %  "hamcrest-core" % "1.3")

  def shapeless(scalaVersion: String) =
    Seq("com.chuusai" %% "shapeless" % "2.3.2")

  lazy val cats = Seq("org.typelevel" %% "cats-core" % "0.7.2")

  lazy val pegdown = Seq("org.pegdown" % "pegdown" % "1.6.0")

  lazy val testInterface = Seq("org.scala-sbt"  % "test-interface" % "1.0")

  lazy val tagsoup = "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2"

  def paradise(scalaVersion: String) =
    if (scalaMinorVersionAtLeast(scalaVersion, 11))
      Nil
    else
      Seq(compilerPlugin(
          "org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.patch),
          "org.scalamacros" %% "quasiquotes" % "2.1.0")

  def scalaParallelCollections(scalaVersion: String) =
    if (scalaMinorVersionAtLeast(scalaVersion, 13))
      Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "0.1.1")
    else
      Seq()

  lazy val resolvers =
    Seq(updateOptions := updateOptions.value.withCachedResolution(true)) ++ {
      sbt.Keys.resolvers ++=
      Seq(
        Resolver.sonatypeRepo("releases"),
        Resolver.sonatypeRepo("snapshots"),
        Resolver.typesafeIvyRepo("releases"),
        "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases")
    }

  def scalaMinorVersionAtLeast(scalaVersion: String, n: Int): Boolean =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, minor)) if minor >= n =>
        true
      case _ =>
        false
    }

}
