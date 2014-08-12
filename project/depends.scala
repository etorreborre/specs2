import sbt._
import Keys._

object depends {

  lazy val scalazVersion = settingKey[String]("defines the current scalaz version")

  lazy val classycle = Seq("org.specs2" % "classycle" % "1.4.3")

  def compiler(scalaVersion: String) = Seq("org.scala-lang" % "scala-compiler" % scalaVersion)

  def reflect(scalaVersion: String)  = Seq("org.scala-lang" % "scala-reflect" % scalaVersion)

  lazy val scalaz = Seq("org.scalaz"        %% "scalaz-core"      ,
                        "org.scalaz"        %% "scalaz-concurrent").map(_ % "7.1.0") ++
                    Seq("org.scalaz.stream" %% "scalaz-stream"    % "0.5a")


  lazy val scalacheck    = Seq("org.scalacheck" %% "scalacheck"   % "1.11.3")
  lazy val mockito       = Seq("org.mockito"    % "mockito-core"  % "1.9.5")
  lazy val junit         = Seq("junit"          % "junit"         % "4.11")
  lazy val hamcrest      = Seq("org.hamcrest"   % "hamcrest-core" % "1.3")

  def shapeless(scalaVersion: String) =
    if (scalaVersion.contains("2.11")) Seq("com.chuusai" % "shapeless_2.11" % "2.0.0")
    else                               Seq("com.chuusai" % ("shapeless_"+scalaVersion) % "2.0.0")

  lazy val pegdown = Seq("org.pegdown" % "pegdown" % "1.2.1")

  lazy val testInterface = Seq("org.scala-sbt"  % "test-interface" % "1.0")


  def paradise(scalaVersion: String) =
    if (scalaVersion.startsWith("2.11")) Nil
    else  Seq(compilerPlugin("org.scalamacros" %% "paradise"    % "2.0.0" cross CrossVersion.full),
                             "org.scalamacros" %% "quasiquotes" % "2.0.0")
}


