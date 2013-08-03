import sbt._
import Keys._

object dependencies {

  lazy val settings = dependencies ++ resolversSettings
  lazy val dependencies = libraryDependencies ++= 
    scalalibs(scalaVersion.value) ++
    scalaz()                ++
    specs2
  
  // Libraries
  def scalaz(scalazVersion: String = "7.0.2") = Seq(
    "org.scalaz"                        %% "scalaz-core"               % scalazVersion,
    "org.scalaz"                        %% "scalaz-concurrent"         % scalazVersion)

  def scalalibs(scalaVersion: String) = Seq(
      "org.scala-lang"          % "scala-reflect"      % scalaVersion  % "optional",
      "org.scala-lang"          % "scala-compiler"     % scalaVersion  % "optional",
      "org.scala-sbt"           % "test-interface"     % "1.0"         % "optional")

  def specs2 = Seq(
      "org.scalacheck"          %% "scalacheck"        % "1.10.0"      % "optional",
      "com.chuusai"             %% "shapeless"         % "1.2.4"       % "optional",
      "org.hamcrest"            % "hamcrest-core"      % "1.2"         % "optional",
      "org.mockito"             % "mockito-all"        % "1.9.0"       % "optional",
      "junit"                   % "junit"              % "4.11"        % "optional",
      "org.pegdown"             % "pegdown"            % "1.2.1"       % "optional",
      "org.specs2"              % "classycle"          % "1.4.1"       % "optional")


  lazy val resolversSettings = resolvers ++= Seq(
    "sonatype-releases"  at "http://oss.sonatype.org/content/repositories/releases",
    "sonatype-snapshots" at "http://oss.sonatype.org/content/repositories/snapshots")
}
