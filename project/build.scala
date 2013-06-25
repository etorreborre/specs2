import sbt._
import complete.DefaultParsers._
import Keys._
import com.typesafe.sbt._
import sbt.Configuration
import SbtSite._
import scala.Some
import SiteKeys._
import SbtGit._
import GitKeys._
import ls.Plugin._
import LsKeys._
import Defaults._

object build extends Build {
  type Settings = Project.Setting[_]

  lazy val specs2 = Project(
    id = "specs2",
    base = file("."),
    settings = Defaults.defaultSettings ++
               specs2Settings           ++
               dependenciesSettings     ++
               compilationSettings      ++
               testingSettings         
  ) 

  lazy val specs2Version = SettingKey[String]("specs2-version", "defines the current specs2 version")
  lazy val specs2Settings: Seq[Settings] = Seq(
    name := "specs2",
    organization := "org.specs2",
    specs2Version in GlobalScope <<= version,
    scalaVersion := "2.10.2")

  lazy val dependenciesSettings: Seq[Settings] = Seq(
    libraryDependencies <<= scalaVersion { scalaVersion => Seq(
      "org.scalaz"              %% "scalaz-core"       % "7.0.0",
      "org.scalaz"              %% "scalaz-concurrent" % "7.0.0",
      "com.chuusai"             %% "shapeless"         % "1.2.4"       % "optional",
      "org.scala-lang"          % "scala-reflect"      % scalaVersion  % "optional",
      "org.scala-lang"          % "scala-compiler"     % scalaVersion  % "optional",
      "org.scalacheck"          % "scalacheck_2.10.0"  % "1.10.0"      % "optional",
      "org.scalasbt.testing"    % "test-interface"     % "0.6"         % "optional",
      "org.hamcrest"            % "hamcrest-all"       % "1.1"         % "optional",
      "org.mockito"             % "mockito-all"        % "1.9.0"       % "optional",
      "junit"                   % "junit"              % "4.7"         % "optional",
      "org.pegdown"             % "pegdown"            % "1.2.1"       % "optional",
      "org.specs2"              % "classycle"          % "1.4.1"       % "optional")
    },
    resolvers ++= Seq("sonatype-releases" at "http://oss.sonatype.org/content/repositories/releases",
                      "sonatype-snapshots" at "http://oss.sonatype.org/content/repositories/snapshots")
  )

  lazy val compilationSettings: Seq[Settings] = Seq(
    javacOptions ++= Seq("-Xmx3G", "-Xms512m", "-Xss4m"),
    maxErrors := 20,
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:implicitConversions,reflectiveCalls,postfixOps,higherKinds,existentials,experimental.macros"),
    scalacOptions in Test ++= Seq("-Yrangepos")
  )

  lazy val testingSettings: Seq[Settings] = Seq(
    initialCommands in console := "import org.specs2._",
    logBuffered := false,
    cancelable := true,
    javaOptions += "-Xmx3G",
    testOptions := Seq(Tests.Filter(s =>
      Seq("Spec", "Suite", "Unit").exists(s.endsWith(_)) && !s.endsWith("FeaturesSpec") ||
        s.contains("UserGuide")         || 
        s.toLowerCase.contains("index") ||
        s.matches("org.specs2.guide.*")))
  )

  /**
   * EXAMPLE PROJECTS
   */
  lazy val examples = Project(id = "examples", base = file("examples"))
}


