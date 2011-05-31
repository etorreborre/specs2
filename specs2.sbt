/** Project */
name := "specs2"

version := "1.4-SNAPSHOT"

organization := "org.specs2"

scalaVersion := "2.9.0-1"

/** Shell */
shellPrompt := { state => System.getProperty("user.name") + "> " }

shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }

/** Dependencies */
resolvers ++= Seq("snapshots-repo" at "http://scala-tools.org/repo-snapshots", 
                  "Local Maven Repository" at "file://$M2_REPO")

libraryDependencies ++= Seq(
  "org.scala-tools.testing" %% "scalacheck" % "1.9", 
  "org.scala-tools.testing" % "test-interface" % "0.5", 
  "org.specs2" %% "specs2-scalaz-core" % "6.0.RC2",
  "org.hamcrest" % "hamcrest-all" % "1.1",
  "org.mockito" % "mockito-all" % "1.8.5",
  "junit" % "junit" % "4.7",
  "org.pegdown" % "pegdown" % "1.0.1"
)

/** Compilation */
javacOptions ++= Seq("-Xmx1812m", "-Xms512m", "-Xss4m")

scalacOptions += "-deprecation"

maxErrors := 20

pollInterval := 1000

testFrameworks += new TestFramework("org.specs2.runner.SpecsFramework")

testOptions := Seq(Tests.Filter(s =>
  Seq("Spec", "Suite", "Unit", "all").exists(s.endsWith(_)) &&
    ! s.endsWith("FeaturesSpec") ||
    s.contains("UserGuide") || 
    s.matches("org.specs2.guide.*")))

/** Console */
initialCommands in console := "import org.specs2._"

// Packaging

/** Publishing */
credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

publishTo <<= (version) { version: String =>
  val nexus = "http://nexus-direct.scala-tools.org/content/repositories/"
  if (version.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus+"snapshots/") 
  else                                   Some("releases" at nexus+"releases/")
}