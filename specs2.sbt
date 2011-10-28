
/** Project */
name := "specs2"

version := "1.7-SNAPSHOT"

organization := "org.specs2"

scalaVersion := "2.9.1"

crossScalaVersions := Seq("2.9.0", "2.9.0-1") ++ (1 to 4).map(i => "2.9.1.RC"+i)

/** Shell */
shellPrompt := { state => System.getProperty("user.name") + "> " }

shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }

/** Dependencies */
resolvers ++= Seq("snapshots-repo" at "http://scala-tools.org/repo-snapshots", 
                  "Local Maven Repository" at "file://c:/Documents and Settings/Eric/.m2/repository")

libraryDependencies ++= Seq(
  "org.scala-tools.testing" %% "scalacheck" % "1.9" % "optional", 
  "org.scala-tools.testing" % "test-interface" % "0.5" % "optional", 
  "org.specs2" %% "specs2-scalaz-core" % "6.0.1",
  "org.hamcrest" % "hamcrest-all" % "1.1" % "optional",
  "org.mockito" % "mockito-all" % "1.8.5" % "optional",
  "junit" % "junit" % "4.7" % "optional",
  "org.pegdown" % "pegdown" % "1.0.2" % "optional"
)

/** Compilation */
javacOptions ++= Seq("-Xmx1812m", "-Xms512m", "-Xss4m")

javaOptions += "-Xmx2G"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

maxErrors := 20 

pollInterval := 1000

logBuffered := false

testOptions := Seq(Tests.Filter(s =>
  Seq("Spec", "Suite", "Unit", "all").exists(s.endsWith(_)) &&
    ! s.endsWith("FeaturesSpec") ||
    s.contains("UserGuide") || 
	s.contains("index") ||
    s.matches("org.specs2.guide.*")))

/** Console */
initialCommands in console := "import org.specs2._"

/** Publishing */
credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

publishTo <<= (version) { version: String =>
  val nexus = "http://nexus-direct.scala-tools.org/content/repositories/"
  if (version.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus+"snapshots/") 
  else                                   Some("releases" at nexus+"releases/")
}
