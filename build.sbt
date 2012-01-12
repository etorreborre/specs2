import sbtrelease._
import Release._
import ReleaseKeys._
 
/** Project */
name := "specs2"

version := "1.8-SNAPSHOT"

organization := "org.specs2"

scalaVersion := "2.9.1"

crossScalaVersions := Seq("2.9.0", "2.9.0-1")

/** Shell */
shellPrompt := { state => System.getProperty("user.name") + "> " }

shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }

/** Dependencies */
resolvers ++= Seq("snapshots-repo" at "http://scala-tools.org/repo-snapshots", 
                  "Local Maven Repository" at "file://c:/Documents and Settings/Eric/.m2/repository")

libraryDependencies <<= scalaVersion { scala_version => Seq(
  "org.specs2" %% "specs2-scalaz-core" % "6.0.1",
  "org.scala-lang" % "scala-compiler" % scala_version % "optional", 
  "org.scala-tools.testing" %% "scalacheck" % "1.9" % "optional", 
  "org.scala-tools.testing" % "test-interface" % "0.5" % "optional", 
  "org.hamcrest" % "hamcrest-all" % "1.1" % "optional",
  "org.mockito" % "mockito-all" % "1.8.5" % "optional",
  "junit" % "junit" % "4.7" % "optional",
  "org.pegdown" % "pegdown" % "1.0.2" % "optional",
  "org.specs2" %% "classycle" % "1.4" % "optional"
  )
}

/** Compilation */
javacOptions ++= Seq("-Xmx1812m", "-Xms512m", "-Xss6m")

javaOptions += "-Xmx2G"

scalacOptions ++= Seq("-deprecation", "-unchecked")

maxErrors := 20 

pollInterval := 1000

logBuffered := false

cancelable := true

testOptions := Seq(Tests.Filter(s =>
  Seq("Spec", "Suite", "Unit", "all").exists(s.endsWith(_)) &&
    !s.endsWith("FeaturesSpec") ||
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

 
seq(releaseSettings: _*)

releaseProcess <<= thisProjectRef apply { ref =>
  import ReleaseStateTransformations._
  Seq[ReleasePart](
    initialGitChecks,                     
    checkSnapshotDependencies,    
    releaseTask(check in Posterous in ref),  
    inquireVersions,                        
    setReleaseVersion,                      
    runTest,                                
    commitReleaseVersion,                   
    tagRelease,                             
    releaseTask(publish in Global in ref),
    releaseTask(publish in Posterous in ref),    
    setNextVersion,                         
    commitNextVersion                       
  )
}

seq(lsSettings :_*)

(LsKeys.ghBranch in LsKeys.lsync) := Some("1.8")

