import com.jsuereth.sbtsite.SiteKeys._
import com.jsuereth.git.{GitKeys,GitRunner}
import GitKeys.{gitBranch, gitRemoteRepo}
import com.jsuereth.ghpages.GhPages.ghpages._

/** Project */
name := "specs2"

version := "1.12.3"

organization := "org.specs2"

scalaVersion := "2.9.2"

crossScalaVersions := Seq("2.9.1", "2.9.1-1", "2.9.2")

/** Shell */
shellPrompt := { state => System.getProperty("user.name") + "> " }

shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }

/** Dependencies */
resolvers ++= Seq("releases" at "http://oss.sonatype.org/content/repositories/releases",
                  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots")

libraryDependencies <<= scalaVersion { scala_version => Seq(
  "org.specs2" %% "specs2-scalaz-core" % "6.0.1",
  "org.scala-lang" % "scala-compiler" % scala_version % "optional",
  if (scala_version contains "-1") "org.scalacheck" % "scalacheck_2.9.1" % "1.9" % "optional"
  else                             "org.scalacheck" %% "scalacheck" % "1.9" % "optional",
  "org.scala-tools.testing" % "test-interface" % "0.5" % "optional",
  "org.hamcrest" % "hamcrest-all" % "1.1" % "optional",
  "org.mockito" % "mockito-all" % "1.9.0" % "optional",
  "junit" % "junit" % "4.7" % "optional",
  "org.pegdown" % "pegdown" % "1.0.2" % "optional",
  "org.specs2" % "classycle" % "1.4.1" % "optional",
  "org.scalaz" %% "scalaz-core" % "6.0.4" % "optional"
  )
}

/** Compilation */
javacOptions ++= Seq("-Xmx1812m", "-Xms512m", "-Xss10m")

javaOptions += "-Xmx2G"

scalacOptions ++= Seq("-deprecation", "-unchecked")

maxErrors := 20 

pollInterval := 1000

logBuffered := false

cancelable := true

testOptions := Seq(Tests.Filter(s =>
  Seq("Spec", "Suite", "Unit").exists(s.endsWith(_)) && !s.endsWith("FeaturesSpec") ||
    s.contains("UserGuide") || 
  	s.toLowerCase.contains("index") ||
    s.matches("org.specs2.guide.*")))

/** Console */
initialCommands in console := "import org.specs2._"

/** Site building */
site.settings

seq(site.settings:_*)

siteSourceDirectory <<= target (_ / "specs2-reports")

// depending on the version, copy the api files to a different directory
siteMappings <++= (mappings in packageDoc in Compile, version) map { (m, v) =>
  for((f, d) <- m) yield (f, "api/"+v+"/"+d)
}

/** Site publication */
seq(ghpages.settings:_*)

// override the synchLocal task to avoid removing the existing files
synchLocal <<= (privateMappings, updatedRepository, GitKeys.gitRunner, streams) map { (mappings, repo, git, s) =>
  val betterMappings = mappings map { case (file, target) => (file, repo / target) }
  IO.copy(betterMappings)
  repo
}

git.remoteRepo := "git@github.com:etorreborre/specs2.git"

/** Publishing */
credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) Some("sonatype-snapshots" at nexus + "content/repositories/snapshots")
  else                             Some("sonatype-staging" at nexus   + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { x => false }

pomExtra := (
  <url>http://specs2.org/</url>
  <licenses>
    <license>
      <name>MIT-style</name>
      <url>http://www.opensource.org/licenses/mit-license.php</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>http://github.com/etorreborre/specs2</url>
    <connection>scm:http:http://etorreborre@github.com/etorreborre/specs2.git</connection>
  </scm>
  <developers>
    <developer>
      <id>etorreborre</id>
      <name>Eric Torreborre</name>
      <url>http://etorreborre.blogspot.com/</url>
      </developer>
    </developers>
)

seq(lsSettings :_*)

(LsKeys.ghBranch in LsKeys.lsync) <<= version { Some(_) }

(LsKeys.ghUser in LsKeys.lsync) := Some("etorreborre")

(LsKeys.ghRepo in LsKeys.lsync) := Some("specs2")
