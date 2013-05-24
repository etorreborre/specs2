import sbt._
import complete.DefaultParsers._
import Keys._
import com.typesafe.sbt._
import pgp.PgpKeys._
import sbt.Configuration
import SbtSite._
import scala.Some
import SiteKeys._
import SbtGit._
import GitKeys._
import SbtGhPages._
import GhPagesKeys._
import sbtrelease._
import ReleasePlugin._
import ReleaseKeys._
import ReleaseStateTransformations._
import ls.Plugin._
import LsKeys._
import Utilities._
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
               testingSettings          ++
               siteSettings             ++
               publicationSettings      ++
               notificationSettings     ++
               releaseSettings
  ) 

  lazy val specs2Version = SettingKey[String]("specs2-version", "defines the current specs2 version")
  lazy val specs2Settings: Seq[Settings] = Seq(
    name := "specs2",
    organization := "org.specs2",
    specs2Version in GlobalScope <<= version,
    scalaVersion := "2.10.2-RC1")

  lazy val dependenciesSettings: Seq[Settings] = Seq(
    libraryDependencies <<= scalaVersion { scalaVersion => Seq(
      "org.scalaz"              %% "scalaz-core"       % "7.0.0",
      "org.scalaz"              %% "scalaz-concurrent" % "7.0.0",
      "com.chuusai"             %% "shapeless"         % "1.2.3"       % "optional",
      "org.scala-lang"          % "scala-reflect"      % scalaVersion  % "optional",
      "org.scala-lang"          % "scala-compiler"     % scalaVersion  % "optional",
      "org.scalacheck"          % "scalacheck_2.10.0"  % "1.10.0"      % "optional",
      "org.scala-tools.testing" % "test-interface"     % "0.5"         % "optional",
      "org.hamcrest"            % "hamcrest-all"       % "1.1"         % "optional",
      "org.mockito"             % "mockito-all"        % "1.9.0"       % "optional",
      "junit"                   % "junit"              % "4.7"         % "optional",
      "org.pegdown"             % "pegdown"            % "1.0.2"       % "optional",
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

  lazy val siteSettings: Seq[Settings] = ghpages.settings ++ SbtSite.site.settings ++ Seq(
    siteSourceDirectory <<= target (_ / "specs2-reports"),
    // depending on the version, copy the api files to a different directory
    siteMappings <++= (mappings in packageDoc in Compile, version) map { (m, v) =>
      for((f, d) <- m) yield (f, if (v.trim.endsWith("SNAPSHOT")) ("api/master/" + d) else ("api/SPECS2-"+v+"/"+d))
    },
    // override the synchLocal task to avoid removing the existing files
    synchLocal <<= (privateMappings, updatedRepository, gitRunner, streams) map { (mappings, repo, git, s) =>
      val betterMappings = mappings map { case (file, target) => (file, repo / target) }
      IO.copy(betterMappings)
      repo
    },
    gitRemoteRepo := "git@github.com:etorreborre/specs2.git"
  )

  lazy val notificationSettings: Seq[Settings] = lsSettings ++ Seq(
    (LsKeys.ghBranch in LsKeys.lsync) := Some("master"),
    (LsKeys.ghUser in LsKeys.lsync) := Some("etorreborre"),
    (LsKeys.ghRepo in LsKeys.lsync) := Some("specs2")
  )

  lazy val publicationSettings: Seq[Settings] = Seq(
    publishTo <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
      else                             Some("staging" at nexus + "service/local/staging/deploy/maven2")
    },
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { x => false },
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
    ),
    credentials := Seq(Credentials(Path.userHome / ".sbt" / "specs2.credentials"))
  )

  /**
   * EXAMPLE PROJECTS
   */
  lazy val examples = Project(id = "examples", base = file("examples"))
  
  /**
   * RELEASE PROCESS
   */
  lazy val releaseSettings =
    ReleasePlugin.releaseSettings ++ Seq(
    tagName <<= (version in ThisBuild) map (v => "SPECS2-" + v),
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      setReleaseVersion,
      commitReleaseVersion,
      generateUserGuide,
      generateIndexPage,
      publishSite,
      publishSignedArtifacts,
      notifyLs,
      notifyHerald,
      tagRelease,
      setNextVersion,
      commitNextVersion,
      pushChanges
    ),
    releaseSnapshotProcess := Seq[ReleaseStep](
      generateUserGuide,
      publishSite,
      publishSignedArtifacts),
    commands += releaseSnapshotCommand
    ) ++
  Seq(publishUserGuideTask <<= pushSite.dependsOn(makeSite).dependsOn(generateUserGuideTask)) ++
  documentationSettings

  lazy val releaseSnapshotProcess = SettingKey[Seq[ReleaseStep]]("release-snapshot-process")
  private lazy val releaseSnapshotCommandKey = "release-snapshot"
  private val WithDefaults = "with-defaults"
  private val SkipTests = "skip-tests"
  private val releaseSnapshotParser = (Space ~> WithDefaults | Space ~> SkipTests).*

  val releaseSnapshotCommand: Command = Command(releaseSnapshotCommandKey)(_ => releaseSnapshotParser) { (st, args) =>
    val extracted = Project.extract(st)
    val releaseParts = extracted.get(releaseSnapshotProcess)

    val startState = st
      .put(useDefaults, args.contains(WithDefaults))
      .put(skipTests, args.contains(SkipTests))

    val initialChecks = releaseParts.map(_.check)
    val process = releaseParts.map(_.action)

    initialChecks.foreach(_(startState))
    Function.chain(process)(startState)
  }

  /**
   * DOCUMENTATION
   */
  lazy val documentationSettings =
    testTaskDefinition(generateUserGuideTask, Seq(Tests.Filter(_.endsWith("UserGuide")), Tests.Argument("html"))) ++
    testTaskDefinition(generateIndexPageTask, Seq(Tests.Filter(_.endsWith("Index")), Tests.Argument("html")))

  lazy val generateUserGuideTask = TaskKey[Tests.Output]("generate-user-guide", "generate the user guide")
  lazy val generateUserGuide     = ReleaseStep { st: State =>
    val st2 = executeStepTask(generateUserGuideTask, "Generating the User Guide", Test)(st)
    commitCurrent("updated the UserGuide")(st2)
  }

  lazy val generateIndexPageTask = TaskKey[Tests.Output]("generate-index-page", "generate the index page")
  lazy val generateIndexPage     = executeStepTask(generateIndexPageTask, "Generating the Index page", Test)

  lazy val publishUserGuideTask = TaskKey[Unit]("publish-user-guide", "publish the user guide")

  lazy val publishSite = ReleaseStep { st: State =>
    val st2 = executeStepTask(makeSite, "Making the site")(st)
    executeStepTask(pushSite, "Publishing the site")(st2)
  }

  def testTaskDefinition(task: TaskKey[Tests.Output], options: Seq[TestOption]) =
    Seq(testTask(task))                          ++
    inScope(GlobalScope)(defaultTestTasks(task)) ++
    inConfig(Test)(testTaskOptions(task))        ++
    (testOptions in (Test, task) ++= options)

  def testTask(task: TaskKey[Tests.Output]) =
    task <<= (streams in Test, loadedTestFrameworks in Test, testLoader in Test,
      testGrouping in Test in test, testExecution in Test in task,
      fullClasspath in Test in test, javaHome in test) flatMap Defaults.allTestGroupsTask

  /**
   * PUBLICATION
   */
  lazy val publishSignedArtifacts = executeStepTask(publishSigned, "Publishing signed artifacts")

  /**
   * NOTIFICATION
   */
  lazy val notifyLs = ReleaseStep { st: State =>
    val st2 = executeTask(writeVersion, "Writing ls.implicit.ly dependencies")(st)
    val st3 = commitCurrent("Added a new ls file")(st2)
    val st4 = pushCurrent(st3)
    executeTask(lsync, "Synchronizing with the ls.implict.ly website")(st4)
  }
  lazy val notifyHerald = ReleaseStep (
    action = (st: State) => {
      Process("herald &").lines; st.log.info("Starting herald to publish the release notes"); st
      commitCurrent("Updated the release notes")(st)
    },
    check  = (st: State) => {
      st.log.info("Checking if herald is installed")
      if ("which herald".!<(st.log) != 0) sys.error("You must install 'herald': http://github.com/n8han/herald on your machine")
      st
    }
  )

  /**
   * UTILITIES
   */
  private def executeStepTask(task: TaskKey[_], info: String) = ReleaseStep { st: State =>
    executeTask(task, info)(st)
  }

  private def executeTask(task: TaskKey[_], info: String) = (st: State) => {
    st.log.info(info)
    val extracted = Project.extract(st)
    val ref: ProjectRef = extracted.get(thisProjectRef)
    extracted.runTask(task in ref, st)._1
  }

  private def executeStepTask(task: TaskKey[_], info: String, configuration: Configuration) = ReleaseStep { st: State =>
    executeTask(task, info, configuration)(st)
  }

  private def executeTask(task: TaskKey[_], info: String, configuration: Configuration) = (st: State) => {
    st.log.info(info)
    val extracted = Project.extract(st)
    val ref: ProjectRef = extracted.get(thisProjectRef)
    extracted.runTask(task in configuration in ref, st)._1
  }

  private def commitCurrent(commitMessage: String): State => State = { st: State =>
    vcs(st).add(".") !! st.log
    val status = (vcs(st).status !!) trim

    if (status.nonEmpty) {
      vcs(st).commit(commitMessage) ! st.log
      st
    } else st
  }

  private def pushCurrent: State => State = { st: State =>
    vcs(st).pushChanges !! st.log
    st
  }

  private def vcs(st: State): Vcs = {
    st.extract.get(versionControlSystem).getOrElse(sys.error("Aborting release. Working directory is not a repository of a recognized VCS."))
  }

}


