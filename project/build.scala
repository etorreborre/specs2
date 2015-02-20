import java.text.SimpleDateFormat
import java.util.Date
import sbt._
import complete.DefaultParsers._
import Keys._
import com.typesafe.sbt._
import pgp.PgpKeys._
import SbtSite._
import SiteKeys._
import SbtGit._
import GitKeys._
import SbtGhPages._
import GhPagesKeys._
import sbtrelease._
import ReleasePlugin._
import ReleaseKeys._
import ReleaseStateTransformations._
import Utilities._
import Defaults._
import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import com.typesafe.tools.mima.plugin.MimaKeys.previousArtifact
import xerial.sbt.Sonatype._
import SonatypeKeys._
import depends._
import com.ambiata.promulgate.project.ProjectPlugin._

object build extends Build {
  type Settings = Def.Setting[_]

  /** MAIN PROJECT */
  lazy val specs2 = Project(
    id = "specs2",
    base = file("."),
    settings = 
      moduleSettings("")       ++
      siteSettings             ++
      releaseSettings          ++
      compatibilitySettings    ++
      Seq(name := "specs2")
  ).aggregate(common, matcher, matcherExtra, core, html, analysis, form, markdown, gwt, junit, scalacheck, mock, tests)
  
  /** COMMON SETTINGS */
  lazy val specs2Settings: Seq[Settings] = Seq(
    organization := "org.specs2",
    specs2Version in GlobalScope <<= version,
    specs2ShellPrompt,
    scalaVersion := "2.11.5",
    scalazVersion := "7.1.1",
    crossScalaVersions := Seq("2.10.4", scalaVersion.value))

  lazy val specs2Version = settingKey[String]("defines the current specs2 version")

  def moduleSettings(name: String): Seq[Settings] =
      coreDefaultSettings  ++
      depends.resolvers    ++
      promulgate.library("org.specs2.info"+(if (name.nonEmpty)s".$name" else ""), "specs2") ++
      specs2Settings       ++
      compilationSettings  ++
      testingSettings      ++
      publicationSettings

  /** MODULES (sorted in alphabetical order) */
  lazy val analysis = Project(id = "analysis", base = file("analysis"),
    settings = Seq(
      libraryDependencies ++= depends.classycle ++ depends.compiler(scalaVersion.value)) ++
    moduleSettings("analysis") ++
    Seq(name := "specs2-analysis")
  ).dependsOn(common % "test->test", core, matcher, scalacheck % "test")

  lazy val common = Project(id = "common", base = file("common"),
    settings = moduleSettings("common") ++
      Seq(conflictWarning ~= { _.copy(failOnConflict = false) }, // lame
          libraryDependencies ++=
            depends.scalaz(scalazVersion.value) ++
            depends.reflect(scalaVersion.value) ++
            depends.paradise(scalaVersion.value) ++
            depends.scalacheck.map(_ % "test"),
          name := "specs2-common")
  )

  lazy val core = Project(id = "core", base = file("core"),
    settings = Seq(
      libraryDependencies ++=
        depends.shapeless(scalaVersion.value) ++
        depends.reflect(scalaVersion.value) ++
        depends.paradise(scalaVersion.value) ++
        depends.testInterface.map(_ % "optional") ++
        depends.mockito.map(_ % "test") ++
        depends.junit.map(_ % "test")) ++
      moduleSettings("core") ++
      Seq(name := "specs2-core")
  ).dependsOn(matcher, common % "test->test")

  lazy val examples = Project(id = "examples", base = file("examples"),
    settings = moduleSettings("examples") ++
      Seq(name := "specs2-examples")
  ).dependsOn(common, matcher, matcherExtra, core, analysis, form, html, markdown, gwt, junit, scalacheck, mock)

  lazy val form = Project(id = "form", base = file("form"),
    settings = moduleSettings("form") ++
      Seq(name := "specs2-form")
  ).dependsOn(core, markdown, matcherExtra, scalacheck % "test->test")

  lazy val guide = Project(id = "guide", base = file("guide"),
    settings = moduleSettings("guide") ++
      Seq(name := "specs2-guide")
  ).dependsOn(examples % "compile->compile;test->test")

  lazy val gwt = Project(id = "gwt", base = file("gwt"),
    settings = Seq(
      libraryDependencies ++= depends.shapeless(scalaVersion.value)) ++
      moduleSettings("gwt") ++
      Seq(name := "specs2-gwt")
  ).dependsOn(core, matcherExtra, scalacheck)

  lazy val html = Project(id = "html", base = file("html"),
    settings =
      Seq(libraryDependencies += depends.tagsoup) ++
      moduleSettings("html") ++
      Seq(name := "specs2-html")
  ).dependsOn(form, mock % "test", matcherExtra % "test", scalacheck % "test")

  lazy val junit = Project(id = "junit", base = file("junit"),
    settings = Seq(
      libraryDependencies ++= depends.junit ++ depends.mockito.map(_ % "test")) ++
      moduleSettings("junit") ++
      Seq(name := "specs2-junit")
  ).dependsOn(core, matcherExtra % "test", mock % "test")

  lazy val markdown = Project(id = "markdown", base = file("markdown"),
    settings = Seq(
     libraryDependencies ++= depends.pegdown ++ depends.shapeless(scalaVersion.value)) ++
      moduleSettings("markdown") ++
      Seq(name := "specs2-markdown")
  ).dependsOn(common, core % "compile->test")

  lazy val matcher = Project(id = "matcher", base = file("matcher"),
    settings = moduleSettings("matcher") ++
      Seq(name := "specs2-matcher")
  ).dependsOn(common)

  lazy val matcherExtra = Project(id = "matcher-extra", base = file("matcher-extra"),
    settings = moduleSettings("matcherextra") ++ Seq(
      name := "specs2-matcher-extra",
      libraryDependencies ++= depends.paradise(scalaVersion.value)
    )
  ).dependsOn(analysis, matcher, core % "test->test")

  lazy val mock = Project(id = "mock", base = file("mock"),
    settings = Seq(
      libraryDependencies ++=
        depends.hamcrest ++
        depends.mockito) ++
      moduleSettings("mock") ++
      Seq(name := "specs2-mock")
  ).dependsOn(core)

  lazy val scalacheck = Project(id = "scalacheck", base = file("scalacheck"),
    settings = Seq(
      libraryDependencies ++= depends.scalacheck) ++
      moduleSettings("scalacheck") ++
      Seq(name := "specs2-scalacheck")
  ).dependsOn(core)

  lazy val tests = Project(id = "tests", base = file("tests"),
    settings = moduleSettings("tests") ++
      Seq(name := "specs2-tests")
  ).dependsOn(core % "compile->compile;test->test", matcherExtra, junit % "test->test", examples % "test->test")

  lazy val specs2ShellPrompt = shellPrompt in ThisBuild := { state =>
    val name = Project.extract(state).currentRef.project
    (if (name == "specs2") "" else name) + "> " 
  }

  lazy val compilationSettings: Seq[Settings] = Seq(
    // https://gist.github.com/djspiewak/976cd8ac65e20e136f05
    unmanagedSourceDirectories in Compile ++=
      Seq((sourceDirectory in Compile).value / s"scala-${scalaBinaryVersion.value}",
          (sourceDirectory in Compile).value / s"scala-scalaz-${scalazVersion.value}",
          (sourceDirectory in (Test, test)).value / s"scala-scalaz-${scalazVersion.value}"),
    javacOptions ++= Seq("-Xmx3G", "-Xms512m", "-Xss4m"),
    maxErrors := 20,
    incOptions := incOptions.value.withNameHashing(true),
    scalacOptions in GlobalScope ++=
      (if (scalaVersion.value == "2.11") Seq("-Ywarn-unused-import", "-Xcheckinit", "-Xlint", "-deprecation", "-unchecked", "-feature", "-language:_")
       else                              Seq("-Xcheckinit", "-Xlint", "-deprecation", "-unchecked", "-feature", "-language:_")),
    scalacOptions in Test ++= Seq("-Yrangepos"),
    scalacOptions in (Compile, console) ++= Seq("-Yrangepos", "-feature", "-language:_"),
    scalacOptions in (Test, console) ++= Seq("-Yrangepos", "-feature", "-language:_")
  )


  lazy val testingSettings: Seq[Settings] = Seq(
    initialCommands in console in test := "import org.specs2._",
    logBuffered := false,
    cancelable := true,
    javaOptions ++= Seq("-Xmx3G", "-Xss4M"),
    fork in test := true,
    testOptions := Seq(Tests.Filter(s => Seq(".guide.").exists(s.contains) || Seq("Spec", "Guide", "Index").exists(s.endsWith) && Seq("Specification", "FeaturesSpec").forall(n => !s.endsWith(n))))
  )

  /**
   * RELEASE PROCESS
   */
  lazy val releaseSettings: Seq[Settings] =
    ReleasePlugin.releaseSettings ++ Seq(
    tagName <<= (version in ThisBuild) map (v => "SPECS2-" + v),
    crossBuild := true,
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      setReleaseVersion,
      commitReleaseVersion,
      generateUserGuide,
      generateIndexPage,
      publishSite,
      ReleaseStep(publishSignedArtifacts, check = identity, enableCrossBuild = true),
      releaseToSonatype,
      notifyHerald,
      tagRelease,
      setNextVersion,
      commitNextVersion,
      pushChanges
    ),
    releaseSnapshotProcess := Seq[ReleaseStep](
      generateUserGuide,
      publishSite,
      ReleaseStep(publishSignedArtifacts, check = identity, enableCrossBuild = true)
      ),
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
  lazy val publishSignedArtifacts = executeAggregateTask(publishSigned, "Publishing signed artifacts")

  lazy val releaseToSonatype = executeStepTask(sonatypeReleaseAll, "Closing and promoting the Sonatype repo")

  lazy val publicationSettings: Seq[Settings] = Seq(
    publishTo in Global <<= version { v: String =>
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
  ) ++
  sonatypeSettings

  /**
   * NOTIFICATION
   */
  lazy val notifyHerald = ReleaseStep (
    action = (st: State) => {
      Process("herald &").lines; st.log.info("Starting herald to publish the release notes")
      commitCurrent("Updated the release notes")(st)
    },
    check  = (st: State) => {
      st.log.info("Checking if herald is installed")
      if ("which herald".!<(st.log) != 0) sys.error("You must install 'herald': http://github.com/n8han/herald on your machine")
      st
    }
  )

  /**
   * COMPATIBILITY
   */
  lazy val compatibilitySettings = mimaDefaultSettings ++
    Seq(previousArtifact := Some("org.specs2" % "specs2_2.10" % "2.3.7"))

  /**
   * UTILITIES
   */

  /** Mark some dependencies of the full artifact as provided */
  def maybeMarkProvided(dep: ModuleID): ModuleID =
    if (providedDependenciesInAggregate.exists(dep.name.startsWith)) dep.copy(configurations = Some("provided"))
    else dep

  /* A list of dependency module names that should be marked as "provided" for the aggregate artifact */
  lazy val providedDependenciesInAggregate = Seq("shapeless")

  private def executeStepTask(task: TaskKey[_], info: String) = ReleaseStep { st: State =>
    executeTask(task, info)(st)
  }

  private def executeAggregateTask(task: TaskKey[_], info: String) = (st: State) => {
    st.log.info(info)
    val extracted = Project.extract(st)
    val ref: ProjectRef = extracted.get(thisProjectRef)
    extracted.runAggregated(task in ref, st)
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
    val status = (vcs(st).status !!).trim

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


