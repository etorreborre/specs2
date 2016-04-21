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
      compatibilitySettings    ++
      releaseSettings          ++
      siteSettings             ++
      Seq(name := "specs2", packagedArtifacts := Map.empty)
  ).aggregate(common, matcher, matcherExtra, cats, core, html, analysis, form, markdown, gwt, junit, scalacheck, mock, tests)
   .enablePlugins(GitBranchPrompt)

  /** COMMON SETTINGS */
  lazy val specs2Settings: Seq[Settings] = Seq(
    organization := "org.specs2",
    specs2Version in GlobalScope <<= version,
    specs2ShellPrompt,
    scalaVersion := "2.11.7",
    scalazVersion := "7.2.0",
    crossScalaVersions := Seq(scalaVersion.value, "2.12.0-M3", "2.10.6"))

  lazy val specs2Version = settingKey[String]("defines the current specs2 version")

  def moduleSettings(name: String): Seq[Settings] =
      coreDefaultSettings  ++
      depends.resolvers    ++
      promulgate.library("org.specs2.info"+(if (name.nonEmpty) s".$name" else ""), "specs2") ++
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
            depends.scalaz(scalazVersion.value, scalaVersion.value) ++
            depends.reflect(scalaVersion.value) ++
            depends.paradise(scalaVersion.value) ++
            depends.kindp ++
            depends.scalacheck.map(_ % "test"),
          name := "specs2-common")
  )

  lazy val core = Project(id = "core", base = file("core"),
    settings = Seq(
      libraryDependencies ++=
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
      Seq(name := "specs2-guide") ++
      documentationSettings
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

  lazy val cats = Project(id = "cats", base = file("cats"),
    settings = moduleSettings("cats") ++
      Seq(libraryDependencies ++= depends.cats) ++
      Seq(name := "specs2-cats")
  ).dependsOn(matcher, core % "test->test")

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
  ).dependsOn(core % "compile->compile;test->test", matcherExtra, junit % "test->test", examples % "test->test", html)

  lazy val specs2ShellPrompt = shellPrompt in ThisBuild := { state =>
    val name = Project.extract(state).currentRef.project
    (if (name == "specs2") "" else name) + "> "
  }

  lazy val compilationSettings: Seq[Settings] = Seq(
    // https://gist.github.com/djspiewak/976cd8ac65e20e136f05
    unmanagedSourceDirectories in Compile ++=
      Seq((sourceDirectory in Compile).value / s"scala-${scalaBinaryVersion.value}",
          if (scalazVersion.value.startsWith("7.0")) (sourceDirectory in Compile).value / s"scala-scalaz-7.0.x"
          else                                       (sourceDirectory in Compile).value / s"scala-scalaz-7.1.x",
          if (scalazVersion.value.startsWith("7.0")) (sourceDirectory in (Test, test)).value / s"scala-scalaz-7.0.x"
          else                                       (sourceDirectory in (Test, test)).value / s"scala-scalaz-7.1.x"),
    javacOptions ++= Seq("-Xmx3G", "-Xms512m", "-Xss4m"),
    maxErrors := 20,
    incOptions := incOptions.value.withNameHashing(true),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1"),
    scalacOptions in GlobalScope ++=
      (if (scalaVersion.value.startsWith("2.11") || scalaVersion.value.startsWith("2.12"))
        Seq("-Xfatal-warnings",
            "-Xlint",
            "-Ywarn-unused-import",
            "-Yno-adapted-args",
            "-Ywarn-numeric-widen",
            "-Ywarn-value-discard",
            "-deprecation:false", "-Xcheckinit", "-unchecked", "-feature", "-language:_")
       else
        Seq("-Xcheckinit", "-Xlint", "-deprecation", "-unchecked", "-feature", "-language:_")),
    scalacOptions in Test ++= Seq("-Yrangepos"),
    scalacOptions in (Compile, console) := Seq("-Yrangepos", "-feature", "-language:_"),
    scalacOptions in (Test, console) := Seq("-Yrangepos", "-feature", "-language:_")
  )

  lazy val testingSettings: Seq[Settings] = Seq(
    initialCommands in console in test := "import org.specs2._",
    logBuffered := false,
    cancelable in Global := true,
    testFrameworks := Seq(TestFramework("org.specs2.runner.Specs2Framework")),
    javaOptions ++= Seq("-Xmx3G", "-Xss4M"),
    fork in test := true,
    testOptions := Seq(Tests.Filter(s =>
      (Seq(".guide.").exists(s.contains) || Seq("Spec", "Guide", "Website").exists(s.endsWith)) &&
        Seq("Specification", "FeaturesSpec").forall(n => !s.endsWith(n))))
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
      tagRelease,
      generateWebsite,
      executeStepTask(makeSite, "make the site", Compile),
      publishSite,
      ReleaseStep(publishSignedArtifacts, check = identity, enableCrossBuild = true),
      releaseToSonatype,
      pushChanges
    ),
    releaseJarsProcess := Seq[ReleaseStep](
      inquireVersions,
      setReleaseVersion,
      ReleaseStep(publishSignedArtifacts, check = identity, enableCrossBuild = true),
      releaseToSonatype
    ),
    releaseOfficialProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      setReleaseVersion,
      tagRelease,
      generateWebsite,
      executeStepTask(makeSite, "make the site", Compile),
      publishSite,
      ReleaseStep(publishSignedArtifacts, check = identity, enableCrossBuild = true),
      releaseToSonatype,
      notifyHerald,
      pushChanges
    ),
    commands ++= Seq(releaseOfficialCommand, releaseJarsCommand)
    ) ++
  documentationSettings ++
  apiSettings               ++
  Seq(scalacOptions in (Compile, doc) += "-Ymacro-no-expand")

  lazy val apiSettings: Seq[Settings] = Seq(
    sources                      in (Compile, doc) := sources.all(aggregateCompile).value.flatten,
    unmanagedSources             in (Compile, doc) := unmanagedSources.all(aggregateCompile).value.flatten,
    unmanagedSourceDirectories   in (Compile, doc) := unmanagedSourceDirectories.all(aggregateCompile).value.flatten,
    unmanagedResourceDirectories in (Compile, doc) := unmanagedResourceDirectories.all(aggregateCompile).value.flatten,
    libraryDependencies                            := libraryDependencies.all(aggregateTest).value.flatten.map(maybeMarkProvided)
  )

  lazy val aggregateCompile = ScopeFilter(
    inProjects(common, matcher, matcherExtra, core, html, analysis, form, markdown, gwt, junit, scalacheck, mock),
    inConfigurations(Compile))

  lazy val aggregateTest = ScopeFilter(
    inProjects(common, matcher, matcherExtra, core, html, analysis, form, markdown, gwt, junit, scalacheck, mock, guide, examples),
    inConfigurations(Test))

  lazy val releaseOfficialProcess = SettingKey[Seq[ReleaseStep]]("release-official-process")
  private lazy val releaseOfficialCommandKey = "release-official"
  private val WithDefaults = "with-defaults"
  private val CrossBuild = "cross"
  private val releaseOfficialParser = (Space ~> WithDefaults | Space ~> CrossBuild).*

  val releaseOfficialCommand: Command = Command(releaseOfficialCommandKey)(_ => releaseOfficialParser) { (st, args) =>
    val extracted = Project.extract(st)
    val releaseParts = extracted.get(releaseOfficialProcess)
    val crossEnabled = extracted.get(crossBuild) || args.contains(CrossBuild)
    val startState = st
      .put(useDefaults, args.contains(WithDefaults))
      .put(ReleaseKeys.cross, crossEnabled)

    val initialChecks = releaseParts.map(_.check)
    val process = releaseParts.map(_.action)

    initialChecks.foreach(_(startState))
    Function.chain(process)(startState)
  }

  lazy val releaseJarsProcess = SettingKey[Seq[ReleaseStep]]("release-jars")
  private lazy val releaseJarsCommandKey = "release-jars"
  private val releaseJarsParser = (Space ~> WithDefaults | Space ~> CrossBuild).*

  val releaseJarsCommand: Command = Command(releaseJarsCommandKey)(_ => releaseJarsParser) { (st, args) =>
    val extracted = Project.extract(st)
    val releaseParts = extracted.get(releaseJarsProcess)
    val crossEnabled = extracted.get(crossBuild) || args.contains(CrossBuild)

    val startState = st
      .put(useDefaults, args.contains(WithDefaults))
      .put(ReleaseKeys.cross, crossEnabled)

    val initialChecks = releaseParts.map(_.check)
    val process = releaseParts.map(_.action)

    initialChecks.foreach(_(startState))
    Function.chain(process)(startState)
  }

  /**
   * DOCUMENTATION
   */
  lazy val siteSettings: Seq[Settings] = ghpages.settings ++ SbtSite.site.settings ++
    Seq(
    siteSourceDirectory <<= target (_ / "specs2-reports" / "site"),
    // copy the api files to a versioned directory
    siteMappings <++= (mappings in packageDoc in Compile, version) map { (m, v) => m.map { case (f, d) => (f, s"api/SPECS2-$v/$d") } },
    includeFilter in makeSite := AllPassFilter,
    // override the synchLocal task to avoid removing the existing files
    synchLocal <<= (privateMappings, updatedRepository, gitRunner, streams) map { (mappings, repo, git, s) =>
      val betterMappings = mappings map { case (file, target) => (file, repo / target) }
      IO.copy(betterMappings)
      repo
    },
    gitRemoteRepo := "git@github.com:etorreborre/specs2.git"
  )

  lazy val documentationSettings =
    testTaskDefinition(generateWebsiteTask, Seq(Tests.Filter(_.endsWith("Website"))))

  lazy val generateWebsiteTask = TaskKey[Tests.Output]("generate-website", "generate the website")
  lazy val generateWebsite     = executeStepTask(generateWebsiteTask in guide, "Generating the website", Test)

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
      Some("staging" at nexus + "service/local/staging/deploy/maven2")
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
          <connection>scm:git:git@github.com:etorreborre/specs2.git</connection>
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
      st
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
    Seq(previousArtifact := Some("org.specs2" %% "specs2" % "3.0"))

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
    extracted.runTask(task in configuration, st)._1
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


