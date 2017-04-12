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
import ohnosequences.sbt.GithubRelease.keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

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
  ).aggregate(
    fpJvm, commonJvm, matcherJvm, coreJvm, matcherExtraJvm, catsJvm, scalazJvm, htmlJvm, analysisJvm,
    shapelessJvm, formJvm, markdownJvm, gwtJvm, junitJvm, scalacheckJvm, mockJvm, tests,
    fpJs, commonJs, matcherJs, coreJs, matcherExtraJs, catsJs, scalazJs, htmlJs, analysisJs,
    shapelessJs, formJs, markdownJs, gwtJs, junitJs, scalacheckJs, mockJs)
   .enablePlugins(GitBranchPrompt)

  /** COMMON SETTINGS */
  lazy val specs2Settings: Seq[Settings] = Seq(
    organization := "org.specs2",
    specs2Version in GlobalScope := version.value,
    scalazVersion in GlobalScope := "7.2.7",
    specs2ShellPrompt,
    scalaVersion := "2.12.1",
    crossScalaVersions := Seq(scalaVersion.value, "2.11.9"))

  lazy val specs2Version = settingKey[String]("defines the current specs2 version")
  lazy val scalazVersion = settingKey[String]("defines the current scalaz version")

  def moduleSettings(name: String): Seq[Settings] =
      coreDefaultSettings  ++
      depends.resolvers    ++
      promulgate.library("org.specs2.info"+(if (name.nonEmpty) s".$name" else ""), "specs2") ++
      specs2Settings       ++
      compilationSettings  ++
      testingSettings      ++
      publicationSettings  ++
      notificationSettings

  /** MODULES (sorted in alphabetical order) */
  lazy val analysis = crossProject.in(file("analysis")).
    settings(Seq(
      libraryDependencies ++= depends.classycle ++ depends.compiler(scalaOrganization.value, scalaVersion.value)) ++
    moduleSettings("analysis") ++
    Seq(name := "specs2-analysis"):_*)

  lazy val analysisJs  = analysis.js.dependsOn(commonJs % "test->test", coreJs, matcherJs, scalacheckJs % "test")
  lazy val analysisJvm = analysis.jvm.dependsOn(commonJvm % "test->test", coreJvm, matcherJvm, scalacheckJvm % "test")

  lazy val fp = crossProject.in(file("fp")).
    settings(moduleSettings("fp"):_*).
    settings(name := "specs2-fp")

  lazy val fpJvm = fp.jvm
  lazy val fpJs  = fp.js

  lazy val common = crossProject.in(file("common")).
    settings(moduleSettings("common") ++
      Seq(conflictWarning ~= { _.copy(failOnConflict = false) }, // lame
          libraryDependencies ++=
            depends.reflect(scalaOrganization.value, scalaVersion.value) ++
            depends.paradise(scalaVersion.value) ++
            depends.scalaParser(scalaVersion.value) ++
            depends.scalaXML(scalaVersion.value) ++
            depends.scalacheck(scalaVersion.value).map(_ % "test"),
        name := "specs2-common"
      ):_*)

  lazy val commonJs  = common.js.dependsOn(fpJs)
  lazy val commonJvm = common.jvm.dependsOn(fpJvm)

  lazy val core = crossProject.in(file("core")).
    settings(Seq(
      libraryDependencies ++=
        depends.paradise(scalaVersion.value) ++
        depends.testInterface.map(_ % "optional") ++
        depends.mockito.map(_ % "test") ++
        depends.junit.map(_ % "test")) ++
      moduleSettings("core") ++
      Seq(name := "specs2-core"):_*)

  lazy val coreJs  = core.js.dependsOn(matcherJs, commonJs % "test->test")
  lazy val coreJvm = core.jvm.dependsOn(matcherJvm, commonJvm % "test->test")

  lazy val examples = crossProject.in(file("examples")).
    settings(moduleSettings("examples") ++
      Seq(name := "specs2-examples"):_*)

  lazy val examplesJs  = examples.js.dependsOn(commonJs, matcherJs, coreJs, matcherExtraJvm, analysisJs, formJs, htmlJs, markdownJs, gwtJs, junitJs, scalacheckJs, mockJs)
  lazy val examplesJvm = examples.jvm.dependsOn(commonJvm, matcherJvm, coreJvm, matcherExtraJvm, analysisJvm, formJvm, htmlJvm, markdownJvm, gwtJvm, junitJvm, scalacheckJvm, mockJvm)

  lazy val form = crossProject.in(file("form")).
    settings(moduleSettings("form") ++
      Seq(name := "specs2-form"):_*)

  lazy val formJs = form.js.dependsOn(coreJs, markdownJs, matcherExtraJs, scalacheckJs % "test->test")
  lazy val formJvm = form.jvm.dependsOn(coreJvm, markdownJvm, matcherExtraJvm, scalacheckJvm % "test->test")

  lazy val guide = Project(id = "guide", base = file("guide"),
    settings = moduleSettings("guide") ++
      Seq(name := "specs2-guide") ++
      documentationSettings
  ).dependsOn(examplesJvm % "compile->compile;test->test", scalazJvm, shapelessJvm)

  lazy val gwt = crossProject.in(file("gwt")).
    settings(Seq(
      libraryDependencies ++= depends.shapeless(scalaVersion.value)) ++
      moduleSettings("gwt") ++
      Seq(name := "specs2-gwt"):_*)
  
  lazy val gwtJs = gwt.js.dependsOn(coreJs, matcherExtraJs, scalacheckJs)
  lazy val gwtJvm = gwt.jvm.dependsOn(coreJvm, matcherExtraJvm, scalacheckJvm)

  lazy val html = crossProject.in(file("html")).
    settings(
      Seq(libraryDependencies += depends.tagsoup) ++
      moduleSettings("html") ++
      Seq(name := "specs2-html"):_*)
  
  lazy val htmlJs = html.js.dependsOn(formJs, mockJs % "test", matcherExtraJs % "test", scalacheckJs % "test")
  lazy val htmlJvm = html.jvm.dependsOn(formJvm, mockJvm % "test", matcherExtraJvm % "test", scalacheckJvm % "test")

  lazy val junit = crossProject.in(file("junit")).
    settings(Seq(
      libraryDependencies ++= depends.junit ++ depends.mockito.map(_ % "test")) ++
      moduleSettings("junit") ++
      Seq(name := "specs2-junit"):_*)
  
  lazy val junitJs = junit.js.dependsOn(coreJs, matcherExtraJs % "test", mockJs % "test")
  lazy val junitJvm = junit.jvm.dependsOn(coreJvm, matcherExtraJvm % "test", mockJvm % "test")

  lazy val markdown = crossProject.in(file("markdown")).
    settings(Seq(
     libraryDependencies ++= depends.pegdown) ++
      moduleSettings("markdown") ++
      Seq(name := "specs2-markdown"):_*)
  
  lazy val markdownJs = markdown.js.dependsOn(commonJs, coreJs % "compile->test")
  lazy val markdownJvm = markdown.jvm.dependsOn(commonJvm, coreJvm % "compile->test")

  lazy val matcher = crossProject.in(file("matcher")).
    settings(moduleSettings("matcher") ++
      Seq(name := "specs2-matcher"):_*)

  lazy val matcherJs  = matcher.js.dependsOn(commonJs)
  lazy val matcherJvm = matcher.jvm.dependsOn(commonJvm)

  lazy val matcherExtra = crossProject.in(file("matcher-extra")).
    settings(moduleSettings("matcherextra") ++ Seq(
      name := "specs2-matcher-extra",
      libraryDependencies ++= depends.paradise(scalaVersion.value)
    ):_*)

  lazy val matcherExtraJs  = matcherExtra.js.dependsOn(analysisJs, matcherJs, coreJs % "test->test")
  lazy val matcherExtraJvm = matcherExtra.jvm.dependsOn(analysisJvm, matcherJvm, coreJvm % "test->test")

  lazy val shapeless = crossProject.in(file("shapeless")).
    settings(moduleSettings("shapeless") ++
      Seq(name := "specs2-shapeless",
        libraryDependencies ++=
          depends.paradise(scalaVersion.value) ++
          depends.shapeless(scalaVersion.value)
      ):_*)
  
  lazy val shapelessJs = shapeless.js.dependsOn(matcherJs)
  lazy val shapelessJvm = shapeless.jvm.dependsOn(matcherJvm)

  lazy val cats = crossProject.in(file("cats")).
    settings(moduleSettings("cats") ++
      Seq(libraryDependencies ++= (
        if (scalaMinorVersionAtLeast(scalaVersion.value, 12))
          Seq()
        else
          depends.cats)) ++
      Seq(name := "specs2-cats") ++
      Seq((skip in compile) := scalaMinorVersionAtLeast(scalaVersion.value, 12),
          publishArtifact := !scalaMinorVersionAtLeast(scalaVersion.value, 12)):_*)
  
  lazy val catsJs = cats.js.dependsOn(matcherJs, coreJs % "test->test")
  lazy val catsJvm = cats.jvm.dependsOn(matcherJvm, coreJvm % "test->test")

  lazy val scalaz = crossProject.in(file("scalaz")).
    settings(moduleSettings("scalaz") ++
      Seq(libraryDependencies ++=
        depends.scalaz(scalazVersion.value) ++
        depends.scalazConcurrent(scalazVersion.value)) ++
      Seq(name := "specs2-scalaz"):_*)
  
  lazy val scalazJs = scalaz.js.dependsOn(matcherJs, coreJs % "test->test")
  lazy val scalazJvm = scalaz.jvm.dependsOn(matcherJvm, coreJvm % "test->test")

  lazy val mock = crossProject.in(file("mock")).
    settings(Seq(
      libraryDependencies ++=
        depends.hamcrest ++
        depends.mockito) ++
      moduleSettings("mock") ++
      Seq(name := "specs2-mock"):_*)
  
  lazy val mockJs = mock.js.dependsOn(coreJs)
  lazy val mockJvm = mock.jvm.dependsOn(coreJvm)

  lazy val scalacheck = crossProject.in(file("scalacheck")).
    settings(Seq(
      libraryDependencies ++= depends.scalacheck(scalaVersion.value)) ++
      moduleSettings("scalacheck") ++
      Seq(name := "specs2-scalacheck"):_*)

  lazy val scalacheckJs  = scalacheck.js.dependsOn(coreJs)
  lazy val scalacheckJvm = scalacheck.jvm.dependsOn(coreJvm)

  lazy val tests = Project(id = "tests", base = file("tests"),
    settings = moduleSettings("tests") ++
      Seq(name := "specs2-tests") ++
      Seq(libraryDependencies ++= depends.scalaParallelCollections(scalaVersion.value))
  ).dependsOn(
    coreJvm % "compile->compile;test->test", shapelessJvm % "compile->compile;test->test",
    junitJvm % "test->test", examplesJvm % "test->test",
    matcherExtraJvm, htmlJvm, scalazJvm)

  lazy val specs2ShellPrompt = shellPrompt in ThisBuild := { state =>
    val name = Project.extract(state).currentRef.project
    (if (name == "specs2") "" else name) + "> "
  }

  def scalaSourceVersion(scalaBinaryVersion: String) =
    if (scalaBinaryVersion.startsWith("2.11"))
      "2.11"
    else
      "2.12"

  lazy val compilationSettings: Seq[Settings] = Seq(
    // https://gist.github.com/djspiewak/976cd8ac65e20e136f05
    unmanagedSourceDirectories in Compile ++=
      Seq((sourceDirectory in Compile).value / s"scala-${scalaSourceVersion(scalaBinaryVersion.value)}",
          (sourceDirectory in Compile).value / s"scala-scalaz-7.1.x",
          (sourceDirectory in (Test, test)).value / s"scala-scalaz-7.1.x"),
    javacOptions ++= Seq("-Xmx3G", "-Xms512m", "-Xss4m"),
    maxErrors := 20,
    incOptions := incOptions.value.withNameHashing(true),
    scalacOptions in Compile ++=
      (if (scalaMinorVersionAtLeast(scalaVersion.value, 11))
        Seq("-Xfatal-warnings",
            "-Xlint",
            "-Ywarn-unused-import",
            "-Yno-adapted-args",
            "-Ywarn-numeric-widen",
            "-Ywarn-value-discard",
            "-deprecation:false", "-Xcheckinit", "-unchecked", "-feature", "-language:_")
       else
        Seq("-Xcheckinit", "-Xlint", "-deprecation", "-unchecked", "-feature", "-language:_")),
    scalacOptions += "-Ypartial-unification",
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
    scalacOptions in Test ++= Seq("-Yrangepos"),
    scalacOptions in (Compile, doc) ++= Seq("-feature", "-language:_"),
    scalacOptions in (Compile, console) ++= Seq("-Yrangepos", "-feature", "-language:_"),
    scalacOptions in (Test, console) ++= Seq("-Yrangepos", "-feature", "-language:_")
  )

  lazy val testingSettings: Seq[Settings] = Seq(
    initialCommands in console in test := "import org.specs2._",
    logBuffered := false,
    cancelable in Global := true,
    testFrameworks := Seq(TestFramework("org.specs2.runner.Specs2Framework")),
    javaOptions ++= Seq("-Xmx3G", "-Xss4M"),
    testOptions := Seq(Tests.Filter(s =>
      (Seq(".guide.").exists(s.contains) || Seq("Spec", "Guide", "Website").exists(s.endsWith)) &&
        Seq("Specification", "FeaturesSpec").forall(n => !s.endsWith(n))))
  )

  /**
   * RELEASE PROCESS
   */
  lazy val releaseSettings: Seq[Settings] =
    ReleasePlugin.releaseSettings ++ Seq(
    tagName := "SPECS2-" + (version in ThisBuild).value,
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
    releaseSiteProcess := Seq[ReleaseStep](
      inquireVersions,
      setReleaseVersion,
      generateWebsite,
      executeStepTask(makeSite, "make the site", Compile),
      publishSite
    ),
    commands ++= Seq(releaseOfficialCommand, releaseJarsCommand, releaseSiteCommand)
    ) ++
  documentationSettings ++
  apiSettings               ++
  Seq(scalacOptions in (Compile, doc) += "-Ymacro-no-expand") ++
  Seq(sources in (Compile, doc) in commonJvm :=
        (if (!scalaMinorVersionAtLeast(scalaVersion.value, 11))
           List()
         else (sources in (Compile, doc) in commonJvm).value),
    sources in (Compile, doc) in coreJvm := List(),
    sources in (Compile, doc) in matcherExtraJvm := List())

  lazy val apiSettings: Seq[Settings] = Seq(
    sources                      in (Compile, doc) := sources.all(aggregateCompile).value.flatten,
    unmanagedSources             in (Compile, doc) := unmanagedSources.all(aggregateCompile).value.flatten,
    unmanagedSourceDirectories   in (Compile, doc) := unmanagedSourceDirectories.all(aggregateCompile).value.flatten,
    unmanagedResourceDirectories in (Compile, doc) := unmanagedResourceDirectories.all(aggregateCompile).value.flatten,
    libraryDependencies                            := libraryDependencies.all(aggregateTest).value.flatten.map(maybeMarkProvided)
  )

  lazy val aggregateCompile = ScopeFilter(
    inProjects(
      commonJvm, matcherJvm, matcherExtraJvm, coreJvm, htmlJvm, analysisJvm, formJvm, markdownJvm, gwtJvm, junitJvm, scalacheckJvm, mockJvm,
      commonJs, matcherJs, matcherExtraJs, coreJs, htmlJs, analysisJs, formJs, markdownJs, gwtJs, junitJs, scalacheckJs, mockJs),
    inConfigurations(Compile))

  lazy val aggregateTest = ScopeFilter(
    inProjects(commonJvm, matcherJvm, matcherExtraJvm, coreJvm, htmlJvm, analysisJvm, formJvm, markdownJvm, gwtJvm, junitJvm, scalacheckJvm, mockJvm, guide, examplesJvm,
      commonJs, matcherJs, matcherExtraJs, coreJs, htmlJs, analysisJs, formJs, markdownJs, gwtJs, junitJs, scalacheckJs, mockJs, examplesJs),
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

  lazy val releaseSiteProcess = SettingKey[Seq[ReleaseStep]]("release-site")
  private lazy val releaseSiteCommandKey = "release-site"
  private val releaseSiteParser = (Space ~> WithDefaults | Space ~> CrossBuild).*

  val releaseSiteCommand: Command = Command(releaseSiteCommandKey)(_ => releaseSiteParser) { (st, args) =>
    val extracted = Project.extract(st)
    val releaseParts = extracted.get(releaseSiteProcess)
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
    siteSourceDirectory := target.value / "specs2-reports" / "site",
    // copy the api files to a versioned directory
    siteMappings ++= { (mappings in packageDoc in Compile).value.map { case (f, d) => (f, s"api/SPECS2-${version.value}/$d") } },
    includeFilter in makeSite := AllPassFilter,
    // override the synchLocal task to avoid removing the existing files
    synchLocal := {
      val betterMappings = privateMappings.value map { case (file, target) => (file, updatedRepository.value / target) }
      IO.copy(betterMappings)
      updatedRepository.value
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
    task := Def.taskDyn {
      Def.task(
        Defaults.allTestGroupsTask(
          (streams in Test).value,
          (loadedTestFrameworks in Test).value,
          (testLoader in Test).value,
          (testGrouping in Test in test).value,
          (testExecution in Test in task).value,
          (fullClasspath in Test in test).value,
          (javaHome in test).value
        )).flatMap(identity)
    }.value

  /**
   * PUBLICATION
   */
  lazy val publishSignedArtifacts = executeAggregateTask(publishSigned, "Publishing signed artifacts")

  lazy val releaseToSonatype = executeStepTask(sonatypeReleaseAll, "Closing and promoting the Sonatype repo")

  lazy val publicationSettings: Seq[Settings] = Seq(
    publishTo in Global := Some("staging" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"),
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

  lazy val notificationSettings: Seq[Settings] = Seq(
    ghreleaseRepoOrg := "etorreborre",
    ghreleaseRepoName := "specs2",
    ghreleaseNotes := { tagName: TagName =>
      // find the corresponding release notes
      val notesFilePath = s"notes/${tagName.replace("SPECS2-", "")}.markdown"
      try io.Source.fromFile(notesFilePath).mkString
      catch { case t: Throwable => throw new Exception(s"the path $notesFilePath not found for tag $tagName") }
    },
    // just upload the notes
    ghreleaseAssets := Seq()
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


