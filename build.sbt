import sbt._
import Keys._
import Defaults._
import com.typesafe.sbt.pgp.PgpKeys._
import java.util.{Date, TimeZone}
import java.text.SimpleDateFormat
// shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.{crossProject, CrossType}

/** MAIN PROJECT */
lazy val specs2 = project.in(file(".")).
  enablePlugins(GitBranchPrompt, ScalaJSPlugin, GhpagesPlugin, BuildInfoPlugin).
  settings(
    moduleSettings("")  ++
    siteSettings,
    apiSettings,
    buildInfoSettings,
    Seq(name := "specs2", packagedArtifacts := Map.empty)
  ).aggregate(
      fpJvm, commonJvm, matcherJvm, coreJvm, matcherExtraJvm, scalazJvm, html, analysisJvm,
      shapelessJvm, form, markdown, gwt, junitJvm, scalacheckJvm, mockJvm, tests,
      fpJs, commonJs, matcherJs, coreJs, matcherExtraJs, scalazJs, analysisJs,
      shapelessJs, form, junitJs, scalacheckJs, mockJs)


/** COMMON SETTINGS */
lazy val specs2Settings = Seq(
  organization := "org.specs2",
  specs2Version in GlobalScope := version.value,
  scalazVersion in GlobalScope := "7.2.15",
  specs2ShellPrompt,
  scalaVersion := "2.12.3",
  crossScalaVersions := Seq(scalaVersion.value, "2.11.11", "2.13.0-M2"))

lazy val versionSettings =
  Seq(
    version := {
      import sys.process._
      if (!"git tag".!!.contains(version.value)) {
        val commish = "git log --pretty=format:%h -n 1".!!.trim
        version.value+"-"+commish+"-"+timestamp(new Date)
      }
      else
        version.value
    }
  )


lazy val latestTag = "git tag"

lazy val buildInfoSettings = Seq(
  buildInfoKeys :=
    Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion) ++
    Seq(BuildInfoKey.action("commit")(Process(s"git log --pretty=format:%h -n  1").lines.head),
        BuildInfoKey.action("timestamp")(timestamp(new Date))),
  buildInfoPackage := "org.specs2"
)

def timestamp(instant: Date, format: String = "yyyyMMddHHmmss") = {
  val formatter = new SimpleDateFormat("yyyyMMddHHmmss")
  formatter.setTimeZone(TimeZone.getTimeZone("UTC"))
  formatter.format(instant)
}

lazy val tagName = Def.setting{
  s"specs2-${version.value}"
}

lazy val commonJsSettings = Seq(
  scalacOptions += {
    val tagOrHash =
      if(isSnapshot.value) sys.process.Process("git rev-parse HEAD").lines_!.head
      else tagName.value
    val a = (baseDirectory in LocalRootProject).value.toURI.toString
    val g = "https://raw.githubusercontent.com/etorreborre/specs2/" + tagOrHash
    s"-P:scalajs:mapSourceURI:$a->$g/"
  },
  parallelExecution in Test := false
)

lazy val specs2Version = settingKey[String]("defines the current specs2 version")
lazy val scalazVersion = settingKey[String]("defines the current scalaz version")
lazy val shapelessVersion = "2.3.2"

def moduleSettings(name: String) =
  coreDefaultSettings  ++
  versionSettings      ++
  depends.resolvers    ++
  specs2Settings       ++
  compilationSettings  ++
  testingSettings      ++
  publicationSettings  ++
  notificationSettings

def moduleJvmSettings(name: String) =
  testingJvmSettings

def moduleJsSettings(name: String) =
  commonJsSettings

/** MODULES (sorted in alphabetical order) */
lazy val analysis = crossProject(JSPlatform, JVMPlatform).in(file("analysis")).
  settings(Seq(
    libraryDependencies ++= depends.classycle ++ depends.compiler(scalaOrganization.value, scalaVersion.value)) ++
    moduleSettings("analysis") ++
    Seq(name := "specs2-analysis"):_*).
  jvmSettings(
    depends.jvmTest,
    moduleJvmSettings("analysis")).
  jsSettings(moduleJsSettings("analysis"))

lazy val analysisJs  = analysis.js.dependsOn(commonJs % "test->test", coreJs, matcherJs, scalacheckJs % "test")
lazy val analysisJvm = analysis.jvm.dependsOn(commonJvm % "test->test", coreJvm, matcherJvm, scalacheckJvm % "test")

lazy val common = crossProject(JSPlatform, JVMPlatform).in(file("common")).
  settings(
    libraryDependencies ++=
      depends.reflect(scalaOrganization.value, scalaVersion.value) ++
      depends.paradise(scalaVersion.value) ++
      depends.scalaXML(scalaVersion.value),
    moduleSettings("common")++
    Seq(name := "specs2-common")
).
  jsSettings(depends.jsTest, moduleJsSettings("common"),
    libraryDependencies ++= Seq(
      "org.scalacheck" %%% "scalacheck" % "1.13.5" % "test",
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.0.5"
    )
  ).
  jvmSettings(moduleJvmSettings("common"),
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.13.5" % "test",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"
    )
  )

lazy val commonJs  = common.js.dependsOn(fpJs)
lazy val commonJvm = common.jvm.dependsOn(fpJvm)

lazy val core = crossProject(JSPlatform, JVMPlatform).in(file("core")).
  settings(
    moduleSettings("core"),
    name := "specs2-core",
    libraryDependencies ++=
      depends.paradise(scalaVersion.value) ++
      depends.mockito.map(_ % "test") ++
      depends.junit.map(_ % "test")
    ).
  jsSettings(depends.jsTest, moduleJsSettings("analysis")).
  jvmSettings(
    depends.jvmTest,
    moduleJvmSettings("core"))

lazy val coreJs  = core.js.dependsOn(matcherJs, commonJs, commonJs % "test->test")
lazy val coreJvm = core.jvm.dependsOn(matcherJvm, commonJvm % "test->test")

lazy val examples = crossProject(JSPlatform, JVMPlatform).in(file("examples")).
  settings(moduleSettings("examples") ++
    Seq(name := "specs2-examples"):_*).
  jsSettings(depends.jsTest, moduleJsSettings("examples")).
  jvmSettings(depends.jvmTest, moduleJvmSettings("examples"))

lazy val examplesJs  = examples.js.dependsOn(commonJs, matcherJs, coreJs, matcherExtraJs, junitJs, scalacheckJs, mockJs)
lazy val examplesJvm = examples.jvm.dependsOn(commonJvm, matcherJvm, coreJvm, matcherExtraJvm, analysisJvm, form, gwt, html, markdown, junitJvm, scalacheckJvm, mockJvm)

lazy val fp = crossProject(JSPlatform, JVMPlatform).in(file("fp")).
  settings(moduleSettings("fp"):_*).
  settings(name := "specs2-fp").
  jsSettings(depends.jsTest, moduleJsSettings("fp")).
  jvmSettings(moduleJvmSettings("fp"))

lazy val fpJvm = fp.jvm
lazy val fpJs  = fp.js

lazy val form = project.in(file("form")).
  settings(moduleSettings("form") ++
    Seq(name := "specs2-form"):_*).
  settings(depends.jvmTest, moduleJvmSettings("form")).
  dependsOn(coreJvm, markdown, matcherExtraJvm, scalacheckJvm % "test->test")

lazy val guide = project.in(file("guide")).
  enablePlugins(BuildInfoPlugin).
  settings(moduleSettings("guide") ++ buildInfoSettings ++
    Seq(name := "specs2-guide",
      scalacOptions in Compile --= Seq("-Xlint", "-Ywarn-unused-import"))).
  dependsOn(examplesJvm % "compile->compile;test->test", scalazJvm, shapelessJvm)

lazy val gwt = project.in(file("gwt")).
  settings(Seq(
    libraryDependencies += "com.chuusai" %% "shapeless" % shapelessVersion) ++
    moduleSettings("gwt") ++
    Seq(name := "specs2-gwt"):_*).
  settings(depends.jvmTest, moduleJvmSettings("gwt")).
  dependsOn(coreJvm, matcherExtraJvm, scalacheckJvm)

lazy val html = project.in(file("html")).
  settings(
    Seq(libraryDependencies += depends.tagsoup) ++
      moduleSettings("html") ++
      Seq(name := "specs2-html"):_*).
  settings(depends.jvmTest, moduleJvmSettings("html")).
  dependsOn(form, mockJvm % "test", matcherExtraJvm % "test", scalacheckJvm % "test")

lazy val junit = crossProject(JSPlatform, JVMPlatform).in(file("junit")).
  settings(Seq(
    libraryDependencies ++= depends.junit ++ depends.mockito.map(_ % "test")) ++
    moduleSettings("junit") ++
    Seq(name := "specs2-junit"):_*).
  jvmSettings(depends.jvmTest, moduleJvmSettings("junit"))

lazy val junitJs = junit.js.dependsOn(coreJs, matcherExtraJs % "test", mockJs % "test")
lazy val junitJvm = junit.jvm.dependsOn(coreJvm, matcherExtraJvm % "test", mockJvm % "test")

lazy val markdown = project.in(file("markdown")).
  settings(Seq(
    libraryDependencies ++= depends.pegdown) ++
    moduleSettings("markdown") ++
    Seq(name := "specs2-markdown"):_*).
  settings(depends.jvmTest, moduleJvmSettings("markdown")).
  dependsOn(commonJvm, coreJvm % "compile->test")

lazy val matcher = crossProject(JSPlatform, JVMPlatform).in(file("matcher")).
  settings(moduleSettings("matcher") ++
    Seq(name := "specs2-matcher"):_*).
  jsSettings(depends.jsTest, moduleJsSettings("matcher")).
  jvmSettings(moduleJvmSettings("matcher"))

lazy val matcherJs  = matcher.js.dependsOn(commonJs)
lazy val matcherJvm = matcher.jvm.dependsOn(commonJvm)

lazy val matcherExtra = crossProject(JSPlatform, JVMPlatform).in(file("matcher-extra")).
  settings(moduleSettings("matcherextra") ++ Seq(
    name := "specs2-matcher-extra",
    libraryDependencies ++= depends.paradise(scalaVersion.value)
  ):_*).
  jsSettings(depends.jsTest, moduleJsSettings("matcher-extra"),
    libraryDependencies ++=
      Seq("org.scala-lang.modules" %%% "scala-parser-combinators" % "1.0.5")
  ).
  jvmSettings(depends.jvmTest, moduleJvmSettings("matcher-extra"))

lazy val matcherExtraJs  = matcherExtra.js.dependsOn(analysisJs, matcherJs, coreJs % "test->test")
lazy val matcherExtraJvm = matcherExtra.jvm.dependsOn(analysisJvm, matcherJvm, coreJvm % "test->test")

lazy val pom = Project(id = "pom", base = file("pom"),
  settings =
    moduleSettings("") ++ Seq(
      name := "specs2")
  ).dependsOn(commonJvm, matcherJvm, matcherExtraJvm, coreJvm, scalazJvm, html, analysisJvm,
    shapelessJvm, form, markdown, gwt, junitJvm, scalacheckJvm, mockJvm)

lazy val shapeless = crossProject(JSPlatform, JVMPlatform).in(file("shapeless")).
  settings(moduleSettings("shapeless") ++
    Seq(name := "specs2-shapeless",
      libraryDependencies ++=
        depends.paradise(scalaVersion.value)
    ):_*).
  jsSettings(depends.jsTest, moduleJsSettings("shapeless"), libraryDependencies +=
    "com.chuusai" %%% "shapeless" % shapelessVersion
  ).
  jvmSettings(depends.jvmTest, moduleJvmSettings("shapeless"), libraryDependencies +=
    "com.chuusai" %% "shapeless" % shapelessVersion
  )

lazy val shapelessJs = shapeless.js.dependsOn(matcherJs, matcherExtraJs % "test->test")
lazy val shapelessJvm = shapeless.jvm.dependsOn(matcherJvm, matcherExtraJvm % "test->test")

lazy val scalaz = crossProject(JSPlatform, JVMPlatform).in(file("scalaz")).
  settings(moduleSettings("scalaz") ++
    Seq(libraryDependencies ++=
      depends.scalaz(scalazVersion.value) ++
        depends.scalazConcurrent(scalazVersion.value)) ++
    Seq(name := "specs2-scalaz"):_*).
  jsSettings(depends.jsTest, moduleJsSettings("scalaz")).
  jvmSettings(depends.jvmTest, moduleJvmSettings("scalaz"))

lazy val scalazJs = scalaz.js.dependsOn(matcherJs, coreJs % "test->test")
lazy val scalazJvm = scalaz.jvm.dependsOn(matcherJvm, coreJvm % "test->test")

lazy val mock = crossProject(JSPlatform, JVMPlatform).in(file("mock")).
  settings(Seq(
    libraryDependencies ++=
      depends.hamcrest ++
        depends.mockito) ++
    moduleSettings("mock") ++
    Seq(name := "specs2-mock"):_*).
  jsSettings(depends.jsTest, moduleJsSettings("mock")).
  jvmSettings(depends.jvmTest, moduleJvmSettings("mock"))

lazy val mockJs = mock.js.dependsOn(coreJs)
lazy val mockJvm = mock.jvm.dependsOn(coreJvm)

lazy val scalacheck = crossProject(JSPlatform, JVMPlatform).in(file("scalacheck")).
  settings(
    moduleSettings("scalacheck") ++
    Seq(name := "specs2-scalacheck"):_*).
  jsSettings(depends.jsTest, moduleJsSettings("scalacheck"), libraryDependencies +=
    "org.scalacheck" %%% "scalacheck" % "1.13.5"
  ).
  jvmSettings(depends.jvmTest, moduleJvmSettings("scalacheck"), libraryDependencies +=
    "org.scalacheck" %% "scalacheck" % "1.13.5"
  )

lazy val scalacheckJs  = scalacheck.js.dependsOn(coreJs)
lazy val scalacheckJvm = scalacheck.jvm.dependsOn(coreJvm)

lazy val tests = Project(id = "tests", base = file("tests"),
  settings = moduleSettings("tests") ++
    Seq(name := "specs2-tests") ++
    Seq(libraryDependencies ++= depends.scalaParallelCollections(scalaVersion.value)) ++
    depends.jvmTest ++
    moduleJvmSettings("tests")
).dependsOn(
  coreJvm      % "compile->compile;test->test",
  shapelessJvm % "compile->compile;test->test",
  junitJvm     % "test->test",
  examplesJvm  % "test->test",
  matcherExtraJvm,
  html,
  scalazJvm)

lazy val specs2ShellPrompt = shellPrompt in ThisBuild := { state =>
  val name = Project.extract(state).currentRef.project
  (if (name == "specs2") "" else name) + "> "
}

def scalaSourceVersion(scalaBinaryVersion: String) =
  if (scalaBinaryVersion.startsWith("2.11"))
    "2.11"
  else
    "2.12"

lazy val compilationSettings = Seq(
  // https://gist.github.com/djspiewak/976cd8ac65e20e136f05
  unmanagedSourceDirectories in Compile ++=
    Seq((sourceDirectory in Compile).value / s"scala-${scalaSourceVersion(scalaBinaryVersion.value)}",
      (sourceDirectory in Compile).value / s"scala-scalaz-7.1.x",
      (sourceDirectory in (Test, test)).value / s"scala-scalaz-7.1.x"),
  maxErrors := 20,
  incOptions := incOptions.value.withNameHashing(true),
  scalacOptions in Compile ++=
      Seq("-Xfatal-warnings",
        "-Xlint",
        "-Ywarn-unused-import",
        "-Yno-adapted-args",
        "-Ywarn-numeric-widen",
        "-Ywarn-value-discard",
        "-deprecation:false", "-Xcheckinit", "-unchecked", "-feature", "-language:_"),
  scalacOptions += "-Ypartial-unification",
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.5"),
  scalacOptions in Test               ++= Seq("-Yrangepos"),
  scalacOptions in (Compile, doc)     ++= Seq("-feature", "-language:_"),
  scalacOptions in (Compile, console) := Seq("-Yrangepos", "-feature", "-language:_"),
  scalacOptions in (Test, console)    := Seq("-Yrangepos", "-feature", "-language:_")
)

lazy val testingSettings = Seq(
  initialCommands in console in test := "import org.specs2._",
  logBuffered := false,
  cancelable in Global := true,
  testFrameworks := Seq(TestFramework("org.specs2.runner.Specs2Framework")),
  testOptions := Seq(Tests.Filter(s =>
    (Seq(".guide.").exists(s.contains) || Seq("Spec", "Guide", "Website").exists(s.endsWith)) &&
      Seq("Specification", "FeaturesSpec").forall(n => !s.endsWith(n))))
)

lazy val testingJvmSettings =
  Seq(javaOptions ++= Seq("-Xmx3G", "-Xss4M"),
      fork in Test := true)

/**
 * DOCUMENTATION
 */
lazy val siteSettings = GhpagesPlugin.projectSettings ++ SitePlugin.projectSettings ++
  Seq(
    siteSourceDirectory := target.value / "specs2-reports" / "site",
    // copy the api files to a versioned directory
    siteMappings ++= { (mappings in packageDoc in Compile).value.map { case (f, d) => (f, s"api/SPECS2-${version.value}/$d") } },
    includeFilter in makeSite := AllPassFilter,
    // override the synchLocal task to avoid removing the existing files
    ghpagesSynchLocal := {
      val betterMappings = ghpagesPrivateMappings.value map { case (file, target) => (file, ghpagesUpdatedRepository.value / target) }
      IO.copy(betterMappings)
      ghpagesUpdatedRepository.value
    },
    git.remoteRepo := "git@github.com:etorreborre/specs2.git"
  )

lazy val apiSettings = Seq(
  sources                      in (Compile, doc) := sources.all(aggregateCompile).value.flatten,
  unmanagedSources             in (Compile, doc) := unmanagedSources.all(aggregateCompile).value.flatten,
  unmanagedSourceDirectories   in (Compile, doc) := unmanagedSourceDirectories.all(aggregateCompile).value.flatten,
  unmanagedResourceDirectories in (Compile, doc) := unmanagedResourceDirectories.all(aggregateCompile).value.flatten,
  libraryDependencies                            := libraryDependencies.all(aggregateTest).value.flatten.map(maybeMarkProvided)) ++
  Seq(scalacOptions in (Compile, doc) += "-Ymacro-no-expand")

lazy val aggregateCompile = ScopeFilter(
  inProjects(fpJvm, commonJvm, matcherJvm, matcherExtraJvm, coreJvm, html, analysisJvm, form, shapelessJvm, markdown, gwt, junitJvm, scalacheckJvm, mockJvm),
  inConfigurations(Compile))

lazy val aggregateTest = ScopeFilter(
  inProjects(fpJvm, commonJvm, matcherJvm, matcherExtraJvm, coreJvm, html, analysisJvm, form, shapelessJvm, markdown, gwt, junitJvm, scalacheckJvm, mockJvm),
  inConfigurations(Test))

def maybeMarkProvided(dep: ModuleID): ModuleID =
  if (providedDependenciesInAggregate.exists(dep.name.startsWith)) dep.copy(configurations = Some("provided"))
  else dep

/* A list of dependency module names that should be marked as "provided" for the aggregate artifact */
lazy val providedDependenciesInAggregate = Seq("shapeless")


/**
 * PUBLICATION
 */
lazy val publishSignedArtifacts = executeAggregateTask(publishSigned, "Publishing signed artifacts")

lazy val publicationSettings = Seq(
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
  Sonatype.projectSettings

/**
 * NOTIFICATION
 */
lazy val notificationSettings = Seq(
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
 * UTILITIES
 */
def executeAggregateTask(task: TaskKey[_], info: String) = (st: State) => {
  st.log.info(info)
  val extracted = Project.extract(st)
  val ref: ProjectRef = extracted.get(thisProjectRef)
  extracted.runAggregated(task in ref, st)
}

def executeTask(task: TaskKey[_], info: String) = (st: State) => {
  st.log.info(info)
  val extracted = Project.extract(st)
  val ref: ProjectRef = extracted.get(thisProjectRef)
  extracted.runTask(task in ref, st)._1
}

def executeTask(task: TaskKey[_], info: String, configuration: Configuration) = (st: State) => {
  st.log.info(info)
  val extracted = Project.extract(st)
  val ref: ProjectRef = extracted.get(thisProjectRef)
  extracted.runTask(task in configuration, st)._1
}
