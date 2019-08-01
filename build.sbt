import sbt._
import Defaults._
import com.typesafe.sbt.pgp.PgpKeys._
import java.util.{Date, TimeZone}
import java.text.SimpleDateFormat
// shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

/** MAIN PROJECT */
lazy val specs2 = project.in(file(".")).
  enablePlugins(GitBranchPrompt, SitePlugin, GhpagesPlugin).
  settings(
    commonSettings,
    siteSettings,
    apiSettings,
    buildInfoSettings,
    name := "specs2",
    packagedArtifacts := Map.empty
  ).aggregate(
    fpJvm, catsJvm, commonJvm, matcherJvm, coreJvm, matcherExtraJvm, scalazJvm, html,
    analysisJvm, shapelessJvm, form, markdown, gwt, junitJvm, scalacheckJvm, mockJvm,
    tests, fpJs, commonJs, matcherJs, coreJs, matcherExtraJs, scalazJs, analysisJs,
    shapelessJs, junitJs, scalacheckJs, mockJs
  )

val scala211 = "2.11.12"

/** COMMON SETTINGS */
lazy val specs2Settings = Seq(
  organization := "org.specs2",
  specs2Version in GlobalScope := version.value,
  scalazVersion in GlobalScope := "7.2.27",
  specs2ShellPrompt,
  scalaVersion := "2.12.8",
  crossScalaVersions := Seq(scalaVersion.value, scala211, "2.13.0"))

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
      Seq(BuildInfoKey.action("commit")(sys.process.Process(s"git log --pretty=format:%h -n  1").lineStream.head),
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
    val tag = tagName.value
    val tagOrHash =
      if(isSnapshot.value) sys.process.Process("git rev-parse HEAD").lineStream_!.head
      else tag
    val a = (baseDirectory in LocalRootProject).value.toURI.toString
    val g = "https://raw.githubusercontent.com/etorreborre/specs2/" + tagOrHash
    s"-P:scalajs:mapSourceURI:$a->$g/"
  },
  parallelExecution in Test := false
)

lazy val commonNativeSettings = Seq(
  scalaVersion := scala211,
  crossScalaVersions := Seq(scala211),
  nativeLinkStubs := true
)

lazy val commonJsNativeSettings = Seq(
  Compile / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "js-native" / "src" / "main" / "scala"
)


lazy val specs2Version = settingKey[String]("defines the current specs2 version")
lazy val scalazVersion = settingKey[String]("defines the current scalaz version")
lazy val shapelessVersion = "2.3.3"
lazy val catsVersion = "1.6.1"
lazy val catsEffectVersion = "1.4.0"

val commonSettings =
  coreDefaultSettings  ++
    versionSettings      ++
    depends.resolvers    ++
    specs2Settings       ++
    compilationSettings  ++
    testingSettings      ++
    publicationSettings  ++
    notificationSettings

def commonJvmSettings =
  testingJvmSettings

/** MODULES (sorted in alphabetical order) */
lazy val analysis = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("analysis")).
  settings(
    libraryDependencies ++= depends.classycle ++ depends.compiler(scalaOrganization.value, scalaVersion.value),
    commonSettings,
    name := "specs2-analysis").
  jvmSettings(
    depends.jvmTest,
    commonJvmSettings).
  jsSettings(commonJsSettings).
  nativeSettings(commonNativeSettings)

lazy val analysisJs  = analysis.js.dependsOn(commonJs % "test->test", coreJs, matcherJs, scalacheckJs % Test)
lazy val analysisJvm = analysis.jvm.dependsOn(commonJvm % "test->test", coreJvm, matcherJvm, scalacheckJvm % Test)
lazy val analysisNative = analysis.native.dependsOn(commonNative % "test->test", coreNative, matcherNative, scalacheckNative % Test)

lazy val cats = crossProject(JSPlatform, JVMPlatform).in(file("cats")).
  settings(
    commonSettings,
    libraryDependencies ++=
      (if (scalaVersion.value == "2.13.0")
          Seq("org.typelevel" % "cats-core_2.13.0-M5" % "1.6.0",
              "org.typelevel" % "cats-effect_2.13.0-M5" % "1.2.0")
        else
          Seq("org.typelevel" %% "cats-core" % catsVersion,
              "org.typelevel" %% "cats-effect" % catsEffectVersion)
      ),
    name := "specs2-cats"
  ).
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(depends.jvmTest, commonJvmSettings)

lazy val catsJs = cats.js.dependsOn(matcherJs, coreJs % "test->test")
lazy val catsJvm = cats.jvm.dependsOn(matcherJvm, coreJvm % "test->test")

lazy val common = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("common")).
  settings(
    libraryDependencies ++=
      depends.paradise(scalaVersion.value) ++
      Seq(depends.reflect(scalaOrganization.value, scalaVersion.value),
        depends.scalaXML),
    commonSettings,
    name := "specs2-common"
  ).
  platformsSettings(JVMPlatform, JSPlatform)(
    libraryDependencies ++= depends.scalaParser.value,
    libraryDependencies +=
      "org.scalacheck" %%% "scalacheck" % "1.14.0" % Test
  ).
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(commonJvmSettings).
  nativeSettings(
    commonNativeSettings,
    depends.nativeTest,
    libraryDependencies ++= depends.scalaParserNative.value,
    libraryDependencies +=
      "com.github.lolgab" %%% "scalacheck" % "1.14.1" % Test
  ).
  platformsSettings(JSPlatform, NativePlatform)(
    commonJsNativeSettings
  )

lazy val commonJs  = common.js.dependsOn(fpJs)
lazy val commonJvm = common.jvm.dependsOn(fpJvm)
lazy val commonNative = common.native.dependsOn(fpNative)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("core")).
  settings(
    commonSettings,
    name := "specs2-core",
    libraryDependencies ++=
      depends.paradise(scalaVersion.value) ++ 
      Seq(
        depends.mockito % Test,
        depends.junit % Test)
  ).
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(
    depends.jvmTest,
    commonJvmSettings).
  nativeSettings(commonNativeSettings).
  platformsSettings(JSPlatform, NativePlatform)(
    commonJsNativeSettings
  )

lazy val coreJs     = core.js.dependsOn(matcherJs, commonJs, commonJs % "test->test")
lazy val coreJvm    = core.jvm.dependsOn(matcherJvm, commonJvm % "test->test")
lazy val coreNative = core.native.dependsOn(matcherNative, commonNative % "test->test")

lazy val examples = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("examples")).
  settings(
    commonSettings,
    name := "specs2-examples").
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  nativeSettings(commonNativeSettings)

lazy val examplesJs  = examples.js.dependsOn(commonJs, matcherJs, coreJs, matcherExtraJs, junitJs, scalacheckJs, mockJs)
lazy val examplesJvm = examples.jvm.dependsOn(commonJvm, matcherJvm, coreJvm, matcherExtraJvm, analysisJvm, form, gwt, html, markdown, junitJvm, scalacheckJvm, mockJvm)
lazy val examplesNative = examples.native.dependsOn(commonNative, matcherNative, coreNative, matcherExtraNative, analysisNative, form, gwt, html, markdown, junitNative, scalacheckNative, mockNative)

lazy val fp = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("fp")).
  settings(commonSettings:_*).
  settings(name := "specs2-fp").
  jvmSettings(commonJvmSettings).
  jsSettings(depends.jsTest, commonJsSettings).
  nativeSettings(depends.nativeTest, commonNativeSettings)

lazy val fpJvm     = fp.jvm
lazy val fpJs      = fp.js
lazy val fpNative  = fp.native

lazy val form = project.in(file("form")).
  settings(
    commonSettings,
    name := "specs2-form").
  settings(depends.jvmTest, commonJvmSettings).
  dependsOn(coreJvm, markdown, matcherExtraJvm, scalacheckJvm % "test->test")

lazy val guide = project.in(file("guide")).
  enablePlugins(BuildInfoPlugin).
  settings(
    commonSettings,
    buildInfoSettings,
    name := "specs2-guide",
    scalacOptions in Compile --= Seq("-Xlint", "-Ywarn-unused-import")).
  dependsOn(examplesJvm % "compile->compile;test->test", scalazJvm, shapelessJvm)

lazy val gwt = project.in(file("gwt")).
  settings(
    libraryDependencies += "com.chuusai" %% "shapeless" % shapelessVersion,
    commonSettings,
    name := "specs2-gwt").
  settings(depends.jvmTest, commonJvmSettings).
  dependsOn(coreJvm, matcherExtraJvm, scalacheckJvm)

lazy val html = project.in(file("html")).
  settings(
    libraryDependencies += depends.tagsoup,
    commonSettings,
    name := "specs2-html").
  settings(depends.jvmTest, commonJvmSettings).
  dependsOn(form, mockJvm % Test, matcherExtraJvm % Test, scalacheckJvm % Test)

lazy val junit = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("junit")).
  settings(
    libraryDependencies ++= Seq(
      depends.junit,
      depends.mockito % Test),
    commonSettings,
    name := "specs2-junit").
  jvmSettings(depends.jvmTest, commonJvmSettings).
  nativeSettings(depends.nativeTest, commonNativeSettings)

lazy val junitJs = junit.js.dependsOn(coreJs, matcherExtraJs % Test, mockJs % Test)
lazy val junitJvm = junit.jvm.dependsOn(coreJvm, matcherExtraJvm % Test, mockJvm % Test)
lazy val junitNative = junit.native.dependsOn(coreNative, matcherExtraNative % Test, mockNative % Test)

lazy val markdown = project.in(file("markdown")).
  settings(
    libraryDependencies += depends.pegdown,
    commonSettings,
    name := "specs2-markdown").
  settings(depends.jvmTest, commonJvmSettings).
  dependsOn(commonJvm, coreJvm % "compile->test")

lazy val matcher = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("matcher")).
  settings(
    commonSettings,
    name := "specs2-matcher").
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(commonJvmSettings).
  nativeSettings(depends.nativeTest, commonNativeSettings)

lazy val matcherJs     = matcher.js.dependsOn(commonJs)
lazy val matcherJvm    = matcher.jvm.dependsOn(commonJvm)
lazy val matcherNative = matcher.native.dependsOn(commonNative)

lazy val matcherExtra = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("matcher-extra")).
  settings(commonSettings ++ Seq(
    name := "specs2-matcher-extra",
    libraryDependencies ++= depends.paradise(scalaVersion.value)
  ):_*).
  jsSettings(depends.jsTest, commonJsSettings
  ).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  nativeSettings(depends.nativeTest, commonNativeSettings)

lazy val matcherExtraJs     = matcherExtra.js.dependsOn(analysisJs, matcherJs, coreJs % "test->test")
lazy val matcherExtraJvm    = matcherExtra.jvm.dependsOn(analysisJvm, matcherJvm, coreJvm % "test->test")
lazy val matcherExtraNative = matcherExtra.native.dependsOn(analysisNative, matcherNative, coreNative % "test->test")

lazy val pom = Project(id = "pom", base = file("pom")).
  settings(commonSettings).
  dependsOn(catsJvm, commonJvm, matcherJvm, matcherExtraJvm, coreJvm, scalazJvm, html, analysisJvm,
    shapelessJvm, form, markdown, gwt, junitJvm, scalacheckJvm, mockJvm)

lazy val shapeless = crossProject(JSPlatform, JVMPlatform, NativePlatform).
  crossType(CrossType.Pure).
  in(file("shapeless")).
  settings(
    commonSettings,
    name := "specs2-shapeless",
    libraryDependencies ++= depends.paradise(scalaVersion.value),
    libraryDependencies += "com.chuusai" %%% "shapeless" % shapelessVersion
  ).
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  nativeSettings(depends.nativeTest, commonNativeSettings)

lazy val shapelessJs = shapeless.js.dependsOn(matcherJs, matcherExtraJs % "test->test")
lazy val shapelessJvm = shapeless.jvm.dependsOn(matcherJvm, matcherExtraJvm % "test->test")
lazy val shapelessNative = shapeless.native.dependsOn(matcherNative, matcherExtraNative % "test->test")

lazy val scalaz = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("scalaz")).
  settings(
    commonSettings,
    libraryDependencies ++=
      depends.scalaz(scalazVersion.value) :+
      depends.scalazConcurrent(scalazVersion.value),
    name := "specs2-scalaz").
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  nativeSettings(depends.nativeTest, commonNativeSettings)

lazy val scalazJs = scalaz.js.dependsOn(matcherJs, coreJs % "test->test")
lazy val scalazJvm = scalaz.jvm.dependsOn(matcherJvm, coreJvm % "test->test")
lazy val scalazNative = scalaz.native.dependsOn(matcherNative, coreNative % "test->test")

lazy val mock = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("mock")).
  settings(
    libraryDependencies ++= Seq(
      depends.hamcrest,
      depends.mockito),
    commonSettings,
    name := "specs2-mock").
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  nativeSettings(depends.nativeTest, commonNativeSettings).
  platformsSettings(JSPlatform, NativePlatform)(
    commonJsNativeSettings
  )

lazy val mockJs = mock.js.dependsOn(coreJs)
lazy val mockJvm = mock.jvm.dependsOn(coreJvm)
lazy val mockNative = mock.native.dependsOn(coreNative)

lazy val scalacheck = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalacheck")).
  settings(
    commonSettings,
    name := "specs2-scalacheck"
  ).
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  platformsSettings(JVMPlatform, JSPlatform)(
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.14.0"
  ).
  nativeSettings(depends.nativeTest, commonNativeSettings,
    libraryDependencies += "com.github.lolgab" %%% "scalacheck" % "1.14.1"
  ).
  platformsSettings(JSPlatform, NativePlatform)(
    commonJsNativeSettings
  )

lazy val scalacheckJs  = scalacheck.js.dependsOn(coreJs)
lazy val scalacheckJvm = scalacheck.jvm.dependsOn(coreJvm)
lazy val scalacheckNative = scalacheck.native.dependsOn(coreNative)

lazy val tests = Project(id = "tests", base = file("tests")).
  settings(
    commonSettings,
    name := "specs2-tests",
    depends.jvmTest,
    commonJvmSettings
  ).dependsOn(
  coreJvm      % "compile->compile;test->test",
  shapelessJvm % "compile->compile;test->test",
  junitJvm     % "test->test",
  examplesJvm  % "test->test",
  matcherExtraJvm,
  html,
  scalazJvm,
  catsJvm)

lazy val specs2ShellPrompt = shellPrompt in ThisBuild := { state =>
  val name = Project.extract(state).currentRef.project
  (if (name == "specs2") "" else name) + "> "
}

def scalaSourceVersion(scalaBinaryVersion: String) =
  scalaBinaryVersion.split('.').take(2).mkString(".")

lazy val compilationSettings = Seq(
  // https://gist.github.com/djspiewak/976cd8ac65e20e136f05
  unmanagedSourceDirectories in Compile ++=
    Seq((sourceDirectory in Compile).value / s"scala-${scalaSourceVersion(scalaBinaryVersion.value)}",
      (sourceDirectory in Compile).value / s"scala-scalaz-7.1.x",
      (sourceDirectory in (Test, test)).value / s"scala-scalaz-7.1.x"),
  maxErrors := 20,
  scalacOptions in Compile ++=
    Seq(
      //"-Xfatal-warnings",
      "-Xlint",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-deprecation:false", "-Xcheckinit", "-unchecked", "-feature", "-language:_"),
  scalacOptions in Compile ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v <= 12 =>
        Seq(
          "-Ywarn-unused-import",
          "-Yno-adapted-args"
        )
      case _ =>
        Nil
    }
  },
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v <= 12 =>
        Seq(
          "-Ypartial-unification"
        )
      case _ =>
        Nil
    }
  },
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
  scalacOptions in Test               += "-Yrangepos",
  scalacOptions in (Compile, doc)    ++= Seq("-feature", "-language:_"),
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

lazy val testingJvmSettings = Seq(
  javaOptions ++= Seq("-Xmx3G", "-Xss4M"),
  fork in Test := true
)

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
  if (providedDependenciesInAggregate.exists(dep.name.startsWith)) dep.withConfigurations(configurations = Some("provided"))
  else dep

/* A list of dependency module names that should be marked as "provided" for the aggregate artifact */
lazy val providedDependenciesInAggregate = Seq("shapeless")


/**
 * PUBLICATION
 */
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
    val notesFilePath = s"notes/${tagName.toUpperCase.replace("SPECS2-", "")}.markdown"
    try scala.io.Source.fromFile(notesFilePath).mkString
    catch { case t: Throwable => throw new Exception(s"the path $notesFilePath not found for tag $tagName") }
  },
  // just upload the notes
  ghreleaseAssets := Seq()
)
