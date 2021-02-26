import sbt._
import Defaults._
import java.util.{Date, TimeZone}
import java.text.SimpleDateFormat
// shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

enablePlugins(ScalaJSPlugin)

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
    fpJVM, catsJVM, commonJVM, matcherJVM, coreJVM, matcherExtraJVM, scalazJVM, html,
    analysisJVM, shapelessJVM, formJVM, markdownJVM, gwtJVM, junitJVM, scalacheckJVM, mockJVM,
    tests, fpJS, catsJS, commonJS, matcherJS, coreJS, matcherExtraJS, scalazJS, analysisJS,
    shapelessJS, formJS, markdownJS, gwtJS,
    junitJS, scalacheckJS, mockJS
  )

val scala211 = "2.13.5"

/** COMMON SETTINGS */
lazy val specs2Settings = Seq(
  organization := "org.specs2",
  specs2Version in GlobalScope := version.value,
  scalazVersion in GlobalScope := "7.2.31",
  specs2ShellPrompt,
  scalaVersion := "2.13.2",
  SettingKey[Boolean]("ide-skip-project") := platformDepsCrossVersion.value == ScalaNativeCrossVersion.binary,
  crossScalaVersions := Seq(scalaVersion.value, scala211, "2.12.11"))

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
lazy val catsVersion = "2.0.0"
lazy val catsEffectVersion = "2.0.0"

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
  nativeSettings(commonNativeSettings).
  dependsOn(common % "test->test", core, matcher, scalacheck % Test)

lazy val analysisJVM = analysis.jvm
lazy val analysisJS = analysis.js
lazy val analysisNative = analysis.native

lazy val cats = crossProject(JSPlatform, JVMPlatform).in(file("cats")).
  settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-effect" % catsEffectVersion
    ),
    name := "specs2-cats"
  ).
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  dependsOn(matcher, core % "test->test")

lazy val catsJS = cats.js
lazy val catsJVM = cats.jvm

lazy val common = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("common")).
  settings(
    libraryDependencies ++=
      depends.paradise(scalaVersion.value) ++
      Seq(depends.reflect(scalaOrganization.value, scalaVersion.value),
        depends.scalaXML, depends.scalacheck.value % Test),
    commonSettings,
    name := "specs2-common"
  ).
  platformsSettings(JVMPlatform, JSPlatform)(
    libraryDependencies ++= depends.scalaParser.value,
  ).
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  nativeSettings(
    commonNativeSettings,
    depends.nativeTest,
    libraryDependencies ++= depends.scalaParserNative.value,
  ).
  platformsSettings(JSPlatform, NativePlatform)(
    commonJsNativeSettings
  ).
  dependsOn(fp)

lazy val commonJS = common.js
lazy val commonJVM = common.jvm
lazy val commonNative = common.native

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
  ).
  dependsOn(matcher, common, common % "test->test")

lazy val coreJVM = core.jvm
lazy val coreJS = core.js
lazy val coreNative = core.native

lazy val examples = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("examples")).
  settings(
    commonSettings,
    name := "specs2-examples").
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  nativeSettings(commonNativeSettings).
  dependsOn(common, matcher, core, matcherExtra, junit, scalacheck, mock)

lazy val examplesJVM = examples.jvm.dependsOn(analysisJVM, formJVM, gwtJVM, html, markdownJVM)
lazy val examplesJS = examples.js
lazy val examplesNative = examples.native

lazy val fp = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("fp")).
  settings(commonSettings:_*).
  settings(name := "specs2-fp").
  jvmSettings(commonJvmSettings).
  jsSettings(depends.jsTest, commonJsSettings).
  nativeSettings(depends.nativeTest, commonNativeSettings)

lazy val fpJVM = fp.jvm
lazy val fpJS = fp.js
lazy val fpNative = fp.native

lazy val form = crossProject(JSPlatform, JVMPlatform, NativePlatform).
  crossType(CrossType.Pure).
  in(file("form")).
  settings(
    commonSettings,
    name := "specs2-form").
  jvmSettings(depends.jvmTest, commonJvmSettings).
  jsSettings(depends.jsTest, commonJsSettings).
  nativeSettings(depends.nativeTest, commonNativeSettings).
  dependsOn(core, markdown, matcherExtra, scalacheck % "test->test")

lazy val formJVM = form.jvm
lazy val formJS = form.js
lazy val formNative = form.native

lazy val guide = project.in(file("guide")).
  enablePlugins(BuildInfoPlugin).
  settings(
    commonSettings,
    buildInfoSettings,
    name := "specs2-guide",
    scalacOptions in Compile --= Seq("-Xlint", "-Ywarn-unused-import")).
  dependsOn(examplesJVM % "compile->compile;test->test", scalazJVM, shapelessJVM)

lazy val gwt = crossProject(JSPlatform, JVMPlatform, NativePlatform).
  crossType(CrossType.Pure).
  in(file("gwt")).
  settings(
    commonSettings,
    libraryDependencies += "com.chuusai" %%% "shapeless" % shapelessVersion,
    name := "specs2-gwt").
  jvmSettings(depends.jvmTest, commonJvmSettings).
  jsSettings(depends.jsTest, commonJsSettings).
  nativeSettings(depends.nativeTest, commonNativeSettings).
  dependsOn(core, matcherExtra, scalacheck)

lazy val gwtJVM = gwt.jvm
lazy val gwtJS = gwt.js
lazy val gwtNative = gwt.native

lazy val html = project.in(file("html")).
  settings(
    libraryDependencies += depends.tagsoup,
    commonSettings,
    name := "specs2-html").
  settings(depends.jvmTest, commonJvmSettings).
  dependsOn(formJVM, mockJVM % Test, matcherExtraJVM % Test, scalacheckJVM % Test)

lazy val junit = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("junit")).
  settings(
    libraryDependencies ++= Seq(
      depends.junit,
      depends.mockito % Test),
    commonSettings,
    name := "specs2-junit").
  jvmSettings(depends.jvmTest, commonJvmSettings).
  nativeSettings(depends.nativeTest, commonNativeSettings).
  dependsOn(core, matcherExtra % Test, mock % Test)

lazy val junitJVM = junit.jvm
lazy val junitJS = junit.js
lazy val junitNative = junit.native

lazy val markdown = crossProject(JSPlatform, JVMPlatform, NativePlatform).
  crossType(CrossType.Pure).
  in(file("markdown")).
  settings(
    libraryDependencies += depends.pegdown,
    commonSettings,
    name := "specs2-markdown").
  jvmSettings(depends.jvmTest, commonJvmSettings).
  jsSettings(depends.jsTest, commonJsSettings).
  nativeSettings(depends.nativeTest, commonNativeSettings).
  dependsOn(common, core % "compile->test")

lazy val markdownJVM = markdown.jvm
lazy val markdownJS = markdown.js
lazy val markdownNative = markdown.native

lazy val matcher = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("matcher")).
  settings(
    commonSettings,
    name := "specs2-matcher").
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(commonJvmSettings).
  nativeSettings(depends.nativeTest, commonNativeSettings).
  dependsOn(common)

lazy val matcherJS = matcher.js
lazy val matcherJVM = matcher.jvm
lazy val matcherNative = matcher.native

lazy val matcherExtra = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("matcher-extra")).
  settings(
    commonSettings,
    name := "specs2-matcher-extra",
    libraryDependencies ++= depends.paradise(scalaVersion.value)
  ).
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  nativeSettings(depends.nativeTest, commonNativeSettings).
  dependsOn(analysis, matcher, core % "test->test")

lazy val matcherExtraJS = matcherExtra.js
lazy val matcherExtraJVM = matcherExtra.jvm
lazy val matcherExtraNative = matcherExtra.native

lazy val pom = Project(id = "pom", base = file("pom")).
  settings(commonSettings).
  dependsOn(catsJVM, commonJVM, matcherJVM, matcherExtraJVM, coreJVM, scalazJVM, html, analysisJVM,
    shapelessJVM, formJVM, markdownJVM, gwtJVM, junitJVM, scalacheckJVM, mockJVM)

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
  nativeSettings(depends.nativeTest, commonNativeSettings).
  dependsOn(matcher, matcherExtra % "test->test")

lazy val shapelessJS = shapeless.js
lazy val shapelessJVM = shapeless.jvm
lazy val shapelessNative = shapeless.native

lazy val scalaz = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("scalaz")).
  settings(
    commonSettings,
    libraryDependencies ++=
      depends.scalaz(scalazVersion.value) :+
      depends.scalazConcurrent(scalazVersion.value),
    name := "specs2-scalaz").
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  nativeSettings(depends.nativeTest, commonNativeSettings).
  dependsOn(matcher, core % "test->test")

lazy val scalazJS = scalaz.js
lazy val scalazJVM = scalaz.jvm
lazy val scalazNative = scalaz.native

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
  platformsSettings(JSPlatform, NativePlatform)(commonJsNativeSettings).
  dependsOn(core)

lazy val mockJS = mock.js
lazy val mockJVM = mock.jvm
lazy val mockNative = mock.native

lazy val scalacheck = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalacheck")).
  settings(
    commonSettings,
    name := "specs2-scalacheck",
    libraryDependencies += depends.scalacheck.value
  ).
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  platformsSettings(JSPlatform, NativePlatform)(
    commonJsNativeSettings
  ).
  dependsOn(core)

lazy val scalacheckJS  = scalacheck.js
lazy val scalacheckJVM = scalacheck.jvm
lazy val scalacheckNative = scalacheck.native

lazy val tests = Project(id = "tests", base = file("tests")).
  settings(
    commonSettings,
    name := "specs2-tests",
    depends.jvmTest,
    commonJvmSettings
  ).dependsOn(
  coreJVM      % "compile->compile;test->test",
  shapelessJVM % "compile->compile;test->test",
  junitJVM     % "test->test",
  examplesJVM  % "test->test",
  matcherExtraJVM,
  html,
  scalazJVM,
  catsJVM)

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
      "-Xlint:-byname-implicit",
      "-Xlint:-nullary-override",
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
  Seq(scalacOptions in (Compile, doc) += "-Ymacro-expand:none")

lazy val aggregateCompile = ScopeFilter(
  inProjects(fpJVM, commonJVM, matcherJVM, matcherExtraJVM, coreJVM, html, analysisJVM, formJVM, shapelessJVM, markdownJVM, gwtJVM, junitJVM, scalacheckJVM, mockJVM),
  inConfigurations(Compile))

lazy val aggregateTest = ScopeFilter(
  inProjects(fpJVM, commonJVM, matcherJVM, matcherExtraJVM, coreJVM, html, analysisJVM, formJVM, shapelessJVM, markdownJVM, gwtJVM, junitJVM, scalacheckJVM, mockJVM),
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
  publishTo in Global := sonatypePublishToBundle.value,
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
 lazy val notificationSettings = Seq()
//   ghreleaseRepoOrg := "etorreborre",
//   ghreleaseRepoName := "specs2",
//   ghreleaseNotes := { tagName: TagName =>
//     // find the corresponding release notes
//     val notesFilePath = s"notes/${tagName.toUpperCase.replace("SPECS2-", "")}.markdown"
//     try scala.io.Source.fromFile(notesFilePath).mkString
//     catch { case t: Throwable => throw new Exception(s"the path $notesFilePath not found for tag $tagName") }
//   },
//   // just upload the notes
//   ghreleaseAssets := Seq()
// )
