import sbt._
import Defaults._
import java.util.{Date, TimeZone}
import java.text.SimpleDateFormat
import libraryDependencies._

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
    fpJVM, commonJVM, matcherJVM, coreJVM, matcherExtraJVM, html,
    formJVM, markdownJVM, junitJVM, scalacheckJVM,
    tests
  )

val scala211 = "2.11.12"
val scalaDotty = "0.26.0-RC1"

/** COMMON SETTINGS */
lazy val specs2Settings = Seq(
  organization := "org.specs2",
  specs2Version in GlobalScope := version.value,
  specs2ShellPrompt,
  scalaVersion := scalaDotty//,
  //crossScalaVersions := Seq(scalaVersion.value, scala211, "2.12.10", scalaDotty)
  )

lazy val buildInfoSettings = Seq(
  buildInfoKeys :=
    Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion) ++
      Seq(BuildInfoKey.action("commit")(sys.process.Process(s"git log --pretty=format:%h -n  1").lineStream.head),
        BuildInfoKey.action("timestamp")(timestamp(new Date))),
  buildInfoPackage := "org.specs2"
)

lazy val commonJsSettings = Seq(
  scalacOptions += {
    val tag = s"SPECS2-${version.value}"
    val tagOrHash =
      if(isSnapshot.value) sys.process.Process("git rev-parse HEAD").lineStream_!.head
      else tag
    val a = (baseDirectory in LocalRootProject).value.toURI.toString
    val g = "https://raw.githubusercontent.com/etorreborre/specs2/" + tagOrHash
    s"-P:scalajs:mapSourceURI:$a->$g/"
  },
  parallelExecution in Test := false
)

// lazy val commonNativeSettings = Seq(
//   scalaVersion := scala211,
//   crossScalaVersions := Seq(scala211),
//   nativeLinkStubs := true
// )

// lazy val commonJsNativeSettings = Seq(
//   Compile / unmanagedSourceDirectories += baseDirectory.value.getParentFile / "js-native" / "src" / "main" / "scala"
// )

lazy val specs2Version = settingKey[String]("defines the current specs2 version")

val commonSettings =
    coreDefaultSettings  ++
    depends.resolvers    ++
    specs2Settings       ++
    compilationSettings  ++
    testingSettings      ++
    publicationSettings

def commonJvmSettings =
  testingJvmSettings

/** MODULES (sorted in alphabetical order) */

lazy val common = crossProject(JVMPlatform).in(file("common")).
  settings(
    depends.reflect,
    depends.scalaXML,
    depends.scalacheckTest,
    commonSettings,
    name := "specs2-common"
  ).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  // jsSettings(depends.jsTest, commonJsSettings).
  // nativeSettings(
  //   commonNativeSettings,
  //   depends.nativeTest,
  //   depends.scalaParserNative,
  // ).
  platformsSettings(JVMPlatform)(depends.scalaParser).
  // platformsSettings(JVMPlatform, JSPlatform)(depends.scalaParser).
  // platformsSettings(JSPlatform, NativePlatform)(commonJsNativeSettings).
  dependsOn(fp)

// lazy val commonJS = common.js
lazy val commonJVM = common.jvm
// lazy val commonNative = common.native

lazy val core = crossProject(JVMPlatform).in(file("core")).
  settings(
    commonSettings,
    name := "specs2-core",
    libraryDependencies += depends.junit % Test
  ).
  // jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  // nativeSettings(commonNativeSettings).
  // platformsSettings(JSPlatform, NativePlatform)(commonJsNativeSettings).
  dependsOn(matcher, common, common % "test->test")

lazy val coreJVM = core.jvm
// lazy val coreJS = core.js
// lazy val coreNative = core.native

lazy val examples = crossProject(JVMPlatform).in(file("examples")).
  settings(
    commonSettings,
    name := "specs2-examples").
  jvmSettings(depends.jvmTest, commonJvmSettings).
  // jsSettings(depends.jsTest, commonJsSettings).
  // nativeSettings(commonNativeSettings).
  dependsOn(common, matcher, core, matcherExtra, junit, scalacheck)

lazy val examplesJVM = examples.jvm.dependsOn(formJVM, html, markdownJVM)
// lazy val examplesJS = examples.js
// lazy val examplesNative = examples.native

lazy val fp = crossProject(JVMPlatform).in(file("fp")).
  settings(commonSettings:_*).
  settings(name := "specs2-fp").
  jvmSettings(commonJvmSettings)//.
  // jsSettings(depends.jsTest, commonJsSettings).
  // nativeSettings(depends.nativeTest, commonNativeSettings)

lazy val fpJVM = fp.jvm
// lazy val fpJS = fp.js
// lazy val fpNative = fp.native

lazy val form = crossProject(JVMPlatform).
  crossType(CrossType.Pure).
  in(file("form")).
  settings(
    commonSettings,
    name := "specs2-form").
  jvmSettings(depends.jvmTest, commonJvmSettings).
  // jsSettings(depends.jsTest, commonJsSettings).
  // nativeSettings(depends.nativeTest, commonNativeSettings).
  dependsOn(core, markdown, matcherExtra, scalacheck % "test->test")

lazy val formJVM = form.jvm
// lazy val formJS = form.js
// lazy val formNative = form.native

lazy val guide = project.in(file("guide")).
  enablePlugins(BuildInfoPlugin).
  settings(
    commonSettings,
    buildInfoSettings,
    name := "specs2-guide",
    scalacOptions in Compile --= Seq("-Xlint", "-Ywarn-unused-import")).
  dependsOn(examplesJVM % "compile->compile;test->test")

lazy val html = project.in(file("html")).
  settings(
    libraryDependencies += depends.tagsoup,
    commonSettings,
    name := "specs2-html").
  settings(depends.jvmTest, commonJvmSettings).
  dependsOn(formJVM, matcherExtraJVM % Test, scalacheckJVM % Test)

lazy val junit = crossProject(JVMPlatform).in(file("junit")).
  settings(
    libraryDependencies ++= Seq(
      depends.junit),
    commonSettings,
    name := "specs2-junit").
  jvmSettings(depends.jvmTest, commonJvmSettings).
  // nativeSettings(depends.nativeTest, commonNativeSettings).
  dependsOn(core, matcherExtra % Test)

lazy val junitJVM = junit.jvm
// lazy val junitJS = junit.js
// lazy val junitNative = junit.native

lazy val markdown = crossProject(JVMPlatform).
  crossType(CrossType.Pure).
  in(file("markdown")).
  settings(
    libraryDependencies += depends.pegdown,
    commonSettings,
    name := "specs2-markdown").
  jvmSettings(depends.jvmTest, commonJvmSettings).
  // jsSettings(depends.jsTest, commonJsSettings).
  // nativeSettings(depends.nativeTest, commonNativeSettings).
  dependsOn(common, core % "compile->test")

lazy val markdownJVM = markdown.jvm
// lazy val markdownJS = markdown.js
// lazy val markdownNative = markdown.native

lazy val matcher = crossProject(JVMPlatform).in(file("matcher")).
  settings(
    commonSettings,
    name := "specs2-matcher").
  jvmSettings(commonJvmSettings).
  //jsSettings(depends.jsTest, commonJsSettings).
  // nativeSettings(depends.nativeTest, commonNativeSettings).
  dependsOn(common)

lazy val matcherJVM = matcher.jvm
// lazy val matcherJS = matcher.js
// lazy val matcherNative = matcher.native

lazy val matcherExtra = crossProject(JVMPlatform).in(file("matcher-extra")).
  settings(
    commonSettings,
    name := "specs2-matcher-extra").
  jvmSettings(depends.jvmTest, commonJvmSettings).
  // jsSettings(depends.jsTest, commonJsSettings).
  // nativeSettings(depends.nativeTest, commonNativeSettings).
  dependsOn(matcher, core, core % "test->test")

lazy val matcherExtraJVM = matcherExtra.jvm
// lazy val matcherExtraJS = matcherExtra.js
// lazy val matcherExtraNative = matcherExtra.native

lazy val pom = Project(id = "pom", base = file("pom")).
  settings(commonSettings).
  dependsOn(commonJVM, matcherJVM, matcherExtraJVM, coreJVM, html,
    formJVM, markdownJVM, junitJVM, scalacheckJVM)

lazy val scalacheck = crossProject(JVMPlatform).
  in(file("scalacheck")).
  settings(
    commonSettings,
    name := "specs2-scalacheck",
    depends.scalacheck
  ).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  // jsSettings(depends.jsTest, commonJsSettings).
  // platformsSettings(JSPlatform, NativePlatform)(commonJsNativeSettings).
  dependsOn(core)

lazy val scalacheckJVM = scalacheck.jvm
// lazy val scalacheckJS  = scalacheck.js
// lazy val scalacheckNative = scalacheck.native

lazy val tests = Project(id = "tests", base = file("tests")).
  settings(
    commonSettings,
    name := "specs2-tests",
    depends.jvmTest,
    commonJvmSettings
  ).dependsOn(
  coreJVM      % "compile->compile;test->test",
  junitJVM     % "test->test",
  examplesJVM  % "test->test",
  matcherExtraJVM,
  html)

lazy val specs2ShellPrompt = shellPrompt in ThisBuild := { state =>
  val name = Project.extract(state).currentRef.project
  (if (name == "specs2") "" else name) + "> "
}

def scalaSourceVersion(scalaBinaryVersion: String) =
  scalaBinaryVersion.split('.').take(2).mkString(".")

lazy val compilationSettings = Seq(
  // https://gist.github.com/djspiewak/976cd8ac65e20e136f05
  unmanagedSourceDirectories in Compile ++= Seq((sourceDirectory in Compile).value / s"scala-${scalaSourceVersion(scalaBinaryVersion.value)}"),
  maxErrors := 20,
  scalacOptions in Compile ++= (if (isDotty.value) Seq("-language:Scala2Compat,implicitConversions,postfixOps", "-new-syntax", "-indent", "-Ykind-projector") else Nil),
  scalacOptions in Compile ++= Seq("-deprecation:false", "-unchecked", "-feature"),
  scalacOptions in Compile ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v <= 12 => Seq("-Ywarn-unused-import", "-Yno-adapted-args", "-Ypartial-unification")
      case _ => Nil
    }
  },
  scalacOptions in (Compile, doc)    ++= Seq("-feature", "-language:_"),
  scalacOptions in (Compile, console) := Seq("-Yrangepos", "-feature", "-language:_"),
  scalacOptions in (Test, console)    := Seq("-Yrangepos", "-feature", "-language:_")//,
  //addCompilerPlugin("org.scalameta" % "semanticdb-scalac" % "4.1.0" cross CrossVersion.full)
  //,
  // addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
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
  libraryDependencies                            := libraryDependencies.all(aggregateTest).value.flatten) ++
  Seq(scalacOptions in (Compile, doc) += "-Ymacro-expand:none")

lazy val aggregateCompile = ScopeFilter(
  inProjects(fpJVM, commonJVM, matcherJVM, matcherExtraJVM, coreJVM, html, formJVM, markdownJVM, junitJVM, scalacheckJVM),
  inConfigurations(Compile))

lazy val aggregateTest = ScopeFilter(
  inProjects(fpJVM, commonJVM, matcherJVM, matcherExtraJVM, coreJVM, html, formJVM, markdownJVM, junitJVM, scalacheckJVM),
  inConfigurations(Test))

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

def timestamp(instant: Date, format: String = "yyyyMMddHHmmss") = {
  val formatter = new SimpleDateFormat("yyyyMMddHHmmss")
  formatter.setTimeZone(TimeZone.getTimeZone("UTC"))
  formatter.format(instant)
}
