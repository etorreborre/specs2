import sbt._
import Defaults._
import libraryDependencies._
import java.util.{Date, TimeZone}
import java.text.SimpleDateFormat

/** MAIN PROJECT */
lazy val specs2 = project.in(file(".")).
  enablePlugins(GitBranchPrompt, SitePlugin, GhpagesPlugin).
  settings(
    commonSettings,
    siteSettings,
    name := "specs2",
    packagedArtifacts := Map.empty
  ).aggregate(
    fpJVM, commonJVM, matcherJVM, coreJVM, matcherExtraJVM, html,
    formJVM, markdownJVM, junitJVM, scalacheckJVM,
    tests
  )

val scala3 = "3.0.0-RC2"

/** COMMON SETTINGS */
lazy val specs2Settings = Seq(
  organization := "org.specs2",
  specs2ShellPrompt,
  scalaVersion := scala3,
  Compile / doc / sources := Seq())

lazy val commonJsSettings = Seq(
  scalacOptions += {
    val tag = s"SPECS2-${version.value}"
    val tagOrHash =
      if(isSnapshot.value) sys.process.Process("git rev-parse HEAD").lineStream_!.head
      else tag
    val a = (LocalRootProject / baseDirectory).value.toURI.toString
    val g = "https://raw.githubusercontent.com/etorreborre/specs2/" + tagOrHash
    s"-P:scalajs:mapSourceURI:$a->$g/"
  },
  Test / parallelExecution := false
)

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
    depends.scalaXML,
    libraryDependencies += depends.scalacheck % Test,
    commonSettings,
    name := "specs2-common"
  ).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  dependsOn(fp)

lazy val commonJVM = common.jvm

lazy val core = crossProject(JVMPlatform).in(file("core")).
  settings(
    commonSettings,
    name := "specs2-core",
    libraryDependencies += depends.junit % Test
  ).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  dependsOn(matcher, common, common % "test->test")

lazy val coreJVM = core.jvm

lazy val examples = crossProject(JVMPlatform).in(file("examples")).
  settings(
    commonSettings,
    name := "specs2-examples").
  jvmSettings(depends.jvmTest, commonJvmSettings).
  dependsOn(common, matcher, core, matcherExtra, junit, scalacheck)

lazy val examplesJVM = examples.jvm.dependsOn(formJVM, html, markdownJVM)

lazy val fp = crossProject(JVMPlatform).in(file("fp")).
  settings(commonSettings:_*).
  settings(name := "specs2-fp").
  jvmSettings(commonJvmSettings)//.

lazy val fpJVM = fp.jvm

lazy val form = crossProject(JVMPlatform).
  crossType(CrossType.Pure).
  in(file("form")).
  settings(
    commonSettings,
    name := "specs2-form").
  jvmSettings(depends.jvmTest, commonJvmSettings).
  dependsOn(core, markdown, matcherExtra, scalacheck % "test->test")

lazy val formJVM = form.jvm

lazy val guide = project.in(file("guide")).
  enablePlugins(BuildInfoPlugin).
  settings(
    commonSettings,
    name := "specs2-guide",
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.specs2",
    Compile / scalacOptions --= Seq("-Xlint", "-Ywarn-unused-import")).
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
  dependsOn(core, matcherExtra % Test)

lazy val junitJVM = junit.jvm

lazy val markdown = crossProject(JVMPlatform).
  crossType(CrossType.Pure).
  in(file("markdown")).
  settings(
    libraryDependencies += depends.flexmark,
    commonSettings,
    name := "specs2-markdown").
  jvmSettings(depends.jvmTest, commonJvmSettings).
  dependsOn(common, core % "compile->test")

lazy val markdownJVM = markdown.jvm

lazy val matcher = crossProject(JVMPlatform).in(file("matcher")).
  settings(
    commonSettings,
    name := "specs2-matcher").
  jvmSettings(commonJvmSettings).
  dependsOn(common)

lazy val matcherJVM = matcher.jvm

lazy val matcherExtra = crossProject(JVMPlatform).in(file("matcher-extra")).
  settings(
    commonSettings,
    name := "specs2-matcher-extra").
  jvmSettings(depends.jvmTest, commonJvmSettings).
  platformsSettings(JVMPlatform)(depends.scalaParser).
  dependsOn(matcher, core, core % "test->test")

lazy val matcherExtraJVM = matcherExtra.jvm

lazy val pom = Project(id = "pom", base = file("pom")).
  settings(commonSettings).
  dependsOn(commonJVM, matcherJVM, matcherExtraJVM, coreJVM, html,
    formJVM, markdownJVM, junitJVM, scalacheckJVM)

lazy val scalacheck = crossProject(JVMPlatform).
  in(file("scalacheck")).
  settings(
    commonSettings,
    name := "specs2-scalacheck",
    libraryDependencies += depends.scalacheck,
  ).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  dependsOn(core)

lazy val scalacheckJVM = scalacheck.jvm

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

lazy val specs2ShellPrompt = ThisBuild / shellPrompt := { state =>
  val name = Project.extract(state).currentRef.project
  (if (name == "specs2") "" else name) + "> "
}

def scalaSourceVersion(scalaBinaryVersion: String) =
  scalaBinaryVersion.split('.').take(2).mkString(".")

lazy val compilationSettings = Seq(
  maxErrors := 20,
  Compile / scalacOptions ++= Seq(
    "-source:future-migration",
    "-language:implicitConversions,postfixOps",
    "-Ykind-projector",
    "-Xcheck-macros",
    "-deprecation:false",
    "-unchecked",
    "-feature")
)

lazy val testingSettings = Seq(
  logBuffered := false,
  Global / cancelable := true,
  testFrameworks := Seq(TestFramework("org.specs2.runner.Specs2Framework")),
  testOptions := Seq(Tests.Filter(s =>
    (Seq(".guide.").exists(s.contains) || Seq("Spec", "Guide", "Website").exists(s.endsWith)) &&
      Seq("Specification", "FeaturesSpec").forall(n => !s.endsWith(n))))
)

lazy val testingJvmSettings = Seq(
  javaOptions ++= Seq("-Xmx3G", "-Xss4M"),
  Test / fork := true
)

/**
 * DOCUMENTATION
 */
lazy val siteSettings = GhpagesPlugin.projectSettings ++ SitePlugin.projectSettings ++
  Seq(
    siteSourceDirectory := target.value / "specs2-reports" / "site",
    makeSite / includeFilter := AllPassFilter,
    // override the synchLocal task to avoid removing the existing files
    ghpagesSynchLocal := {
      val betterMappings = ghpagesPrivateMappings.value map { case (file, target) => (file, ghpagesUpdatedRepository.value / target) }
      IO.copy(betterMappings)
      ghpagesUpdatedRepository.value
    },
    git.remoteRepo := "git@github.com:etorreborre/specs2.git"
  )

/**
 * PUBLICATION
 */
lazy val publicationSettings = Seq(
  Global / publishTo := sonatypePublishToBundle.value,
  publishMavenStyle := true,
  Test / publishArtifact := false,
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
