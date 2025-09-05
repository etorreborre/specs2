import sbt._
import Defaults._
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
    name := "specs2",
    packagedArtifacts := Map.empty,
    ThisBuild / githubWorkflowArtifactUpload := false,
    ThisBuild / githubWorkflowUseSbtThinClient := false,
    ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("11")),
    ThisBuild / githubWorkflowBuild := Seq(WorkflowStep.Sbt(List("testOnly -- xonly exclude ci"), name = Some("Build project"))),
    Global / onChangedBuildSource := ReloadOnSourceChanges,
    test := {}
  ).aggregate(
    fpJVM, catsJVM, commonJVM, matcherJVM, coreJVM, matcherExtraJVM, scalazJVM, html,
    formJVM, markdownJVM, junitJVM, scalacheckJVM, xmlJVM,
    tests, fpJS, catsJS, commonJS, matcherJS, coreJS, matcherExtraJS, scalazJS,
    junitJS, scalacheckJS, xmlJS, fpNative, commonNative, matcherNative, coreNative, matcherExtraNative, scalazNative,
    junitNative, scalacheckNative, xmlNative
  )

/** COMMON SETTINGS */
lazy val specs2Settings = Seq(
  organization := "org.specs2",
  GlobalScope / scalazVersion := "7.2.34",
  specs2ShellPrompt,
  ThisBuild / scalaVersion := "3.3.3",
  ThisBuild / crossScalaVersions := Seq("3.3.3"))

lazy val tagName = Def.setting {
  s"specs2-${version.value}"
}

lazy val commonJsSettings = Seq(
  Test / parallelExecution := false
  ) ++ depends.jsMacrotaskExecutor

lazy val commonNativeSettings = Seq(
)

lazy val specs2Version = settingKey[String]("defines the current specs2 version")
lazy val scalazVersion = settingKey[String]("defines the current scalaz version")
lazy val catsVersion = "2.13.0"
lazy val catsEffectVersion = "3.1.1"

val commonSettings: Seq[Def.Setting[_]] =
    coreDefaultSettings  ++
    depends.resolvers    ++
    specs2Settings       ++
    compilationSettings  ++
    testingSettings      ++
    publicationSettings

def commonJvmSettings =
  testingJvmSettings

/** MODULES (sorted in alphabetical order) */
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
    libraryDependencies += depends.scalacheck.value % Test,
    commonSettings,
    name := "specs2-common"
  ).
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  nativeSettings(depends.nativeTest, commonNativeSettings).
  dependsOn(fp)

lazy val commonJS = common.js
lazy val commonJVM = common.jvm
lazy val commonNative = common.native

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("core")).
  settings(
    commonSettings,
    name := "specs2-core",
    libraryDependencies ++=
      Seq(depends.junit % Test)
  ).
  jsSettings(depends.jsTest, commonJsSettings).
  platformsSettings(JSPlatform)(
    commonJsSettings
  ).
  jvmSettings(
    depends.jvmTest,
    commonJvmSettings).
  dependsOn(matcher, common, common % "test->test")

lazy val coreJVM = core.jvm
lazy val coreJS = core.js
lazy val coreNative = core.native

lazy val examples = crossProject(JSPlatform, JVMPlatform).in(file("examples")).
  settings(
    commonSettings,
    name := "specs2-examples").
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  dependsOn(common, matcher, core, matcherExtra, junit, scalacheck)

lazy val examplesJVM = examples.jvm.dependsOn(formJVM, html, markdownJVM)
lazy val examplesJS = examples.js

lazy val fp = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("fp")).
  settings(commonSettings:_*).
  settings(name := "specs2-fp").
  jvmSettings(commonJvmSettings).
  jsSettings(depends.jsTest, commonJsSettings)

lazy val fpJVM = fp.jvm
lazy val fpJS = fp.js
lazy val fpNative = fp.native

lazy val form = crossProject(JSPlatform, JVMPlatform).
  crossType(CrossType.Pure).
  in(file("form")).
  settings(
    commonSettings,
    name := "specs2-form").
  jvmSettings(depends.jvmTest, commonJvmSettings).
  jsSettings(depends.jsTest, commonJsSettings).
  dependsOn(core, markdown, matcherExtra, scalacheck % "test->test", xml)

lazy val formJVM = form.jvm
lazy val formJS = form.js

lazy val guide = project.in(file("guide")).
  enablePlugins(BuildInfoPlugin).
  settings(
    commonSettings,
    name := "specs2-guide",
    Compile / scalacOptions --= Seq("-Xlint", "-Ywarn-unused-import")).
  dependsOn(examplesJVM % "compile->compile;test->test", scalazJVM)

lazy val html = project.in(file("html")).
  settings(
    libraryDependencies += depends.tagsoup,
    depends.scalaParser,
    commonSettings,
    name := "specs2-html").
  settings(depends.jvmTest, commonJvmSettings).
  dependsOn(formJVM, matcherExtraJVM % Test, scalacheckJVM % Test, xmlJVM)

lazy val junit = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("junit")).
  settings(
    libraryDependencies ++= Seq(
      depends.junit),
    depends.scalaXml,
    commonSettings,
    name := "specs2-junit").
  jvmSettings(depends.jvmTest, commonJvmSettings).
  dependsOn(core, matcherExtra % Test)

lazy val junitJVM = junit.jvm
lazy val junitJS = junit.js
lazy val junitNative = junit.native

lazy val markdown = crossProject(JSPlatform, JVMPlatform).
  crossType(CrossType.Pure).
  in(file("markdown")).
  settings(
    libraryDependencies ++= Seq(
      depends.pegdown),
    depends.scalaXml,
    commonSettings,
    name := "specs2-markdown").
  jvmSettings(depends.jvmTest, commonJvmSettings).
  jsSettings(depends.jsTest, commonJsSettings).
  dependsOn(common, core % "compile->test")

lazy val markdownJVM = markdown.jvm
lazy val markdownJS = markdown.js

lazy val matcher = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("matcher")).
  settings(
    commonSettings,
    name := "specs2-matcher").
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(commonJvmSettings).
  dependsOn(common)

lazy val matcherJS = matcher.js
lazy val matcherJVM = matcher.jvm
lazy val matcherNative = matcher.native

lazy val matcherExtra = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("matcher-extra")).
  settings(
    commonSettings,
    depends.scalaParser,
    name := "specs2-matcher-extra"
  ).
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  nativeSettings(depends.nativeTest, commonNativeSettings).
  dependsOn(matcher, xml, core % "test->test")

lazy val matcherExtraJS = matcherExtra.js
lazy val matcherExtraJVM = matcherExtra.jvm
lazy val matcherExtraNative = matcherExtra.native

lazy val pom = Project(id = "pom", base = file("pom")).
  settings(commonSettings).
  dependsOn(catsJVM, commonJVM, matcherJVM, matcherExtraJVM, coreJVM, scalazJVM, html,
  formJVM, markdownJVM, junitJVM, scalacheckJVM)

lazy val scalaz = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("scalaz")).
  settings(
    commonSettings,
    libraryDependencies ++=
      depends.scalaz(scalazVersion.value) :+
      depends.scalazConcurrent(scalazVersion.value),
    name := "specs2-scalaz").
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  dependsOn(matcher, core % "test->test")

lazy val scalazJS = scalaz.js
lazy val scalazJVM = scalaz.jvm
lazy val scalazNative = scalaz.native

lazy val scalacheck = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalacheck")).
  settings(
    commonSettings,
    name := "specs2-scalacheck",
    libraryDependencies += depends.scalacheck.value
  ).
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(depends.jvmTest, commonJvmSettings).
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
  junitJVM     % "test->test",
  examplesJVM  % "test->test",
  matcherExtraJVM,
  html,
  scalazJVM,
  catsJVM)

lazy val xml = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("xml")).
  settings(
    depends.scalaXml,
    commonSettings,
    name := "xml"
  ).
  jsSettings(depends.jsTest, commonJsSettings).
  jvmSettings(depends.jvmTest, commonJvmSettings).
  dependsOn(core)

lazy val xmlJVM = xml.jvm
lazy val xmlJS = xml.js
lazy val xmlNative = xml.native

lazy val specs2ShellPrompt = ThisBuild / shellPrompt := { state =>
  val name = Project.extract(state).currentRef.project
  (if (name == "specs2") "" else name) + "> "
}

def scalaSourceVersion(scalaBinaryVersion: String) =
  scalaBinaryVersion.split('.').take(2).mkString(".")

lazy val compilationSettings: Seq[Def.Setting[_]] = Seq(
  // https://gist.github.com/djspiewak/976cd8ac65e20e136f05
  Compile / unmanagedSourceDirectories ++=
    Seq((Compile / sourceDirectory).value / s"scala-${scalaSourceVersion(scalaBinaryVersion.value)}",
      (Compile / sourceDirectory).value / s"scala-scalaz-7.1.x",
      (Test / test / sourceDirectory).value / s"scala-scalaz-7.1.x"),
  maxErrors := 20,
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => Seq(
        "-old-syntax", "-rewrite",
        "-language:implicitConversions",
        "-language:_",
        "-Ykind-projector:underscores")
      case Some((2, v)) if v <= 12 =>
        Seq(
          "-Ypartial-unification",
          "-language:_",
          "-Xsource:3",
         "-P:kind-projector:underscore-placeholders"
        )
      case _ => Seq(
        "-Xlint",
        "-Ywarn-numeric-widen",
        "-Ywarn-value-discard",
        "-deprecation:false",
        "-Xcheckinit",
        "-unchecked",
        "-feature",
        "-language:_",
        "-Xsource:3",
        "-P:kind-projector:underscore-placeholders")
    }
  },
  Test / scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => Seq()
      case _ => Seq("-Yrangepos")
    }
  },
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
       case Some((3, _)) => Seq()
       case _ => List(compilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full))
    }
  },
  Compile / doc / scalacOptions ++= Seq("-feature", "-language:_"),
  Compile / console / scalacOptions := Seq("-feature", "-language:_"),
  Test / console / scalacOptions := Seq("-feature", "-language:_"),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
       case Some((3, _)) => Seq()
       case _ => List(compilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full))
    }
  }
)


lazy val testingSettings = Seq(
  test / console / initialCommands := "import org.specs2.*",
  logBuffered := false,
  Global / cancelable := true,
  testFrameworks := Seq(TestFramework("org.specs2.runner.Specs2Framework")),
  testOptions := Seq(Tests.Filter(s =>
    (Seq(".guide.").exists(s.contains) || Seq("Spec", "Guide", "Website").exists(s.endsWith)) &&
      Seq("Specification", "FeaturesSpec").forall(n => !s.endsWith(n))))
)

lazy val testingJvmSettings = Seq(
  javaOptions ++= Seq("-Xmx3G", "-Xss4M", "--add-opens=java.base/java.lang=ALL-UNNAMED"),
  Test / fork := true
)

/**
 * DOCUMENTATION
 */
lazy val siteSettings = GhpagesPlugin.projectSettings ++ SitePlugin.projectSettings ++
  Seq(
    siteSourceDirectory := target.value / "specs2-reports" / "site",
    // copy the api files to a versioned directory
    siteMappings ++= { (Compile / packageDoc / mappings).value.map { case (f, d) => (f, s"api/SPECS2-${version.value}/$d") } },
    makeSite / includeFilter := AllPassFilter,
    // override the synchLocal task to avoid removing the existing files
    ghpagesSynchLocal := {
      val betterMappings = ghpagesPrivateMappings.value map { case (file, target) => (file, ghpagesUpdatedRepository.value / target) }
      IO.copy(betterMappings)
      ghpagesUpdatedRepository.value
    },
    git.remoteRepo := "git@github.com:etorreborre/specs2.git"
  )

lazy val apiSettings = Seq(
  Compile / doc / sources := sources.all(aggregateCompile).value.flatten,
  Compile / doc / unmanagedSources := unmanagedSources.all(aggregateCompile).value.flatten,
  libraryDependencies := libraryDependencies.all(aggregateTest).value.flatten) ++
  Seq(Compile / doc / scalacOptions += "-Ymacro-expand:none")

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
