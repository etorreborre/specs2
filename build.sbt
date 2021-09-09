/** ROOT PROJECT */
lazy val specs2 = project
  .in(file("."))
  .enablePlugins(GitBranchPrompt, GitVersioning, ScalaUnidocPlugin)
  .settings(
    name := "specs2",
    rootSettings
  )
  .aggregate(
    fp.jvm,
    common.jvm,
    matcher.jvm,
    core.jvm,
    matcherExtra.jvm,
    junit.jvm,
    scalacheck.jvm,
    xml.jvm,
    examples.jvm,
    fp.js,
    common.js,
    matcher.js,
    core.js,
    matcherExtra.js,
    junit.js,
    scalacheck.js,
    xml.js,
    examples.js,
    markdown,
    form,
    html,
    tests,
    guide
  )

/** COMMON SETTINGS */

val Scala3 = "3.0.2"

lazy val specs2Settings = Seq(
  organization := "org.specs2",
  specs2ShellPrompt,
  ThisBuild / crossScalaVersions := Seq(Scala3),
  ThisBuild / scalaVersion := Scala3
)

lazy val rootSettings =
  specs2Settings ++
    compilationSettings ++
    testSettings ++
    releaseSettings ++
    Seq(
      Compile / doc / sources := sources.all(aggregateCompile).value.flatten,
      packagedArtifacts := Map.empty,
      test := {},
      mimaPreviousArtifacts := Set.empty
    )

lazy val commonSettings =
  specs2Settings ++
    compilationSettings ++
    testSettings ++
    Seq(mimaPreviousArtifacts := Set.empty)

lazy val commonJvmSettings =
  testJvmSettings ++
    Seq(mimaPreviousArtifacts := Set(organization.value %% moduleName.value % "5.0.0-RC-07"))

import org.scalajs.linker.interface.ESVersion

lazy val commonJsSettings =
  depends.jsMacrotaskExecutor ++
    Seq(scalaJSLinkerConfig ~= { _.withESFeatures(_.withESVersion(ESVersion.ES2018)) }) ++
    testJsSettings ++
    Seq(mimaPreviousArtifacts := Set.empty)

/** MODULES (sorted in alphabetical order) */

val platforms = List(JVMPlatform, JSPlatform)
val jvm = JVMPlatform

lazy val common = crossProject(platforms: _*)
  .withoutSuffixFor(jvm)
  .in(file("common"))
  .settings(name := "specs2-common", commonSettings, depends.scalacheckTest, depends.sbt)
  .jvmSettings(commonJvmSettings)
  .jsSettings(commonJsSettings)
  .dependsOn(fp)

lazy val core = crossProject(platforms: _*)
  .withoutSuffixFor(jvm)
  .in(file("core"))
  .settings(name := "specs2-core", commonSettings, depends.junitTest)
  .jvmSettings(commonJvmSettings)
  .jsSettings(commonJsSettings)
  .dependsOn(matcher, common, common % "test->test")

lazy val examples = crossProject(platforms: _*)
  .withoutSuffixFor(jvm)
  .crossType(CrossType.Pure)
  .in(file("examples"))
  .jvmSettings(commonJvmSettings)
  .jsSettings(commonJsSettings)
  // no mima check because that jar is not published
  .settings(commonSettings, name := "specs2-examples", mimaPreviousArtifacts := Set.empty)
  .dependsOn(common, matcher, core, matcherExtra, junit, scalacheck)

lazy val fp = crossProject(platforms: _*)
  .withoutSuffixFor(jvm)
  .crossType(CrossType.Pure)
  .in(file("fp"))
  .settings(name := "specs2-fp", commonSettings)
  .jvmSettings(commonJvmSettings)
  .jsSettings(commonJsSettings)

lazy val form = project
  .in(file("form"))
  .settings(name := "specs2-form", commonSettings)
  .dependsOn(core.jvm, markdown, matcherExtra.jvm, scalacheck.jvm % "test->test", xml.jvm)

lazy val guide = project
  .in(file("guide"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "specs2-guide",
    commonSettings,
    Compile / scalacOptions --= Seq("-Xlint", "-Ywarn-unused-import"),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.specs2"
  )
  .dependsOn(html, form % "compile->compile;test->test", examples.jvm % "compile->compile;test->test")

lazy val html = project
  .in(file("html"))
  .settings(name := "specs2-html", commonSettings, libraryDependencies += depends.tagsoup)
  .dependsOn(form, matcherExtra.jvm % Test, scalacheck.jvm % Test)

lazy val junit = crossProject(platforms: _*)
  .withoutSuffixFor(jvm)
  .crossType(CrossType.Pure)
  .in(file("junit"))
  .settings(name := "specs2-junit", commonSettings, depends.junit)
  .jvmSettings(commonJvmSettings)
  .jsSettings(commonJsSettings)
  .dependsOn(common, core, matcherExtra % Test, xml)

lazy val markdown = project
  .in(file("markdown"))
  .settings(name := "specs2-markdown", commonSettings, libraryDependencies += depends.flexmark)
  .dependsOn(common.jvm, core.jvm, xml.jvm)

lazy val matcher = crossProject(platforms: _*)
  .withoutSuffixFor(jvm)
  .in(file("matcher"))
  .settings(name := "specs2-matcher", commonSettings)
  .jvmSettings(commonJvmSettings)
  .jsSettings(commonJsSettings)
  .dependsOn(common)

lazy val matcherExtra = crossProject(platforms: _*)
  .withoutSuffixFor(jvm)
  .in(file("matcher-extra"))
  .settings(name := "specs2-matcher-extra", commonSettings, depends.scalaParser)
  .jvmSettings(commonJvmSettings)
  .jsSettings(commonJsSettings)
  .dependsOn(matcher, core, core % "test->test", xml)

lazy val pom = project
  .in(file("pom"))
  .settings(commonSettings)
  .dependsOn(common.jvm, matcher.jvm, matcherExtra.jvm, core.jvm, form, markdown, junit.jvm, scalacheck.jvm, html)

lazy val scalacheck = crossProject(platforms: _*)
  .withoutSuffixFor(jvm)
  .crossType(CrossType.Pure)
  .in(file("scalacheck"))
  .settings(
    commonSettings,
    name := "specs2-scalacheck",
    depends.scalacheck
  )
  .jvmSettings(commonJvmSettings)
  .jsSettings(commonJsSettings)
  .dependsOn(core)

lazy val tests = project
  .in(file("tests"))
  .settings(
    commonSettings,
    name := "specs2-tests"
  )
  .dependsOn(
    core.jvm % "compile->compile;test->test",
    junit.jvm % "test->test",
    examples.jvm % "test->test",
    matcherExtra.jvm,
    html
  )

lazy val xml = crossProject(platforms: _*)
  .withoutSuffixFor(jvm)
  .crossType(CrossType.Pure)
  .in(file("xml"))
  .settings(
    name := "specs2-xml",
    depends.scalaXml,
    commonSettings
  )
  .jvmSettings(commonJvmSettings)
  .jsSettings(commonJsSettings)
  .dependsOn(core)

lazy val specs2ShellPrompt = ThisBuild / shellPrompt := { state =>
  val name = Project.extract(state).currentRef.project
  (if (name == "specs2") "" else name) + "> "
}

def scalaSourceVersion(scalaBinaryVersion: String) =
  scalaBinaryVersion.split('.').take(2).mkString(".")

lazy val compilationSettings = Seq(
  maxErrors := 20,
  Global / onChangedBuildSource := ReloadOnSourceChanges,
  Compile / scalacOptions ++= compilationOptions,
  Compile / doc / scalacOptions --= compilationOptions
)

lazy val compilationOptions = Seq(
  "-source:future-migration",
  "-language:implicitConversions,postfixOps",
  "-Ykind-projector",
  "-Xcheck-macros",
  "-deprecation:false",
  "-unchecked",
  "-feature"
)

lazy val testSettings = Seq(
  logBuffered := false,
  Global / cancelable := true,
  testFrameworks := Seq(TestFramework("org.specs2.runner.Specs2Framework")),
  testOptions := Seq(
    Tests.Filter(s =>
      (Seq(".guide.").exists(s.contains) || Seq("Spec", "Guide", "Website").exists(s.endsWith)) &&
        Seq("Specification", "FeaturesSpec").forall(n => !s.endsWith(n))
    )
  )
) ++ depends.sharedTest

lazy val testJvmSettings = Seq(
  javaOptions ++= Seq("-Xmx3G", "-Xss4M"),
  Test / fork := true
) ++ depends.jvmTest

lazy val testJsSettings = Seq(
  Test / fork := false,
  Test / parallelExecution := false,
  Test / scalaJSStage := FastOptStage
) ++ depends.jsTest

/** RELEASE
  */
lazy val releaseSettings: Seq[Setting[_]] = Seq(
  ThisBuild / versionScheme := Some("early-semver"),
  ThisBuild / githubWorkflowArtifactUpload := false,
  ThisBuild / githubWorkflowBuildPreamble ++= List(
    WorkflowStep.Sbt(List("scalafmtCheckAll"), name = Some("Check formatting ✔"))
  ),
  ThisBuild / githubWorkflowBuild := Seq(
    WorkflowStep
      .Sbt(name = Some("Build and test 🔧"), commands = List("testOnly -- xonly exclude ci,website timefactor 3"))
  ),
  ThisBuild / githubWorkflowTargetTags ++= Seq(SPECS2 + "*"),
  ThisBuild / githubWorkflowPublishTargetBranches := Seq(RefPredicate.StartsWith(Ref.Tag(SPECS2))),
  ThisBuild / githubWorkflowPublishPreamble ++= List(
    WorkflowStep.Sbt(List("mimaReportBinaryIssues"), name = Some("Check binary compatibility ✔"))
  ),
  ThisBuild / githubWorkflowPublish := Seq(
    WorkflowStep.Sbt(
      name = Some("Release to Sonatype 📇"),
      commands = List("ci-release"),
      env = Map(
        "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
        "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
        "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
        "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
      )
    ),
    WorkflowStep.Use(
      name = Some("Install Pandoc 🏁"),
      ref = UseRef.Public("r-lib/actions", "setup-pandoc", "v1"),
      params = Map("pandoc-version" -> "2.7.3")
    ),
    WorkflowStep
      .Sbt(name = Some("Generate the specs2 website 📚"), commands = List("guide/testOnly *Website -- xonly")),
    WorkflowStep.Use(
      name = Some("Update the website 🚀"),
      ref = UseRef.Public("JamesIves", "github-pages-deploy-action", "4.1.4"),
      params = Map("branch" -> "gh-pages", "clean" -> "false", "folder" -> "target/specs2-reports/site")
    )
  ),
  organization := "org.specs2",
  homepage := Some(url("https://github.com/etorreborre/specs2")),
  licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer(
      "etorreborre",
      "Eric Torreborre",
      "etorreborre@yahoo.com",
      url("https://github.com/etorreborre")
    )
  ),
  ThisBuild / git.gitTagToVersionNumber := { tag: String =>
    if (tag matches SPECS2 + ".*") Some(tag.replace(SPECS2, "")) else None
  },
  ThisBuild / git.useGitDescribe := true,
  ThisBuild / dynverTagPrefix := SPECS2
)

val SPECS2 = "SPECS2-"

lazy val aggregateCompile = ScopeFilter(
  inProjects(
    fp.jvm,
    common.jvm,
    matcher.jvm,
    core.jvm,
    matcherExtra.jvm,
    html,
    form,
    markdown,
    junit.jvm,
    scalacheck.jvm,
    xml.jvm,
    examples.jvm
  ),
  inConfigurations(Compile)
)
