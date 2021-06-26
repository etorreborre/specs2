/** ROOT PROJECT */
lazy val specs2 = project.in(file(".")).
  enablePlugins(GitBranchPrompt, GitVersioning, ScalaUnidocPlugin).
  settings(
    name := "specs2",
    rootSettings,
  ).aggregate(
    fp, common, matcher, core, matcherExtra, html, guide,
    form, markdown, junit, scalacheck, xml, tests
  )

/** COMMON SETTINGS */

val Scala212 = "2.12.13"
val Scala213 = "2.13.5"
val Scala3 = "3.0.0"

lazy val specs2Settings = Seq(
  organization := "org.specs2",
  specs2ShellPrompt,
  ThisBuild / crossScalaVersions := Seq(Scala3),
  ThisBuild / scalaVersion := Scala3)

lazy val rootSettings =
  specs2Settings       ++
  compilationSettings  ++
  testingSettings      ++
  testingJvmSettings   ++
  releaseSettings      ++
  Seq(
    Compile / doc / sources := sources.all(aggregateCompile).value.flatten,
    packagedArtifacts := Map.empty,
    test := {},
  )

lazy val commonSettings =
  specs2Settings       ++
  compilationSettings  ++
  testingSettings      ++
  testingJvmSettings

lazy val commonJsSettings = Seq(
  scalacOptions += {
    val tag = "${version.value}"
    val tagOrHash =
      if(isSnapshot.value) sys.process.Process("git rev-parse HEAD").lineStream_!.head
      else tag
    val a = (LocalRootProject / baseDirectory).value.toURI.toString
    val g = "https://raw.githubusercontent.com/etorreborre/specs2/" + tagOrHash
    s"-P:scalajs:mapSourceURI:$a->$g/"
  },
  Test / parallelExecution := false
)

/** MODULES (sorted in alphabetical order) */

lazy val common = project.in(file("common")).
  settings(
    libraryDependencies ++= Seq(depends.sbt,depends.scalacheck % Test),
    commonSettings,
    name := "specs2-common"
  ).
  dependsOn(fp)

lazy val core = project.in(file("core")).
  settings(
    commonSettings,
    name := "specs2-core",
    libraryDependencies += depends.junit % Test
  ).
  dependsOn(matcher, common, common % "test->test")

lazy val examples = project.in(file("examples")).
  settings(
    commonSettings,
    name := "specs2-examples").
  dependsOn(common, matcher, core, matcherExtra, junit, scalacheck, form, html, markdown)

lazy val fp = project.in(file("fp")).
  settings(commonSettings).
  settings(name := "specs2-fp")

lazy val form = project.
  in(file("form")).
  settings(
    commonSettings,
    name := "specs2-form").
  dependsOn(core, markdown, matcherExtra, scalacheck % "test->test")

lazy val guide = project.in(file("guide")).
  enablePlugins(BuildInfoPlugin).
  settings(
    commonSettings,
    name := "specs2-guide",
    Compile / scalacOptions --= Seq("-Xlint", "-Ywarn-unused-import"),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.specs2").
  dependsOn(examples % "compile->compile;test->test")

lazy val html = project.in(file("html")).
  settings(
    libraryDependencies += depends.tagsoup,
    commonSettings,
    name := "specs2-html").
  dependsOn(form, matcherExtra % Test, scalacheck % Test)

lazy val junit = project.in(file("junit")).
  settings(
    libraryDependencies ++= Seq(
      depends.junit),
    commonSettings,
    name := "specs2-junit").
  dependsOn(core, matcherExtra % Test, xml)

lazy val markdown = project.
  in(file("markdown")).
  settings(
    libraryDependencies += depends.flexmark,
    commonSettings,
    name := "specs2-markdown").
  dependsOn(common, core % "compile->test", xml)

lazy val matcher = project.in(file("matcher")).
  settings(
    commonSettings,
    name := "specs2-matcher").
  dependsOn(common)

lazy val matcherExtra = project.in(file("matcher-extra")).
  settings(
    commonSettings,
    libraryDependencies += depends.scalaParser,
    name := "specs2-matcher-extra").
  dependsOn(matcher, core, core % "test->test", xml)

lazy val pom = Project(id = "pom", base = file("pom")).
  settings(commonSettings).
  dependsOn(common, matcher, matcherExtra, core, html,
    form, markdown, junit, scalacheck)

lazy val scalacheck = project.
  in(file("scalacheck")).
  settings(
    commonSettings,
    name := "specs2-scalacheck",
    libraryDependencies += depends.scalacheck,
  ).
  dependsOn(core)

lazy val tests = Project(id = "tests", base = file("tests")).
  settings(
    commonSettings,
    name := "specs2-tests",
  ).dependsOn(
  core      % "compile->compile;test->test",
  junit     % "test->test",
  examples  % "test->test",
  matcherExtra,
  html)

lazy val xml = project.in(file("xml")).
  settings(
    libraryDependencies += depends.scalaXml,
    commonSettings,
    name := "specs2-xml"
  ).
  dependsOn(core)

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
  Compile / doc / scalacOptions ++= compilationOptions)

lazy val compilationOptions =  Seq(
    "-source:future-migration",
    "-language:implicitConversions,postfixOps",
    "-Ykind-projector",
    "-Xcheck-macros",
    "-deprecation:false",
    "-unchecked",
    "-feature")

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
 * RELEASE
 */
lazy val releaseSettings: Seq[Setting[_]] = Seq(
  ThisBuild / githubWorkflowArtifactUpload := false,
  ThisBuild / githubWorkflowBuild := Seq(
    WorkflowStep.Sbt(
      name = Some("Build and test 🔧"),
      commands = List("testOnly -- xonly exclude ci,website timefactor 3"))),
  ThisBuild / githubWorkflowTargetTags ++= Seq(SPECS2+"*"),
  ThisBuild / githubWorkflowPublishTargetBranches := Seq(RefPredicate.StartsWith(Ref.Tag(SPECS2))),
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
      params = Map("pandoc-version" -> "2.7.3")),
    WorkflowStep.Sbt(
      name = Some("Generate the specs2 website 📚"),
      commands = List("guide/testOnly *Website -- xonly")),
    WorkflowStep.Use(
      name = Some("Update the website 🚀"),
      ref = UseRef.Public("JamesIves", "github-pages-deploy-action", "4.1.4"),
      params = Map("branch" -> "gh-pages",
                   "clean" -> "false",
                   "folder" -> "guide/target/specs2-reports/site"))
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
  ThisBuild / git.gitTagToVersionNumber := { tag: String => if (tag matches SPECS2+".*") Some(tag.replace(SPECS2, "")) else None },
  ThisBuild / git.useGitDescribe := true,
  ThisBuild / dynverTagPrefix := SPECS2)

val SPECS2 = "SPECS2-"

lazy val aggregateCompile = ScopeFilter(
  inProjects(fp, common, matcher, core, matcherExtra, html, form, markdown, junit, scalacheck, xml),
  inConfigurations(Compile))
