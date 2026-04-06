import com.typesafe.tools.mima.core._

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
    fp.native,
    common.native,
    matcher.native,
    core.native,
    matcherExtra.native,
    scalacheck.native,
    xml.native,
    markdown,
    form,
    html,
    tests.jvm,
    tests.js,
    guide
  )

/** COMMON SETTINGS */

val Scala3 = "3.3.7"

lazy val specs2Settings = Seq(
  organization := "org.specs2",
  homepage := Some(url("https://github.com/etorreborre/specs2")),
  licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer("etorreborre", "Eric Torreborre", "etorreborre@yahoo.com", url("https://github.com/etorreborre"))
  ),
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
      mimaPreviousArtifacts := Set(),
      mimaFailOnNoPrevious := false
    )

lazy val commonSettings =
  specs2Settings ++
    compilationSettings ++
    testSettings ++
    mimaSettings

lazy val mimaSettings =
  Seq(
    mimaPreviousArtifacts := Set(organization.value %% moduleName.value % "5.0.0"),
    mimaFailOnNoPrevious := false,
    mimaBinaryIssueFilters ++= Seq(
      // This was needed to fix a warning (see https://github.com/etorreborre/specs2/commit/343bf6d5425733f89c5a3f1e237b39e37a3205a8)
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.execute.Result.*"),
      // changed the implementation and return types for a better API
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.matcher.ExceptionMatchers*"),
      ProblemFilters.exclude[MissingClassProblem]("org.specs2.matcher.ExceptionMatchers*"),
      // changed the implementation and return types for a better API
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.matcher.Try*"),
      ProblemFilters.exclude[MissingClassProblem]("org.specs2.matcher.Try*"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.matcher.Try*"),
      ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.specs2.matcher.Try*"),
      // made the signature more specific but also more correct
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.reporter.HtmlBodyPrinter.printStatistics"),
      // fixed warnings when upgrading to Scala 3.1.3: https://github.com/etorreborre/specs2/commit/763891e99b8ab74cfeb58b557968f17c84b2b3b2
      ProblemFilters.exclude[ReversedMissingMethodProblem]("org.specs2.specification.dsl.mutable.ReferenceDsl.*"),
      // reworked the internals of the json matchers
      ProblemFilters.exclude[DirectMissingMethodProblem](
        "org.specs2.matcher.JsonMatchers#JsonMatcher.anyValueToJsonType"
      ),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.matcher.JsonSelectors*"),
      ProblemFilters.exclude[MissingClassProblem]("org.specs2.matcher.JsonSelectors*"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("org.specs2.matcher.JsonSelectors*"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.matcher.JsonSelectors*"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.matcher.JsonMatchers.JsonEqualValueSelector"),

      // the fixed executor has a new argument in order to remove warnings when some futures cannot be completed
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.concurrent.ExecutorServices.fixedExecutor"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.main.Execute.*"),

      // CustomInstances, PrinterFactory, SpecFactory, SpecificationFinder cannot have a default Env
      // because there a risk that the default env not be shutdown. Moreover the Env.shutdown functions
      // have been fixed
      ProblemFilters.exclude[Problem]("org.specs2.reporter.CustomInstances.*"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.reporter.PrinterFactory.default"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.runner.SpecFactory.default"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.runner.SpecificationsFinder.default"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.specification.core.Env.shutdown"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.specification.core.Env.shutdownResult"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.specification.core.Env.awaitShutdown"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.specification.core.EnvDefault.<clinit>"),

      // The OwnEnv and OwnExecutionEnv traits should not expect to have a val env defined when mixed with a specification
      // Because, in that case, it is very possible to confuse the ownEnv with the env and shutdown the env
      // which breaks the execution of the whole specification
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.specification.core.OwnEnv.ownEnv"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.specification.core.OwnExecutionEnv.env"),

      // issue #1277 NaturalTransformation needs to be made lazier
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("org.specs2.fp.NaturalTransformation.apply"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("org.specs2.fp.NaturalTransformation.apply"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("org.specs2.fp.NaturalTransformation#naturalId.apply"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem]("org.specs2.control.Operation#operationToAction.apply"),

      // issue #1322 Typecheck equality is now stricter. Breaks are potential issues that should be fixed
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.matcher.ReturnsSyntax.!=="),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.matcher.ReturnsSyntax.==="),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.matcher.TypedEqual.==="),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.matcher.TypedEqual.!=="),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.specification.Tables.!=="),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.specification.Tables.==="),

      // Failing path on windows, an additional argument with a default value was added
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.io.DirectoryPath.apply"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.io.DirectoryPath.unsafe"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.io.DirectoryPath.this"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.io.DirectoryPath.copy"),

      // Remove an irrelevant method
      ProblemFilters.exclude[MissingClassProblem]("org.specs2.matcher.ShouldExpectable*"),

      // JsonMatchers #1419
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.matcher.JsonMatchers.*"),
      ProblemFilters.exclude[IncompatibleMethTypeProblem](
        "org.specs2.matcher.JsonMatchersImplicits#given_Conversion_K_V_JsonPairSelector.this"
      ),
      ProblemFilters.exclude[MissingClassProblem]("org.specs2.matcher.JsonMatchersImplicits*"),
      ProblemFilters.exclude[MissingClassProblem](
        "org.specs2.matcher.JsonMatchersLowImplicits$given_ToJsonSelector_Boolean$"
      ),
      ProblemFilters.exclude[MissingClassProblem](
        "org.specs2.matcher.JsonMatchersLowImplicits$given_ToJsonSelector_Double$"
      ),
      ProblemFilters.exclude[MissingClassProblem](
        "org.specs2.matcher.JsonMatchersLowImplicits$given_ToJsonSelector_String$"
      ),
      ProblemFilters.exclude[MissingClassProblem](
        "org.specs2.matcher.JsonMatchersLowImplicits$given_ToJsonSelector_Int$"
      ),

      // Traversable -> Iterable rename in matcher traits and inherited mixins
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.matcher.Matchers"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.matcher.Matchers$"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.matcher.MustMatchers"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.matcher.MustMatchers$"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.matcher.MustThrownMatchers"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.matcher.MustThrownMatchers$"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.matcher.ShouldMatchers"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.matcher.ShouldMatchers$"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.matcher.ShouldThrownMatchers"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.matcher.ShouldThrownMatchers$"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.matcher.ContentMatchers"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.matcher.ContentMatchers$"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.matcher.FilesContentMatchers"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.matcher.JUnitMustMatchers$"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.matcher.JUnitShouldMatchers$"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.matcher.TraversableMatchers"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.matcher.TraversableMatchers$"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.Spec"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.Specification"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.mutable.Spec"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.mutable.Specification"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.mutable.script.Specification"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.specification.script.Spec"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.specification.script.Specification"),
      ProblemFilters.exclude[MissingClassProblem]("org.specs2.matcher.TraversableBaseMatchersLowImplicits*"),
      ProblemFilters.exclude[NewMixinForwarderProblem]("org.specs2.matcher.TraversableBaseMatchers.*"),
      ProblemFilters.exclude[DirectMissingMethodProblem](
        "org.specs2.matcher.TraversableBaseMatchers.containTheSameElementsAs$default$2$"
      ),

      // removed deprecated EitherSyntax.toOption
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.fp.EitherSyntax.toOption"),

      // removed deprecated collection/data/io/text utilities
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.collection.Iterablex.mapFirst"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.data.Trees.size"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.data.Trees.size$default$2"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.io.package./"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.io.package.|"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.text.Regexes.matches"),

      // removed deprecated data utilities (static synthetic default arg method has trailing $)
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.data.Trees.size$default$2$"),

      // SbtEvents and SbtRunners converted from case class to regular class
      // instance methods use # notation; companion object types use $ notation
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.reporter.SbtEvents$SpecSuiteEvent"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.reporter.SbtEvents#SpecSuiteEvent.*"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.reporter.SbtEvents$SpecSuiteEvent$"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.reporter.SbtEvents#SpecSuiteEvent$.apply"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.reporter.SbtEvents#SpecSuiteEvent$.unapply"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.reporter.SbtEvents#SpecSuiteEvent$.fromProduct"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.reporter.SbtEvents$SpecTestEvent"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.reporter.SbtEvents#SpecTestEvent.*"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.reporter.SbtEvents$SpecTestEvent$"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.reporter.SbtEvents#SpecTestEvent$.apply"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.reporter.SbtEvents#SpecTestEvent$.unapply"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.reporter.SbtEvents#SpecTestEvent$.fromProduct"),
      ProblemFilters.exclude[MissingClassProblem]("org.specs2.runner.MasterSbtRunner$"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.runner.MasterSbtRunner"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.runner.MasterSbtRunner.*"),
      ProblemFilters.exclude[MissingClassProblem]("org.specs2.runner.SlaveSbtRunner$"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.runner.SlaveSbtRunner"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.runner.SlaveSbtRunner.*"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("org.specs2.runner.sbtRun.*"),
      ProblemFilters.exclude[MissingTypesProblem]("org.specs2.runner.sbtRun$")
    )
  )

lazy val commonJvmSettings =
  testJvmSettings

import org.scalajs.linker.interface.ESVersion

lazy val commonJsSettings =
  depends.jsMacrotaskExecutor ++
    Seq(scalaJSLinkerConfig ~= { _.withESFeatures(_.withESVersion(ESVersion.ES2018)) }) ++
    testJsSettings ++
    Seq(mimaPreviousArtifacts := Set.empty)

lazy val testNativeSettings = Seq(
  Test / fork := false,
  Test / parallelExecution := false
)

lazy val commonNativeSettings =
  depends.nativeTest ++
    testNativeSettings ++
    Seq(
      mimaPreviousArtifacts := Set.empty,
      libraryDependencySchemes += "org.scala-native" % "test-interface_native0.5_3" % VersionScheme.Always
    )

/** MODULES (sorted in alphabetical order) */

val platforms = List(JVMPlatform, JSPlatform, NativePlatform)
val jvmJsPlatforms = List(JVMPlatform, JSPlatform)
val jvm = JVMPlatform

lazy val common = crossProject(platforms: _*)
  .withoutSuffixFor(jvm)
  .in(file("common"))
  .settings(name := "specs2-common", commonSettings, depends.scalacheckTest)
  .jvmSettings(commonJvmSettings, depends.sbt)
  .jsSettings(commonJsSettings)
  .nativeSettings(commonNativeSettings)
  .dependsOn(fp)

lazy val core = crossProject(platforms: _*)
  .withoutSuffixFor(jvm)
  .in(file("core"))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name := "specs2-core",
    commonSettings,
    // until 5.0.0-RC-23 is published
    // mimaPreviousArtifacts := Set.empty,
    mimaFailOnNoPrevious := false,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.specs2"
  )
  .jvmSettings(commonJvmSettings, depends.junitTest)
  .jsSettings(commonJsSettings)
  .nativeSettings(commonNativeSettings)
  .dependsOn(matcher, common, common % "test->test")

lazy val examples = crossProject(jvmJsPlatforms: _*)
  .withoutSuffixFor(jvm)
  .in(file("examples"))
  .jvmSettings(commonJvmSettings)
  .jsSettings(commonJsSettings)
  // no mima check because that jar is not published
  .settings(
    commonSettings,
    name := "specs2-examples",
    mimaPreviousArtifacts := Set.empty,
    mimaFailOnNoPrevious := false
  )
  .dependsOn(common, matcher, core, matcherExtra, junit, scalacheck)

lazy val fp = crossProject(platforms: _*)
  .withoutSuffixFor(jvm)
  .crossType(CrossType.Pure)
  .in(file("fp"))
  .settings(name := "specs2-fp", commonSettings)
  .jvmSettings(commonJvmSettings)
  .jsSettings(commonJsSettings)
  .nativeSettings(commonNativeSettings)

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

lazy val junit = crossProject(jvmJsPlatforms: _*)
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
  .nativeSettings(commonNativeSettings)
  .dependsOn(common)

lazy val matcherExtra = crossProject(platforms: _*)
  .withoutSuffixFor(jvm)
  .in(file("matcher-extra"))
  .settings(name := "specs2-matcher-extra", commonSettings, depends.scalaParser)
  .jvmSettings(commonJvmSettings)
  .jsSettings(commonJsSettings)
  .nativeSettings(commonNativeSettings)
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
  .nativeSettings(commonNativeSettings)
  .dependsOn(core)

lazy val tests = crossProject(jvmJsPlatforms: _*)
  .withoutSuffixFor(jvm)
  .in(file("tests"))
  .settings(
    commonSettings,
    name := "specs2-tests",
    mimaPreviousArtifacts := Set.empty,
    mimaFailOnNoPrevious := false
  )
  .dependsOn(
    core % "compile->compile;test->test",
    junit % "compile->compile;test->test",
    examples % "test->test",
    matcherExtra
  )
  .jvmConfigure(_.dependsOn(html))
  .jsSettings(scalaJSLinkerConfig ~= { _.withESFeatures(_.withESVersion(ESVersion.ES2018)) })

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
  .nativeSettings(commonNativeSettings)
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
  "-deprecation:true",
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
  Test / javaOptions ++= Seq("-Xmx3G", "-Xss4M") ++ javaSpecs2Properties,
  Test / fork := true
) ++ depends.jvmTest

// extract the specs2 properties as java options arguments
// in order to pass them to a forked jvm for testing
lazy val javaSpecs2Properties: List[String] = {
  new sys.SystemProperties().names.toList.flatMap { case key =>
    if (key.startsWith("specs2")) List(s"-D$key=${System.getProperty(key)}")
    else Nil
  }
}

lazy val testJsSettings = Seq(
  Test / javaOptions := javaSpecs2Properties,
  Test / fork := false,
  Test / parallelExecution := false,
  Test / scalaJSStage := FastOptStage
) ++ depends.jsTest

/** RELEASE
  */
lazy val releaseSettings: Seq[Setting[_]] = Seq(
  ThisBuild / versionScheme := Some("early-semver"),
  ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("25")),
  ThisBuild / githubWorkflowArtifactUpload := false,
  ThisBuild / githubWorkflowBuildPreamble ++= List(
    WorkflowStep.Sbt(List("scalafmtCheckAll"), name = Some("Check formatting ✔"))
  ),
  ThisBuild / githubWorkflowBuild := Seq(
    WorkflowStep
      .Sbt(name = Some("Build and test 🔧"), commands = List("testOnly -- xonly exclude ci,website timefactor 3"))
  ),
  ThisBuild / githubWorkflowAddedJobs ++= Seq(
    WorkflowJob(
      id = "native",
      name = "Test Scala Native",
      scalas = List(Scala3),
      javas = List(JavaSpec.temurin("25")),
      steps =
        List(WorkflowStep.Checkout) ++
          WorkflowStep.SetupJava(List(JavaSpec.temurin("25"))) ++
          List(
            WorkflowStep.SetupSbt(),
            WorkflowStep.Run(
              name = Some("Install native dependencies"),
              commands = List("sudo apt-get update && sudo apt-get install -y clang zlib1g-dev libstdc++-12-dev")
            ),
            WorkflowStep.Sbt(
              name = Some("Test Scala Native 🔧"),
              commands = List(
                "fpNative/test",
                "commonNative/test",
                "matcherNative/test",
                "coreNative/test",
                "matcherExtraNative/test",
                "scalacheckNative/test",
                "xmlNative/test"
              )
            )
          )
    )
  ),
  ThisBuild / githubWorkflowTargetTags ++= Seq(SPECS2 + "*"),
  ThisBuild / githubWorkflowPublishTargetBranches := Seq(RefPredicate.StartsWith(Ref.Tag(SPECS2))),
  ThisBuild / githubWorkflowPublishPreamble ++= List(
    WorkflowStep.Sbt(List("mimaReportBinaryIssues"), name = Some("Check binary compatibility ✔"))
  ),
  ThisBuild / githubWorkflowPublish := Seq(
    WorkflowStep.Run(
      name = Some("Import GPG key 🔑"),
      commands = List(importGpgCommand),
      env = Map(
        "PGP_KEY_ID" -> "${{ secrets.PGP_KEY_ID }}",
        "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
        "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      )
    ),
    WorkflowStep.Sbt(
      name = Some("Release to Sonatype 📇"),
      commands = List("ci-release"),
      env = Map(
        "PGP_KEY_ID" -> "${{ secrets.PGP_KEY_ID }}",
        "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
        "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
        "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
        "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
      )
    ),
    WorkflowStep.Use(
      name = Some("Install Pandoc 🏁"),
      ref = UseRef.Public("r-lib/actions", "setup-pandoc", "v2"),
      params = Map("pandoc-version" -> "latest")
    ),
    WorkflowStep
      .Sbt(
        name = Some("Generate the specs2 website 📚"),
        commands = List("unidoc", "guide/testOnly *Website -- xonly")
      ),
    WorkflowStep.Use(
      name = Some("Update the website 🚀"),
      ref = UseRef.Public("JamesIves", "github-pages-deploy-action", "4.1.4"),
      params = Map("branch" -> "gh-pages", "clean" -> "false", "folder" -> "target/specs2-reports/site")
    )
  ),
  ThisBuild / git.useGitDescribe := true,
  ThisBuild / dynverTagPrefix := SPECS2,
  ThisBuild / git.gitTagToVersionNumber := { tag: String =>
    if (tag matches SPECS2 + ".*") Some(tag.replace(SPECS2, "")) else None
  },
  ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(
    fp.js,
    common.js,
    matcher.js,
    core.js,
    matcherExtra.js,
    junit.js,
    scalacheck.js,
    xml.js,
    examples.js,
    tests.js,
    fp.native,
    common.native,
    matcher.native,
    core.native,
    matcherExtra.native,
    scalacheck.native,
    xml.native,
    guide
  )
)

val importGpgCommand = """echo "$PGP_SECRET" | base64 --decode | gpg --batch --yes --import
printf "pinentry-mode loopback\n" >> ~/.gnupg/gpg.conf
printf "allow-loopback-pinentry\n" >> ~/.gnupg/gpg-agent.conf
gpgconf --kill gpg-agent
gpg --list-secret-keys --keyid-format LONG
gpg --batch --yes -u "$PGP_KEY_ID" --dry-run --pinentry-mode loopback --passphrase "$PGP_PASSPHRASE" --sign <<<"test"
"""

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
    guide,
    markdown,
    junit.jvm,
    scalacheck.jvm,
    xml.jvm,
    examples.jvm
  ),
  inConfigurations(Compile)
)
