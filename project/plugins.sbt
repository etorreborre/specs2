val scalaJSVersion = Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.7.1")

// dev
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.3")

// ScalaJS
addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.1.0")

// release management
addSbtPlugin("com.codecommit" % "sbt-github-actions" % "0.13.0")
addSbtPlugin("com.geirsson" % "sbt-ci-release" % "1.5.7")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.10.0")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.0.1")
addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.2")
// addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.3")
lazy val root = (project in file(".")).dependsOn(unidocPlugin)
lazy val unidocPlugin = RootProject(uri("git://github.com/pikinier20/sbt-unidoc.git#de031db4dbac927a76e52a1bf357c10f54fef551"))

resolvers += Resolver.url("sonatype", new URL("https://oss.sonatype.org/content/repositories/releases"))(
  Resolver.ivyStylePatterns
)
resolvers += Resolver.sonatypeRepo("snapshots")
