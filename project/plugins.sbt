val scalaJSVersion = Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.19.0")

// dev
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.5")

// ScalaJS
addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")

// release management
addSbtPlugin("com.github.sbt" % "sbt-github-actions" % "0.25.0")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.11.1")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.2")
addSbtPlugin("com.github.sbt" % "sbt-git" % "2.1.0")
addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.5.0")
addSbtPlugin("io.chrisdavenport" % "sbt-mima-version-check" % "0.1.2")

resolvers += Resolver.url("sonatype", new URL("https://oss.sonatype.org/content/repositories/releases"))(
  Resolver.ivyStylePatterns
)
resolvers += Resolver.sonatypeRepo("snapshots")
