val scalaJSVersion = Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.5.1")
val scalaNativeVersion = Option(System.getenv("SCALANATIVE_VERSION")).getOrElse("0.4.1-SNAPSHOT")

// compilation
//addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % scalaJSVersion)
// testing
//addSbtPlugin("org.scoverage"      % "sbt-scoverage"                 % "1.6.1")
// release management
addSbtPlugin("com.codecommit"     % "sbt-github-actions"            % "0.12.0")
addSbtPlugin("com.geirsson"       % "sbt-ci-release"                % "1.5.7")
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"                 % "0.10.0")
addSbtPlugin("com.eed3si9n"       % "sbt-unidoc"                    % "0.4.3")

resolvers += Resolver.url("sonatype", new URL("https://oss.sonatype.org/content/repositories/releases"))(Resolver.ivyStylePatterns)
resolvers += Resolver.sonatypeRepo("snapshots")
