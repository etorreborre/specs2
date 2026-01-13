val scalaJSVersion =
Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.20.2")
val scalaNativeVersion =
Option(System.getenv("SCALANATIVE_VERSION")).getOrElse("0.5.8")

addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % scalaJSVersion)
addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % scalaNativeVersion)
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.3.2")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("com.github.sbt"     % "sbt-pgp"                       % "2.3.1")
addSbtPlugin("com.github.sbt"     % "sbt-ghpages"                   % "0.9.0")
addSbtPlugin("com.github.sbt"     % "sbt-site"                      % "1.7.0")
addSbtPlugin("org.scoverage"      % "sbt-scoverage"                 % "2.4.4")
addSbtPlugin("com.github.sbt"     % "sbt-git"                       % "2.1.0")
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"                 % "0.13.1")
addSbtPlugin("com.github.sbt"     % "sbt-github-actions"            % "0.29.0")

resolvers += Resolver.url("sonatype", new URL("https://oss.sonatype.org/content/repositories/releases"))(Resolver.ivyStylePatterns)

// needed by sbt-github-release when building on JDK 11.
// reference: https://github.com/ohnosequences/sbt-github-release/issues/28
libraryDependencies += "com.sun.activation" % "javax.activation" % "1.2.0"
