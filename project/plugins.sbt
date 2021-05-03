val scalaJSVersion =
Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.5.1")
val scalaNativeVersion =
Option(System.getenv("SCALANATIVE_VERSION")).getOrElse("0.4.0")

addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % scalaJSVersion)
addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % scalaNativeVersion)
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.0.0")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.0.0")
addSbtPlugin("io.crashbox"        % "sbt-gpg"                       % "0.2.1")
addSbtPlugin("com.typesafe.sbt"   % "sbt-ghpages"                   % "0.6.3")
addSbtPlugin("com.typesafe.sbt"   % "sbt-site"                      % "1.4.1")
addSbtPlugin("org.scoverage"      % "sbt-scoverage"                 % "1.7.2")
addSbtPlugin("com.typesafe.sbt"   % "sbt-git"                       % "1.0.0")
addSbtPlugin("org.xerial.sbt"     % "sbt-sonatype"                  % "3.9.7")
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"                 % "0.9.0")
addSbtPlugin("com.codecommit"     % "sbt-github-actions"            % "0.10.1")

resolvers += Resolver.url("sonatype", new URL("https://oss.sonatype.org/content/repositories/releases"))(Resolver.ivyStylePatterns)

// needed by sbt-github-release when building on JDK 11.
// reference: https://github.com/ohnosequences/sbt-github-release/issues/28
libraryDependencies += "com.sun.activation" % "javax.activation" % "1.2.0"
