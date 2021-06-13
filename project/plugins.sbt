val scalaJSVersion = Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.5.1")
val scalaNativeVersion = Option(System.getenv("SCALANATIVE_VERSION")).getOrElse("0.4.1-SNAPSHOT")

addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % scalaJSVersion)
addSbtPlugin("org.scalameta"      % "sbt-scalafmt"                  % "2.4.0")
addSbtPlugin("com.jsuereth"       % "sbt-pgp"                       % "2.0.1")
addSbtPlugin("com.typesafe.sbt"   % "sbt-ghpages"                   % "0.6.3")
addSbtPlugin("com.typesafe.sbt"   % "sbt-site"                      % "1.4.0")
addSbtPlugin("org.scoverage"      % "sbt-scoverage"                 % "1.6.1")
addSbtPlugin("com.typesafe.sbt"   % "sbt-git"                       % "1.0.0")
addSbtPlugin("org.xerial.sbt"     % "sbt-sonatype"                  % "3.8.1")
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"                 % "0.10.0")
addSbtPlugin("com.codecommit"     % "sbt-github-actions"            % "0.12.0")

resolvers += Resolver.url("sonatype", new URL("https://oss.sonatype.org/content/repositories/releases"))(Resolver.ivyStylePatterns)

// needed by sbt-github-release when building on JDK 11.
// reference: https://github.com/ohnosequences/sbt-github-release/issues/28
libraryDependencies += "com.sun.activation" % "javax.activation" % "1.2.0"

resolvers += Resolver.sonatypeRepo("snapshots")
//addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.4.0-RC1-192-72a856b6")
