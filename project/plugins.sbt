addSbtPlugin("org.scala-js"      % "sbt-scalajs"        % "0.6.15")
addSbtPlugin("com.jsuereth"      % "sbt-pgp"            % "1.0.0")
addSbtPlugin("com.typesafe.sbt"  % "sbt-site"           % "0.6.2")
addSbtPlugin("com.typesafe.sbt"  % "sbt-ghpages"        % "0.5.1")
addSbtPlugin("com.typesafe.sbt"  % "sbt-git"            % "0.8.0")
addSbtPlugin("com.github.gseitz" % "sbt-release"        % "0.8.5")
addSbtPlugin("com.typesafe"      % "sbt-mima-plugin"    % "0.1.6")
addSbtPlugin("org.xerial.sbt"    % "sbt-sonatype"       % "0.4.0")
addSbtPlugin("com.orrsella"      % "sbt-stats"          % "1.0.5")
addSbtPlugin("ohnosequences"     % "sbt-github-release" % "0.4.0")
addSbtPlugin("com.ambiata"       % "promulgate"         % "0.11.0-20160104104535-e21b092")

resolvers += Resolver.url("ambiata-oss", new URL("https://ambiata-oss.s3.amazonaws.com"))(Resolver.ivyStylePatterns)
resolvers += "Era7 maven releases" at "https://s3-eu-west-1.amazonaws.com/releases.era7.com"
resolvers += "Jenkins repo" at "http://repo.jenkins-ci.org/public/"

