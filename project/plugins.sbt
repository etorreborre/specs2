addSbtPlugin("org.scala-js"      % "sbt-scalajs"        % "0.6.18")
addSbtPlugin("com.jsuereth"      % "sbt-pgp"            % "1.0.0")
addSbtPlugin("com.typesafe.sbt"  % "sbt-site"           % "0.6.2")
addSbtPlugin("com.typesafe.sbt"  % "sbt-ghpages"        % "0.5.1")
addSbtPlugin("com.typesafe.sbt"  % "sbt-git"            % "0.8.0")
addSbtPlugin("org.xerial.sbt"    % "sbt-sonatype"       % "0.4.0")
addSbtPlugin("com.orrsella"      % "sbt-stats"          % "1.0.5")
addSbtPlugin("ohnosequences"     % "sbt-github-release" % "0.4.0")

resolvers += "Era7 maven releases" at "https://s3-eu-west-1.amazonaws.com/releases.era7.com"
resolvers += "Jenkins repo" at "http://repo.jenkins-ci.org/public/"
