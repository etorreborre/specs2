sbt -jvm-opts .travis.jvmopts -sbt-version 1.2.3 -scala-version $TRAVIS_SCALA_VERSION ";set parallelExecution in ThisBuild := false; $1/testOnly -- xonly timefactor 3 neverstore exclude travis"

