sbt -jvm-opts .travis.jvmopts -scala-version $TRAVIS_SCALA_VERSION ";set parallelExecution in ThisBuild := false; $1/testOnly $2 -- xonly timefactor 3 neverstore exclude travis"
