sbt -jvm-opts .travis.jvmopts -sbt-version 0.13.15 -scala-version $TRAVIS_SCALA_VERSION "$1/testOnly -- timefactor 3 neverstore exclude travis"
