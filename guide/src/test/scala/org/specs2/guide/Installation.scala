package org.specs2
package guide

object Installation extends UserGuidePage { def is = s2"""

The recommended way to install $specs2 is via [sbt](http://scala-sbt.org).

### SBT

First you need to [install sbt itself](https://www.scala-sbt.org/release/docs/Setup.html) then you need to add the specs2 dependency:
```
libraryDependencies += "org.specs2" %% "specs2-core" % "$VERSION" % "test"

// if you want to use ScalaJS you need to use %%%
libraryDependencies += "org.specs2" %%% "specs2-core" % "$VERSION" % "test"
```

See [here](https://www.scala-sbt.org/release/docs/Library-Dependencies.html) to learn more about sbt dependencies.

### Other dependencies

Depending on the $specs2 features you want to use you will need to add more dependencies to your build:

 Name                    | Functionality
 ----------------------- | ----------------------------
 `specs2-matcher-extra`  | for the optional $specs2 matchers
 `specs2-html`           | to export specifications as html
 `specs2-form`           | to create html form-like specifications (experimental)
 `specs2-junit`          | to run specifications as JUnit tests
 `scalamock`             | to use mocks in specifications (see the [ScalaMock project](https://scalamock.org))
 `specs2-scalacheck`     | to use ScalaCheck properties in specifications (see the [ScalaCheck project](https://github.com/typelevel/scalacheck))
 `specs2-cats`           | for the [cats](https://github.com/typelevel/cats) matchers (see [specs2-cats](https://github.com/etorreborre/specs2-cats))
 `specs2-scalaz`         | for the [scalaz](https://github.com/scalaz/scalaz) matchers (see [specs2-scalaz](https://github.com/etorreborre/specs2-scalaz))

Note: the `specs2-core` jar depends on 3 other $specs2 jars:

 Name                    | Functionality
 ----------------------- | ----------------------------
 `specs2-fp`             | utility classes for functional programming
 `specs2-common`         | utility classes for text, collections, xml,...
 `specs2-matcher`        | common $specs2 matchers. They can be used as a stand-alone library with [JUnit](http://junit.org)

"""
}
