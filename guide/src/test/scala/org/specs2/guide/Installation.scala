package org.specs2
package guide

object Installation extends UserGuidePage { def is = s2"""

There are 3 preferred ways to install ***specs2***:

 - [sbt](http://scala-sbt.org)
 - [maven](http://maven.apache.org)
 - [gradle](http://gradle.org)

## SBT

First you need to [install sbt itself](http://www.scala-sbt.org/release/tutorial/Setup.html) then you need to add the following dependency:
```
libraryDependencies += "org.specs2" %% "specs2-core" % $VERSION % "test"
```

See [here](http://www.scala-sbt.org/release/tutorial/Library-Dependencies.html) to learn more about sbt dependencies.

## Maven

You can install Maven from [there](http://maven.apache.org/guides/getting-started/maven-in-five-minutes.html). Once installed, you need to create a `pom.xml` file with the [`maven-scala-plugin`](http://davidb.github.io/scala-maven-plugin/plugin-info.html). In the `pom.xml` file you can add the following dependency:
```
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  ...
<dependencies>
  <dependency>
    <groupId>org.specs2</groupId>
    <artifactId>specs2-core_2.11</artifactId>
    <version>$VERSION</version>
    <scope>test</scope>
  </dependency>
</dependencies>
  ...
</project>
```

## Gradle

Go to this [page](http://www.gradle.org/installation) to install Gradle. You then need to install the [Scala plugin](http://www.gradle.org/docs/current/userguide/scala_plugin.html) and add to your `build.gradle` file:
```
repositories {
  maven {
     url "https://oss.sonatype.org/content/repositories/releases"
  }
}

dependencies {
  testCompile "org.specs2:specs2-core_2.11:$VERSION"
}
```

"""
}
