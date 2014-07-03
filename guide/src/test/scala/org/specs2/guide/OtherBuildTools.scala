package org.specs2
package guide

object OtherBuildTools extends UserGuidePage { def is = s2"""
The most straightforward way to run specs2 specifications is to use [sbt](http://scala-sbt.org). However other build tools such as Maven and Gradle can be used too (please refer to the ${"Installation" ~ Installation} guide for instructions on how to set-up projects for those tools).

## Maven

With Maven you need to use the [Surefire](http://maven.apache.org/surefire/maven-surefire-plugin/test-mojo.html) plugin and the `test` command. You will need however to annotate your specification classes as JUnit tests (this requires the `specs2-junit` jar).${snippet{
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class MySpecification extends org.specs2.Specification { def is = s2"""
  Define your specification as usual here
"""
}
}}

## Gradle

For Gradle you need to use the same `RunWith` annotation and the [`test`](http://www.gradle.org/docs/current/dsl/org.gradle.api.tasks.testing.Test.html) task. You can also follow the instructions in [this blog post](http://blog.mindcrime-ilab.de/2013/10/25/gradle-rocking-scala-specs2-tests) and create a task that will leverage the ${"`specs2.file` runner" ~ RunInShell}. This way you will avoid having to annotate the classes. 
"""
}

