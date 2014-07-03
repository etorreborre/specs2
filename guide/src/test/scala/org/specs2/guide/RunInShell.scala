package org.specs2
package guide

object RunInShell extends UserGuidePage { def is = s2"""
It is not necessary to use a build tool to run a specification. You just need to have the right dependencies on the classpath and use of of specs2 "runners".

## Dependencies

When you use a build tool you generally only need to specify the main dependencies then the transitive dependencies will be fetched for you. In addition to the scala jars and ${"specs2 jars" ~ Installation} you might need the following jars (in sbt notation):

 Dependency                                                                             | Comment
 -------------------------------------------------------------------------------------- | ---------------------------
 `"org.scalaz" %% "scalaz-core" % "7.0.6"`                                              | mandatory
 `"org.scalaz" %% "scalaz-concurrent" % "7.0.6"`                                        | mandatory
 `"org.scalaz.stream" %% "scalaz-stream" % "0.5.0"`                                     | mandatory
 `"com.chuusai" %% "shapeless" % "2.0.0"`                                               | if you use the GWT trait
 `"org.scalacheck" %% "scalacheck" % "1.11.3"`                                          | if using ScalaCheck
 `"org.mockito" % "mockito-core" % "1.9.5"`                                             | if using Mockito. Note: specs2.jar must be placed before mockito.jar on the classpath
 `"org.hamcrest" % "hamcrest-core" % "1.3"`                                             | if using Hamcrest matchers with Mockito
 `"junit" % "junit" % "4.11"`                                                           | if using JUnit
 `"org.specs2" % "classycle" % "1.4.1"`                                                 | if using the `org.specs2.specification.Analysis` trait
 `"org.scala-lang" % "scala-reflect" % "2.10.3"`                                        | if using interpolated specifications and/or macro matchers
 `compilerPlugin("org.scalamacros" %% "paradise" % "2.0.0-M7" cross CrossVersion.full)` | if using macro matchers
 `"org.scalamacros" %% "quasiquotes" % "2.0.0-M7"`                                      | if using macro matchers

## From the shell

The `specs2.run` object can be used to run a specification from the shell. The first argument is expected to be the class name:
```
home> java -cp ... specs2.run org.acme.MySpec xonly
```

## From the scala console

The `specs2.run` object also has an `apply` method to execute specifications from the Scala console:
```
scala> specs2.run(spec1, spec2)
```
If you want to pass specific arguments you can import the `specs2.arguments` object member functions:
```
scala> import specs2.arguments._
scala> specs2.run(spec1)(nocolor)
```
Or you can set implicit arguments which will be used for any specification execution:

```
scala> import specs2.arguments._
scala> implicit val myargs = nocolor

scala> specs2.run(spec1)
```

## Files Runner

The `specs2.files` object will, by default, select and execute all Specifications found in the test source directory according to the following parameters:

Name                    | Default value       | Description
----------------------- | ------------------- | -----------
`filesrunner.basepath`  | `src/test/scala`    | source directory for test files
`filesrunner.path`      | `**/*.scala`        | glob pattern for the file paths
`filesrunner.pattern`   | `.*Spec`            | regular expression for the specification class/object name

## Outputs

By default the `specs2.run` a `specs2.files` will output their results to the console but you can also use other printers as described in the ${"Runners" ~ Runners} section.

"""
}
