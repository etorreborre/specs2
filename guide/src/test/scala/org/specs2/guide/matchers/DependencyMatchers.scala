package org.specs2
package guide
package matchers

import form.Card

object DependencyMatchers extends UserGuideCard with specification.Analysis with UserGuideVariables {
  def title = "Dependency Matchers"
  def text = s2"""
It is highly desirable to have acyclic dependencies between the packages of a project. This often leads to describing the packages structure as "layered": each package on a layer can only depend on a package on a lower layer. **_specs2_** helps you enforce this design property with specific matchers.

**Layers definition**

First you need to define the packages and their expected dependencies. Mix-in the `${fullName[specification.Analysis]}` trait and define, (taking $specs2 as an example): ${snippet{

layers (
  "runner",
  "reporter",
  "specification mutable",
  "mock      form",
  "matcher",
  "execute",
  "reflect    xml  time html",
  "collection control io text main data").withPrefix("org.specs2")
}}

The above expression defines layers as an ordered list of `String`s containing space-separated package names. It is supplemented by a `withPrefix` declaration to factor out the common package prefix between all these packages.

By default, the packages are supposed to correspond to directories in the `src/target/scala-<version>/classes` directory. If your project has a different layout you can declare another target directory: ${snippet{

layers("...").inTargetDir("out/classes")
}}

**Inclusion/Exclusion**

Every rule has exceptions :-). In some rare cases, it might be desirable to exclude a class from being checked on a given layer. To do this, you can use the `include/exclude` methods on the `Layer` class: ${snippet{

layers (
  "runner",
  "reporter",
  "specification mutable".exclude("mutable.SpecificationWithJUnit"),
  "mock      form",
  "matcher",
  "execute",
  "reflect  xml  time html",
  "collection control io text main data").withPrefix("org.specs2")
}}

The `include/exclude` methods accept a list of regular expressions to:

- exclude fully qualified class names (generally, only `exclude` will be necessary)
- re-include fully qualified class names if the exclusion list is to big

***Verification***

Now you've defined layers, you can use the `beRespected` matcher to check if all the dependencies are verified: ${snippet{

val design = layers("...")
design must beRespected
}}

If some dependencies are not respected:

```
those dependencies are not satisfied:
org.specs2.main x-> org.specs2.io because org.specs2.io.FileSystem -> org.specs2.main.Arguments
org.specs2.main x-> org.specs2.io because org.specs2.io.FileSystem -> org.specs2.main.ArgumentsArgs
```

***Layers as an `Example`***

The `${fullName[specification.Analysis]}` trait allows to directly embed the layers definition in a `Specification` and turn it into an `Example`: ${snippet{

class DependenciesSpec extends Specification with specification.Analysis { def is =
  "this is the application design" ^
    layers(
      "gui commandline",
      "controller",
      "backend"
    )
}
}}

***Alternative implementation***

Another implementation of the same functionality is available through the `org.specs2.analysis.CompilerDependencyFinder` trait. This implementation uses the compiler dependency analysis functionality but needs more time, since it recompiles the sources.

The source files are taken from the `src/main/scala` directory by default but you can change this value by using the `Layers.inSourceDir` method.

While this implementation is slower than the Classycle one, it might retrieve more dependencies, for example when constants are inlined in class files.

Note: since this functionality relies on the scala compiler library, so you need to add it to your build file:

```
// use sbt's scalaVersion Setting to define the scala-compiler library version
libraryDependencies <<= scalaVersion { scala_version => Seq(
  "org.specs2" %% "specs2" % $VERSION % "test",
  "org.scala-lang" % "scala-compiler" % scala_version % "test")
}
```
"""
}
