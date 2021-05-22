package org.specs2
package guide

import org.specs2.main.FilesRunnerArguments._

object RunInShell extends UserGuidePage { def is = s2"""
It is not necessary to use a build tool to run a specification. You just need to have the right dependencies on the classpath and use one of the $specs2 runners.

### Dependencies

When you use a build tool you generally only need to specify the main dependencies then any transitive dependency will be fetched for you.
However, when you run specifications from the shell you need to specify the classpath yourself. The best way to do this is to use `sbt` to
make sure your specification compiles with all the required dependencies, then export the project classpath:
```
sbt> export Runtime / fullClasspath
sbt> export Test / fullClasspath
```
With the output of both commands you can define an environment variable, `$$SPECS2_PATH`
```
sh> export $$SPECS2_PATH=<runtime classpath>:<test classpath>
```

### From the shell

#### Run one specification

The `specs2.run` object can be used to run a specification from the shell. The first argument is expected to be the class name
```
sh> java -cp $$SPECS2_PATH specs2.run org.acme.MySpec xonly
```

And the other arguments are used to drive the execution of the specification as described in the ${"Arguments" ~/ ArgumentsReference} section.

#### Run several specifications

The `specs2.files` object will select and execute all specifications found in the test source directory according to the following parameters:

Name                    | Default value              | Description
-----------             | ---------------            | -------------------------
`filesrunner.basepath`  | `$specificationsBasePath`  | source directory for test files
`filesrunner.path`      | `$specificationsPath`      | glob pattern for the file paths
`filesrunner.pattern`   | `$specificationsPattern`   | regular expression capturing the specification class/object name
`filesrunner.verbose`   | `false`                    | logs of searched paths and potential classes to instantiate

Specification arguments can be passed after those parameters
```
sh> java -cp $$SPECS2_PATH specs2.files filesrunner.basepath examples xonly
```

**Tip!** use the `filesrunner.verbose` argument to make sure that you are looking for specifications in the right place

**Tip!** don't forget to quote the `filesrunner.path` argument, since this is a glob and your shell might expand it too soon

```
sh> java -cp $$SPECS2_PATH specs2.files filesrunner.path '**/examples/*Spec.scala'
```

#### Outputs

By default the `specs2.run` and `specs2.files` runners will output their results to the console but you can also use other printers as described in the ${"Runners" ~/ Runners} section.

### From the scala console

The `specs2.run` object also has an `apply` method to execute specifications from the Scala console
```
scala> specs2.run(spec1, spec2)
```
If you want to pass specific arguments you can import the `specs2.arguments` object member functions
```
scala> import specs2.arguments._
scala> specs2.run(spec1)(nocolor)
```
Or you can set implicit arguments which will be used for any specification execution
```
scala> import specs2.arguments._
scala> implicit val myargs = nocolor

scala> specs2.run(spec1)
```
.
"""
}
