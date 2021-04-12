package org.specs2
package guide

object Runners extends UserGuidePage { def is = s2"""

The most common way to run $specs2 specifications is to use [sbt](http://scala-sbt.org). In this section we will present the most important options for running specifications

### Via sbt

[Sbt](www.scala-sbt.org) recognizes $specs2 as a ["test framework"](http://www.scala-sbt.org/release/docs/Testing.html). This means that any class or object extending the `Specification` abstract class can be executed by sbt. The `test` command will run all the specifications in your project provided you put them in the `src/test/scala` directory:
```
sbt> test
```

Most of the time however you will use the `testOnly` command, either because you want to run one specification only or because you want to pass arguments:
```
sbt> testOnly org.acme.secret.KillerAppSpec
```

Only show failed tests:
```
sbt> testOnly org.acme.secret.KillerAppSpec -- xonly
```

#### sbt options

Various sbt options can apply to [test execution in sbt](http://www.scala-sbt.org/release/docs/Testing.html) but here are the ones which you are most likely to use:

 - exclude some specifications:
   `testOptions := Seq(Tests.Filter(s => Seq("Spec", "Unit").exists(s.endsWith)))`

 - don't execute the specifications in parallel
   `parallelExecution in Test := false`

 - pass $specs2 arguments to all specifications
   `testOptions in Test += Tests.Argument("exclude", "integration")`

 - display results as soon as they've been executed
   `logBuffered := false`

 - [custom tags](http://www.scala-sbt.org/0.13/docs/Parallel-Execution.html#Tagging+Tasks) to restrict the parallel execution of specifications.
     *** if you want to use sbt tags you will also need to pass the `sbt.tags` argument on the command-line ***

### Output

When you run a specification, whatever environment you are in: sbt, shell, IDE,... you can specify different outputs for the results. For example, when you execute a specification with sbt, the results appear in the console. If you want JUnit XML files to be produced instead you need to pass the `junitxml` argument. Adding any "output" argument will deactivate the console (you will see no output in the console) but you can enable it again by passing the `console` argument. You can of course specify several outputs like `html junitxml console`.

Here is a list of all the existing `Printers` in $specs2 with links to the corresponding section in the User Guide for more information.

<div markdown='1'>
 Argument   | Section
 ---------- | -----------------------
 `console`  | ${link(ConsoleOutput).mute}
 `junitxml` | ${link(JUnitXmlOutput).mute}
 `html`     | ${link(HtmlOutput).mute}
 `markdown` | ${link(MarkdownOutput).mute}
 `notifier` | ${link(CustomOutput).mute}
 `printer`  | ${link(CustomOutput).mute}
</div>

### Arguments

With the `testOnly` command arguments can be passed on the command line for selecting, executing or reporting a specification. Please consult the following sections for more information:

 - ${("Filtering" ~ Selection).mute} to select only some example to run
 - ${("Execution" ~ Execution).mute} to modify the execution parameters
 - ${"Console output" ~/ ConsoleOutput}, ${"Html output" ~/ HtmlOutput}, ${"Custom output" ~/ CustomOutput}... for "reporting" arguments
 - the ${"arguments reference guide" ~/ ArgumentsReference} for a list of all arguments


$NowLearnTo

 * run specifications ${("in an IDE" ~ RunInIDE).mute}: [IntelliJ IDEA](http://www.jetbrains.com/idea/features/scala.html), [ScalaIDE](http://scala-ide.org)
 * output ${"JUnit XML files" ~/ JUnitXmlOutput} to run in a continuous integration server like [Jenkins](http://jenkins-ci.org)
 * ${"output HTML files" ~/ HtmlOutput}

$vid

$AndIfYouWantToKnowMore

 * run specifications with ${"another build tool" ~/ OtherBuildTools}: maven, gradle
 * run specifications ${("without a build tool" ~ RunInShell).mute}
 * output ${"Markdown files" ~/ MarkdownOutput}
 * use your ${"own reporting tool" ~/ CustomOutput} by implementing the `Notifier` interface (simple) or the `Printer` interface
 * execute ${"referenced specifications" ~/ ReferenceOtherSpecifications} with the `all` argument

$vid
"""
}
