package org.specs2
package guide

object Runners extends UserGuidePage { def is = s2"""

## Presentation

The most common way to run $specs2 specifications is to use [sbt](http://scala-sbt.org). In this section we will present the most important options for running specifications

### Via sbt

Sbt recognizes $specs2 as a ["test framework"](http://www.scala-sbt.org/release/docs/Testing.html). This means that any class or object extending the `Specification` abstract class can be executed by sbt. The `test` command will run all the specifications in your project provided you put them in the `src/test/scala` directory:
```
sbt>test
```

Most of the time however you will use the `test-only` command, either because you want to run one specification only or because you want to pass arguments:
```
sbt> test-only org.acme.secret.KillerAppSpec
```

Various sbt options can apply to [test execution in sbt](http://www.scala-sbt.org/release/docs/Testing.html) but here are the ones which you are most likely to use:

 - exclude some specifications:
   `testOptions := Seq(Tests.Filter(s => Seq("Spec", "Unit").exists(s.endsWith)))`

 - don't execute the specifications in parallel
   `parallelExecution in Test := false`

 - pass some arguments to all specifications
   `testOptions in Test += Tests.Argument("nocolor", "neverstore")`
   `testOptions in Test += Tests.Argument("exclude", "integration")`

 - display results as soon as they've been executed
   `logBuffered := false`

### Output

When you run a specification, whatever environment you are in: sbt, shell, IDE,... you can specify different outputs for the results. For example in sbt by default the results appear in the console but if you want JUnit XML files to be produced you can just add the `junitxml` argument. Adding any "Output" argument will deactivate the console (you will see no output in the console) but you can enable it again by passing the `console` argument. You can of course specify several outputs like `html junitxml console`.

Here is a list of all the existing `Printers` in $specs2 with links to the corresponding section in the User Guide for more information.

 Argument   | Section
 ---------- | --------
 `console`  | $ConsoleOutput
 `junitxml` | $JUnitXmlOutput
 `html`     | $HtmlOutput
 `markdown` | $MarkdownOutput
 `pdf`      | $PdfOutput
 `notifier` | $CustomOutput
 `printer`  | $CustomOutput

### Arguments

With the `test-only` command arguments can be passed on the command line for selecting, executing or reporting a specification. Please consult the following sections for more information:

 - ${"Filtering" ~/ Selection} to select only some example to run
 - ${"Execution" ~/ Execution} to modify the execution parameters
 - ${"Console output" ~/ ConsoleOutput}, ${"Html output" ~/ HtmlOutput}, ${"Custom output" ~/ CustomOutput}... for "reporting" arguments
 - the ${"arguments reference guide" ~/ ArgumentsReference} for a list of all arguments

### Output types

## Now learn to...

 * run specifications ${"in an IDE" ~/ RunInIDE}: [ScalaIDE](http://scala-ide.org), [IntelliJ IDEA](http://www.jetbrains.com/idea/features/scala.html)
 * output ${"JUnit XML files" ~/ JUnitXmlOutput} to run in a continuous integration server like [Jenkins](http://jenkins-ci.org)
 * ${"output HTML files" ~/ HtmlOutput}

## And if you want to know more

 * run specifications with ${"another build tool" ~/ OtherBuildTools}: maven, gradle
 * run specifications ${"without a build tool" ~/ RunInShell}
 * output ${"Markdown files" ~/ MarkdownOutput}
 * output ${"PDF files" ~/ PdfOutput}
 * use your own reporting tool implementing the ${"`Notifier` interface (simple) or the `Printer` interface" ~/ CustomOutput}
                                                                                                                        """
}