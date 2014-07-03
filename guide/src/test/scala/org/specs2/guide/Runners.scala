package org.specs2
package guide

import main.Diffs

object Runners extends UserGuidePage { def is = s2"""
## Presentation

The most common way to run specs2 specifications is to use [sbt](http://scala-sbt.org). In this section we will present the most important options for running specifications

### Via sbt

Sbt recognizes ***specs2*** as a ["test framework"](http://www.scala-sbt.org/release/docs/Testing.html). This means that any class or object extending the `Specification` abstract class can be executed by sbt. The `test` command will run all the specifications in your project provided you put them in the `src/test/scala` directory:
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

Here is a list of all the existing `Printers` in specs2 with links to the corresponding section in the User Guide for more information.

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

 - ${"Filtering" ~ Filtering} to select only some example to run
 - ${"Execution" ~ Execution} to modify the execution parameters
 - ${"Console output" ~ ConsoleOutput}, ${"Html output" ~ HtmlOutput}, ${"Custom output" ~ CustomOutput}... for "reporting" arguments
 - the ${"arguments reference guide" ~ ArgumentsReference} for a list of all arguments

### Output types

## Now learn to...

 * run specifications ${"in an IDE" ~ RunInIDE}: [ScalaIDE](http://scala-ide.org), [IntelliJ IDEA](http://www.jetbrains.com/idea/features/scala.html)
 * output ${"JUnit XML files" ~ JUnitXmlOutput} to run in a continuous integration server like [Jenkins](http://jenkins-ci.org)
 * ${"output HTML files" ~ HtmlOutput}

## And if you want to know more

 * run specifications with ${"another build tool" ~ OtherBuildTools}: maven, gradle
 * run specifications ${"without a build tool" ~ RunInShell}
 * output ${"Markdown files" ~ MarkdownOutput}
 * output ${"PDF files" ~ PdfOutput}
 * use your own reporting tool implementing the ${"`Notifier` interface (simple) or the `Printer` interface" ~ CustomOutput}


### With your own

#### Executor

The `org.specs2.reporter.Executor` trait can be used to change the execution a Specification. This trait defines different methods for the executing a Specification and you can override them:

  * `select` selects the fragments to execute, filtering out some examples based on tags for instance
  * `sequence` groups fragments which can be executed concurrently
  * `execute` executes the fragments
  * `store` stores the results

##### In sbt

You can use a custom `Executor` from inside sbt by passing the `executor` argument with a `Executor` implementation class name:

    sbt> testOnly *BinarySpec* -- executor com.mycompany.reporting.RandomExecutor


#### Notifier

The `org.specs2.reporter.Notifier` trait can be used to report execution events. It notifies of the following:

 * specification start: the beginning of a specification, with its name
 * specification end: the end of a specification, with its name
 * context start: the beginning of a sub-level when the specification is seen as a tree or Fragments
 * context end: the end of a sub-level when the specification is seen as a tree or Fragments
 * text: any Text fragment that needs to be displayed
 * example start
 * example result: success / failure / error / skipped / pending

All those notifications come with a location (to trace back to the originating fragment in the Specification) and a duration when relevant (i.e. for examples and actions).

##### NotifierRunner

The `NotifierRunner` class can be instantiated with a custom `Notifier` and used from the command line.

##### In sbt

You can also use a custom `Notifier` from inside sbt by passing the `notifier` argument with a `Notifier` implementation class name:

```
sbt> testOnly *BinarySpec* -- notifier com.mycompany.reporting.FtpNotifier
```

#### Exporter

The `org.specs2.reporter.Exporter` trait can be used to collect `ExecutedFragments` and report them as desired. The only method to implement is:

    def export(implicit args: Arguments): ExecutingSpecification => ExecutedSpecification

 * `args` is an `Arguments` object created from command line options
 * `ExecutingSpecification` is a list of fragments which might or might not have finished their execution
 * `ExecutedSpecification` must be a list of executed fragments

Please see the Scaladoc API of each trait to see how to use them.

##### In sbt

You can use a custom `Exporter` from inside sbt by passing the `exporter` argument with a `Exporter` implementation class name:

    sbt> testOnly *BinarySpec* -- exporter com.mycompany.reporting.FtpExporter

   - - -

                                                                                                                        """
}