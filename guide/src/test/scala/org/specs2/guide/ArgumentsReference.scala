package org.specs2
package guide

import org.specs2.main.FilesRunnerArguments._

object ArgumentsReference extends UserGuidePage { def is = "Arguments reference".title ^ s2"""

Arguments can be passed on the command line, or declared inside a specification, using the `args(name=value)` syntax:${snippet {

class MySpec extends Specification { def is = args(xonly=true) ^ s2"""
  Clever spec title
  And some intro text
  brilliant expectation $success                                   """
}
}}

They can also be passed as system properties: `-Dspecs2.name=value` (`-Dname=value` also works but you might have collisions with other properties).

Note that boolean properties like `color` can be defined in several ways:

 - not defined => the default value, `true`, is used
 - `color`     => use colors
 - `color true` => use colors
 - `color false` => don't use colors (`nocolor` is an alias)
 - `-Dspecs2.color`     => use colors
 - `-Dspecs2.color=true` => use colors
 - `-Dspecs2.color=false` => don't use colors (`-Dspecs2.nocolor` is an alias)

This reference guide for arguments is divided in several sections:

 - "path" arguments defining where $specs2 should read or write files
 - "selection" arguments to select the examples to run
 - "execution" arguments for execution parameters
 - "storing" arguments to store statistics
 - "reporting" arguments to control the output
 - API for arguments when used in the code

**Tip!** the `verbose` argument will report any unknown argument passed on the command line

## Paths

 Name                    | Default value                  | Description
 ----------------------- | ------------------------------ | -------------------------------------------------------------------------------------------
 `stats.outdir`          | `target/specs2-reports/stats`  | output directory for run statistics (see $Selection)
 `junit.outdir`          | `target/test-reports/`         | output directory for JUnit XML files (see $JUnitXmlOutput)
 `html.outdir`           | `target/specs2-reports`        | output directory for html files (see $HtmlOutput)
 `filesrunner.basepath`  | `$specificationsBasePath`      | source directory for test files (see $RunInShell)
 `filesrunner.path`      | `$specificationsPath`          | glob pattern for the file paths (see $RunInShell)
 `filesrunner.pattern`   | `$specificationsPattern`       | regular expression capturing the specification class/object name (see $RunInShell)
 `filesrunner.verbose`   | `false`                        | logs of searched paths and potential classes to instantiate (see $RunInShell)

## Selection

 Name                    | Default value                  | Description
 ----------------------- | ------------------------------ | -------------------------------------------------------------------------------------------
 `ex`                    | `.*`                           | regular expression specifying the examples to execute. Use `ex .*brilliant.*` on the command line
 `was`                   | `""`                           | select only some previously executed examples based on their status
 `include`               | `""`                           | execute only the fragments tagged with any of the comma-separated list of tags: `t1,t2,...`
 `exclude`               | `""`                           | do not execute the fragments tagged with any of the comma-separated list of tags: `t1,t2,...`
 `selector`              | `""`                           | implementation of the `org.specs2.specification.process.Selector` trait

## Execution

 Name                    | Default value                                    | Description
 ----------------------- | ----------------------------------------         | -------------------------------------------------------------------------------------------
 `plan`                  | `false`                                          | only report the text of the specification without executing anything
 `skipAll`               | `false`                                          | skip all the examples
 `skipAllIf(condition)`  |                                                  | skip all the examples if a given condition is met
 `skipAllUnless(condition)` |                                               | skip all the examples unless a given condition is met
 `stopOnFail`            | `false`                                          | skip all examples after the first failure
 `stopOnError`           | `false`                                          | skip all examples after the first error
 `stopOnIssue`           | `false`                                          | skip all examples after the first failure or error
 `stopOnSkip`            | `false`                                          | skip all examples after the first skipped result
 `sequential`            | `false`                                          | don't execute examples concurrently
 `asap`                  | `false`                                          | execute all the examples before reporting them
 `isolated`              | `false`                                          | execute each example in its own specification to get "fresh" local variables
 `useCustomClassLoader`  | `false`                                          | use a custom class loader set on the `Env` to execute each example. By default sbt's classloader for running tests is attached to the `Env`
 `threadsNb`             | `min(Runtime.getRuntime.availableProcessors, 4)` | number of threads to use for concurrent execution inside the user examples
 `specs2ThreadsNb`       | `min(Runtime.getRuntime.availableProcessors, 4)` | number of threads to use for the concurrent execution of specs2 actions
 `scheduledThreadsNb`    | `1`                                              | number of threads to use for timing out execution
 `batchSize`             | `min(Runtime.getRuntime.availableProcessors, 4)` | number of examples which start executing concurrently at the time
 `timeFactor`            | `1`                                              | multiply all durations by this factor when waiting for futures
 `executor`              | `""`                                             | implementation of the `org.specs2.specification.process.Executor` trait


## Storing

 Name                    | Default value                  | Description
 ----------------------- | ------------------------------ | ------------------------------
 `neverstore`            | `false`                        | never store statistics if true
 `resetstore`            | `false`                        | delete previous store statistics if true

## Reporting

See the $ConsoleOutput page for a more precise description of these options.

 Name                    | Default value                  | Description
 ----------------------- | ------------------------------ | ------------------------------
 `all`                   | `false`                        | execute and report linked specifications
 `xonly`                 | `false`                        | only report failures and errors
 `showonly`              | `""`                           | only report some examples based on their status
 `failtrace`             | `false`                        | show a stack trace for failures
 `fullstacktrace`        | `false`                        | show a full stack trace
 `tracefilter`           | `""`                           | comma-separated include patterns separated by `/` with exclude patterns
 `smartdiffs`            | `""`                           | define the parameters for the `SmartDiffs` instance for differences
 `diffsclass`            | `""`                           | use a specific instance of the `Diffs` trait
 `color`                 | `true`                         | use colors
 `colors`                | `""`                           | use different colors
 `colorsclass`           | `""`                           | use a specific instance of the `Colors` trait
 `showtimes`             | `false`                        | show execution times for examples
 `notifier`              | `""`                           | name of a class extending the `org.specs2.reporter.Notifier` trait
 `printer`               | `""`                           | name of a class extending the `org.specs2.reporter.Printer` trait

For ${"the HTML output" ~/ HtmlOutput} the following options can be used:

 Name                    | Default value                                 | Description
 ----------------------- | --------------------------------------------- | ------------------------------
 `all`                   | `false`                                       | execute and report linked specifications
 `html.outdir`           | `target/specs2-reports/`                      | output directory for the html files
 `html.template`         | `target/specs2-reports/templates/specs2.html` | copied from the `resources/templates` directory
 `html.variables`        | `Map[String, String]()`                       | passed to the template during the Pandoc evaluation
 `html.nostats`          | `false`                                       | if true no stats are displayed
 `html.search`           | `false`                                       | add a search box to the generated files
 `html.toc`              | `false`                                       | add a table of contents to the generated files
 `html.toc.entrymaxsize` | `18`                                          | maximum number of characters for an entry in the table of contents
 `html.warn.missingrefs` | `true`                                        | report "see" references which do not correspond to any generated file

## Arguments API

From inside a specification, the `args` method provides the most frequent arguments as `args(argumentName = argumentValue)`. In the least frequent cases you will have to write:${snippet {
// for selection arguments
args.select(ex = "example \\d*")

// for reporting arguments
args.execute(threadsNb = 4)

// for storing arguments
args.store(reset = true)

// for reporting arguments
args.report(xonly = true)
}}

There are also a few shortcuts:

 Name                                                                  | Equivalent
 --------------------------------------------------------------------- | -----------------------
 `include(tags: String)`                                               | `args(include=tags)`
 `exclude(tags: String)`                                               | `args(exclude=tags)`
 `only(examples: String)`                                              | `args(ex=examples)`
 `was(status: String)`                                                 | `args(was=status)`
 `plan`                                                                | `args(plan=true)`
 `skipAll`                                                             | `args(skipAll=true)`
 `skipAllIf(condition)`                                                | `args(skipAll=condition)`
 `skipAllUnless(condition)`                                            | `args(skipAll=!condition)`
 `stopOnFail`                                                          | `args(stopOnFail=true)`
 `stopOnError`                                                         | `args(stopOnError=true)`
 `stopOnIssue`                                                         | `args(stopOnIssue=true)`
 `stopOnSkip`                                                          | `args(stopOnSkip=true)`
 `sequential`                                                          | `args(sequential=true)`
 `isolated`                                                            | `args(isolated=true)`
 `xonly`                                                               | `args(xonly=true)`
 `showOnly(status: String)`                                            | `args(showOnly=status)`
 `fullStackTrace`                                                      | `args.report(traceFilter=NoStackTraceFilter)`
 `diffs(show, separators, triggerSize, shortenSize, diffRatio, full)`  | `args.report(diffs=SmartDiffs(show, separators, triggerSize, shortenSize, diffRatio, full))`
"""

}
