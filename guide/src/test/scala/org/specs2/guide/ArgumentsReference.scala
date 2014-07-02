package org.specs2
package guide

object ArgumentsReference extends UserGuidePage { def is = s2"""
## Presentation

Arguments can be passed on the command line, or declared inside a specification, using the `args(name=value)` syntax:${snippet {

class MySpec extends Specification { def is = args(xonly=true) ^ s2"""
  Clever spec title
  And some intro text
  brilliant expectation $success                                   """
}
}}

## Arguments API

From inside a specification, the available arguments are the following.

For selection:

  Name               | Default value  | Description
 ------------------- | -------------- | -------------------------------------------------------------------------------------------
 *`ex`        *      | .*             | regular expression specifying the examples to execute. Use `ex .*brilliant.*` on the command line
 *`include`   *      | ""             | execute only the fragments tagged with any of the comma-separated list of tags: "t1,t2,..."
 *`exclude`   *      | ""             | do not execute the fragments tagged with any of the comma-separated list of tags: "t1,t2,..."
 *`wasIssue`  *      | false          | select only previously failed/error examples
 *`was`       *      | ""             | select only some previously executed examples based on their status
 `specName`          | ".\*Spec"      | regular expression to use when executing specifications with the `FilesRunner`

For execution:

  Name               | Default value                            | Description
 ------------------- | ---------------------------------------- | -------------------------------------------------------------------------------------------
 *`sequential`*      | false                                    | don't execute examples concurrently
 *`plan`      *      | false                                    | only report the text of the specification without executing anything
 *`skipAll`   *      | false                                    | skip all the examples
 *`stopOnFail`*      | false                                    | skip all examples after the first failure or error
 *`stopOnSkip`*      | false                                    | skip all examples after the first skipped result
 *`isolated`*        | false                                    | execute each example in its own specification to get "fresh" local variables
 `random`            | false                                    | execute each example sequentially but in a random order
 `threadsNb`         | `Runtime.getRuntime.availableProcessors` | number of threads to use for concurrent execution
 ***Storing***       |||
 `never`             | false                                    | never store statistics
 `reset`             | false                                    | remove previously stored statistics
 ***Reporting***     |||
 *`xonly`   *        | false                                    | only report failures and errors
 *`showOnly`*        | ""                                       | only report some examples based on their status
 *`color`   *        | true                                     | use colors in the output (`nocolor` can also be used on the command line)
 `pegdownExtensions` | `org.pegdown.Extensions.ALL`             | specifies the extensions used when processing Markdown content with the pegdown library; the value is an `Int` that is determinded by combining the possible values from `org.pegdown.Extensions` with a logical `AND`; note that SMARTS and QUOTES are always disabled for now
 `pegdownTimeout`    | `2000`                                   | Timeout for parsing Markdown text with Pegdown in milliseconds
 `failtrace`         | false                                    | report the stacktrace for failures
 `colors`            | `org.specs2.text.MappedColors`           | define alternative colors (replace failureColor from being yellow to magenta for example)
 `showtimes`         | false                                    | show individual execution times
 `debugMarkdown`     | false                                    | print more information when Markdown formatting fails
 `diffs`             | `SmartDiffs`                             | use a specific algorithm to display differences
 `fromSource`        | true                                     | true takes an AutoExample description from the file, false from the expectation ok message
 `traceFilter`       | `DefaultStackTraceFilter`                | use a StackTraceFilter instance for filtering the reported stacktrace elements
 `checkUrls`         | false                                    | if true, will parse the html files and check that local or http hrefs can be accessed
 `notoc`             | false                                    | if true, will not create a table of contents on the generated html page
 `notifier`          | String                                   | name of a class extending the `org.specs2.reporter.Notifier` trait
 `exporter`          | String                                   | name of a class extending the `org.specs2.reporter.Exporter` trait
 `reporter`          | String                                   | name of a class extending the `org.specs2.reporter.Reporter` trait

##### Most/Least frequently used arguments

Most of the arguments above can be set in a specification with `args(name=value)`. However Scala would not allow the `args` method to accept *all* the possible
arguments as parameters (because a method can only have up to 22 parameters). This is why the least frequently used arguments (not in italics) can be set with an object called `args`, having separate methods for setting all the parameters, by "category". For example: ${snippet{

  args.select(specName = ".*Test", include="slow")
  args.execute(threadsNb = 2)
  args.report(showtimes = true, xonly = true)
}}

##### Shortcuts

There are some available shortcuts for some arguments

 Name                                                                  | Equivalent                                                                            | Description                                                                                      |
 ---------------                                                       | -----------------------                                                               | -----------                                                                                      |
 `include(tags: String)`                                               | `args(include=tags)`                                                                  |                                                                                                  |
 `exclude(tags: String)`                                               | `args(exclude=tags)`                                                                  |                                                                                                  |
 `only(examples: String)`                                              | `args(ex=examples)`                                                                   |                                                                                                  |
 `wasIssue`                                                            | `args(wasIssue=true)`                                                                 |                                                                                                  |
 `was(status: String)`                                                 | `args(was=status)`                                                                    |                                                                                                  |
 `plan`                                                                | `args(plan=true)`                                                                     |                                                                                                  |
 `skipAll`                                                             | `args(skipAll=true)`                                                                  |                                                                                                  |
 `stopOnFail`                                                          | `args(stopOnFail=true)`                                                               |                                                                                                  |
 `stopOnSkip`                                                          | `args(stopOnSkip=true)`                                                               |                                                                                                  |
 `sequential`                                                          | `args(sequential=true)`                                                               |                                                                                                  |
 `isolated`                                                            | `args(isolated=true)`                                                                 |                                                                                                    |
 `xonly`                                                               | `args(xonly=true)`                                                                    |                                                                                                  |
 `showOnly(status: String)`                                            | `args(showOnly=status)`                                                               |                                                                                                |
 `descFromExpectations`                                                | `args.report(fromSource=false)`                                                              | create the example description for the ok message of the expectation instead of the source file  |
 `fullStackTrace`                                                      | `args.report(traceFilter=NoStackTraceFilter)`                                                | the stacktraces are not filtered                                                                 |
 `diffs(show, separators, triggerSize, shortenSize, diffRatio, full)`  | `args.report(diffs=SmartDiffs(show, separators, triggerSize, shortenSize, diffRatio, full))`  | to display the differences when doing equality comparison
"""
}

