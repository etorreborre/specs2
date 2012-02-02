package org.specs2
package guide

class Runners extends Specification { def is = noindent ^
  "Runners guide".title ^
                                                                                                                        """
### Presentation

There are many ways to execute ***specs2*** specifications:

 * on the command line, with a console output, and the `specs2.run` runner
 * on the command line, with a html output, and the `specs2.html` runner
 * on the command line, with a console or a html output, and the `specs2.files` runner
 * using [sbt](http://code.google.com/p/simple-build-tool)
 * using [JUnit](http://www.junit.org)
 * using [Intellij IDEA](http://confluence.jetbrains.net/display/SCA/Scala+Plugin+for+IntelliJ+IDEA)
 * using your own reporting tool implementing the `Notifier` interface


### Dependencies

 ***specs2*** is available for Scala 2.9.0 onwards and uses the following libraries, as specified using the [sbt dsl](http://code.google.com/p/simple-build-tool/wiki/LibraryManagement#Basic_Dependencies):

 <table class="dataTable"><tr><th>Dependency</th><th>Comment</th></tr><tr><td class="info">`"org.specs2" %% "specs2-scalaz-core" % "6.0.1"`</td><td class="info">mandatory. This jar bundles the scalaz classes but renamed as `org.specs2.internal.scalaz._`.</td></tr><tr><td class="info"> `"org.scala-tools.testing" %% "scalacheck" % "1.9"`</td><td class="info">only if using ScalaCheck</td></tr><tr><td class="info">`"org.mockito" % "mockito-all" % "1.9.0"`</td><td class="info">only if using Mockito</td></tr><tr><td class="info">`"org.hamcrest" % "hamcrest-all" % "1.1"`</td><td class="info">only if using Hamcrest matchers with Mockito</td></tr><tr><td class="info">`"junit" % "junit" % "4.7"`</td><td class="info">only if using JUnit</td></tr><tr><td class="info">`"org.scala-tools.testing" % "test-interface" % "0.5"`</td><td class="info">provided by sbt when using it</td></tr><tr><td class="info">`"org.pegdown" % "pegdown" % "1.0.2"`</td><td class="info">only if using the html runner</td></tr><tr><td class="info">`"org.specs2" % "classycle_2.9.1" % "1.4"`</td><td class="info">only if using the `org.specs2.specification.Analysis` trait</td></tr><tr><td class="info">`"org.scala-lang" % "scala-compiler" % "2.9.1"`</td><td class="info">only if using the `org.specs2.specification.Analysis` trait with the `CompilerDependencyFinder` trait</td></tr></table>

**Note**: there are versions of specs2 available for Scala 2.8.1 but they [miss some "context" functionalities](org.specs2.guide.SpecStructure.html#In+a+mutable+specification).

### Arguments

You can specify arguments which will control the execution and reporting. They can be passed on the command line, or declared inside the specification, using the `args(name=value)` syntax:

      class MySpec extends Specification { def is = args(xonly=true)    ^
        "Clever spec title"                                             ^
        "this will not be indented"                                     ^
        "brilliant expectation"                                         ! success
      }

#### In a Specification

From inside a specification, the available arguments are the following:

  Name           | Default value                            | Description
 --------------- | ---------------------------------------- | -------------------------------------------------------------------------------------------
 ***Selection*** |||
 *`ex`        *  | .*                                       | regular expression specifying the examples to execute. Use `ex .*brilliant.*` on the command line
 *`include`   *  | ""                                       | execute only the fragments tagged with any of the comma-separated list of tags: "t1,t2,..."
 *`exclude`   *  | ""                                       | do not execute the fragments tagged with any of the comma-separated list of tags: "t1,t2,..."
 *`wasIssue`  *  | false                                    | select only previously failed/error examples
 *`was`       *  | ""                                       | select only some previously executed examples based on their status
 `specName`      | ".*Spec"                                 | regular expression to use when executing specifications with the FilesRunner
 ***Execution*** |||
 *`plan`      *  | false                                    | only report the text of the specification without executing anything
 *`skipAll`   *  | false                                    | skip all the examples
 *`stopOnFail`*  | false                                    | skip all examples after the first failure or error
 *`sequential`*  | false                                    | don't execute examples concurrently
 `threadsNb`     | `Runtime.getRuntime.availableProcessors` | number of threads to use for concurrent execution
 ***Storing***   |||
 `never`         | false                                    | never store statistics
 `reset`         | false                                    | remove previously stored statistics
 ***Reporting*** |||
 *`xonly`   *    | false                                    | only report failures and errors
 *`showOnly`*    | ""                                       | only report some examples based on their status
 *`color`   *    | true                                     | use colors in the output (`nocolor` can also be used on the command line)
 *`noindent`*    | false                                    | don't indent automatically text and examples
 *`markdown`*    | true                                     | interpret text as Markdown in the html reporter
 `failtrace`     | false                                    | report the stacktrace for failures
 `colors`        | `org.specs2.text.SmartColors`            | define alternative colors (replace failureColor from being yellow to magenta for example)
 `showtimes`     | false                                    | show individual execution times
 `debugMarkdown` | false                                    | print more information when Markdown formatting fails
 `diffs`         | `SmartDiffs`                             | use a specific algorithm to display differences
 `fromSource`    | true                                     | true takes an AutoExample description from the file, false from the expectation ok message
 `traceFilter`   | `DefaultStackTraceFilter`                | use a StackTraceFilter instance for filtering the reported stacktrace elements

##### Most/Least frequently used arguments

Most of the arguments above can be set in a specification with `args(name=value)`. However Scala would not allow the `args` method to accept *all* the possible
arguments as parameters (because a method can only have up to 22 parameters). This is why the least frequently used arguments (not in italics) can be set with an object called `args`, having separate methods for setting all the parameters, by "category". For example:

      args.select(specName = ".*Test", include="slow")
      args.execute(threadsNb = 2)
      args.report(showtimes = true, xonly = true)


##### Shortcuts

There are some available shortcuts for some arguments

 Name                                                                  | Equivalent                                                                            | Description                                                                                      |
 ---------------                                                       | -----------------------                                                               | -----------                                                                                      |
 `include(tags: String)`                                               | `args(include=tags)`                                                                  |                                                                                                  |
 `exclude(tags: String)`                                               | `args(exclude=tags)`                                                                  |                                                                                                  |
 `only(examples: String)`                                              | `args(ex=examples)`                                                                   |                                                                                                  |
 `wasIssue`                                                          | `args(wasIssue=true)`                                                                   |                                                                                                  |
 `was(status: String)`                                               | `args(was=status)`                                                                      |                                                                                                  |
 `plan`                                                                | `args(plan=true)`                                                                     |                                                                                                  |
 `skipAll`                                                             | `args(skipAll=true)`                                                                  |                                                                                                  |
 `stopOnFail`                                                          | `args(stopOnFail=true)`                                                               |                                                                                                  |
 `sequential`                                                          | `args(sequential=true)`                                                               |                                                                                                  |
 `xonly`                                                               | `args(xonly=true)`                                                                    |                                                                                                  |
 `showOnly(status: String)`                                          | `args(showOnly=status)`                                                                 |                                                                                                  |
 `noindent`                                                            | `args(noindent=true)`                                                                 |                                                                                                  |
 `literate`                                                            | `args(noindent=true, sequential=true)`                                                | for specifications where text must not be indented and examples be executed in order             |
 `freetext`                                                            | `args(plan=true, noindent=true)`                                                      | for specifications with no examples at all and free display of text                              |
 `descFromExpectations`                                                | `args.report(fromSource=false)`                                                              | create the example description for the ok message of the expectation instead of the source file  |
 `fullStackTrace`                                                      | `args.report(traceFilter=NoStackTraceFilter)`                                                | the stacktraces are not filtered                                                                 |
 `diffs(show, separators, triggerSize, shortenSize, diffRatio, full)`  | `args.report(diffs=SmartDiffs(show, separators, triggerSize, shortenSize, diffRatio, full)`  | to display the differences when doing equality comparison                                        |

##### Output directory

All the files created during the execution of a specification will be created in the `target/specs-report` directory. You can change that by setting the
`-Dspecs2.outDir` system property.

##### Storing previous results

When a specification has been executed its statistics and failed examples will be stored by default in a specific `stats` directory created in the output directory. This data can be used on subsequent runs to:

 * display trends in statistics
 * compute the statuses of the links of an index page
 * select only previously failed examples for execution

You can either:

  * disable this functionality (for performance reasons for example) with the `args.store(never=true)` argument (or `neverstore` on the command line)
  * reset the previous statistics with the `args.store(reset=true)` argument (or `resetstore` on the command line)

The statistics directory can also be redefined independently of the output directory with the `specs2.statsDir` system variable.

##### Status flags

The `was` and `showOnly` arguments expect a String made of "status flags". For example, `xonly` is equivalent to `showOnly("x!")`. Here is the list of all the flags which you can use to control the selection of fragments before execution or their display:

  Flag | Description
 ----- | ------------
  `+`  | successful example
  `x`  | failed example
  `!`  | error example
  `o`  | skipped example
  `*`  | pending example
  `-`  | text
  `1`  | statistics


##### Diffs

For the diffs arguments the values you can specify are:

  * `show` will not show anything (default is true)
  * `separators` allows to change the separators used to show the differences (default is "[]")
  * `triggerSize` controls the size above which the differences must be shown (default is 20)
  * `shortenSize` controls the number of characters to display around each difference (default is 5)
  * `diffRatio` percentage of differences above which the differences must not be shown (default is 30)
  * `full` displays the full original expected and actual strings

You can also specify your own enhanced algorithm for displaying difference by providing an instance of the `org.specs2.main.Diffs` trait:

        trait Diffs {
          /** @return true if the differences must be shown */
          def show: Boolean
          /** @return true if the differences must be shown for 2 different strings */
          def show(expected: String, actual: String): Boolean
          /** @return the diffs */
          def showDiffs(expected: String, actual: String): (String, String)
          /** @return true if the full strings must also be shown */
          def showFull: Boolean
          /** @return the separators to use */
          def separators: String
        }


##### StackTraceFilter

The `traceFilter` argument takes an instance of the `org.specs2.control.StackTraceFilter` trait to define how stacktraces should be filtered in a report. By default the `DefaultStackTraceFilter` filter will exclude lines matching the following packages:

 * `org.specs2`
 * `scalaz\\.`
 * `scala\\.`, `java\\.`
 * `sbt\\.`, `com.intellij`, `org.eclipse.jdt`, `org.junit`

If this is not what you want, you can either:

 * use `includeTrace(patterns: String*)` to create a new `StackTraceFilter` which will include only the traces matching
   those patterns
 * use `excludeTrace(patterns: String*)` to create a new `StackTraceFilter` which will exclude only the traces matching
   those patterns
 * use `includeAlsoTrace(patterns: String*)` to add new include patterns to the `DefaultStackTraceFilter`
 * use `excludeAlsoTrace(patterns: String*)` to add new exclude patterns to the `DefaultStackTraceFilter`
 * use the `org.specs2.control.IncludeExcludeStackTraceFilter` class to define both include and exclude patterns
 * define your own logic by extending the `org.specs2.control.StackTraceFilter`

#### On the command line

On the command line you can pass the following arguments:

  Name            | Value format            | Comments
 ---------------- | ----------------------- | ------------------------------------------------------------------------
 ***Selection***  |||
 `ex`             | regexp                  |                                                                         |
 `include`        | csv                     |                                                                         |
 `exclude`        | csv                     |                                                                         |
 `wasIssue`       | boolean                 |                                                                         |
 `was`            | String                  | see: Status flags                                                       |
 `specname`       | regexp                  |                                                                         |
 ***Execution***  |||
 `plan`           | boolean                 |                                                                         |
 `skipall`        | boolean                 |                                                                         |
 `sequential`     | boolean                 |                                                                         |
 `threadsnb`      | int                     |                                                                         |
 ***Storing***    |||
 `resetstore`     | boolean                 |                                                                         |
 `neverstore`     | boolean                 |                                                                         |
 ***Reporting***  |||
 `xonly`          | boolean                 |                                                                         |
 `showonly`       | String                  | see: Status flags                                                       |
 `failtrace`      | boolean                 |                                                                         |
 `color`          | boolean                 |                                                                         |
 `colors`         | map                     | e.g. text:be, failure:m (see the Colors section)                        |
 `noindent`       | boolean                 |                                                                         |
 `showtimes`      | boolean                 |                                                                         |
 `markdown`       | boolean                 |                                                                         |
 `debugmarkdown`  | boolean                 |                                                                         |
 `fromsource`     | boolean                 |                                                                         |
 `fullstacktrace` | boolean                 |                                                                         |
 `tracefilter`    | regexp-csv/regexp-csv   | comma-separated include patterns separated by `/` with exclude patterns |

_[`regexp` is a Java regular expression, csv a list of comma-separated values, map is a list of csv pairs key:value]_


#### From system properties

You can pass any argument to ***specs2*** from system properties:

 * for a boolean argument, you need to pass `-Dspecs2.name` or `-Dname`
 * for a string argument, you need to pass `-Dspecs2.name=value` or `-Dname=value`

While the format `-Dname=value` can be convenient, `-Dspecs2.name=value` is recommended to avoid conflicts with other libraries.

### Console output

Executing a specification `com.company.SpecName` in the console is very easy:

`scala -cp ... specs2.run com.company.SpecName [argument1 argument2 ...]`

### Html output

If you want html pages to be produced for your specification you'll need to execute:

`scala -cp ... specs2.html com.company.SpecName [argument1 argument2 ...]`

### JUnit XML output

Many Continuous Integration systems rely on JUnit XML reports to display build and test results. It is possible to produce
those result by using the `specs2.junitxml` object:

`scala -cp ... specs2.junitxml com.company.SpecName [argument1 argument2 ...]`

### Files Runner

The `specs2.files` object will, by default, select and execute Specifications found in the test source directory:

 * the source directory is defined as `src/test/scala` but can be changed by adjusting the system property `specs2.srcTestDir`
 * the specifications files are selected as classes or object which names match `.*Spec`. This value can be changed by
   passing a different `specName` value as a command-line argument
 * `console` or `html` has to be passed on the command-line to specify which kind of output you want

You can also extend the `org.specs2.runner.FilesRunner` trait and override its behavior to implement something more appropriate
to your environment if necessary.

### Simple build tool

#### with sbt 0.7.x

In order to use ***specs2*** with sbt 0.7.x you need first to add the following lines to your sbt project:

      def specs2Framework = new TestFramework("org.specs2.runner.SpecsFramework")
      override def testFrameworks = super.testFrameworks ++ Seq(specs2Framework)

Then, depending on the naming of your specification, you have to specify which classes you want to include for reporting:

      override def includeTest(s: String) = { s.endsWith("Spec") || s.contains("UserGuide") }

#### with sbt > 0.9.x

In this case you don't need to do much because ***specs2*** will be recognized out-of-the-box. However, if you want to filter some specifications you need to add this to your `build.sbt` file (see [here](https://github.com/harrah/xsbt/wiki/Testing) for more information):

      // keep only specifications ending with Spec or Unit
      testOptions := Seq(Tests.Filter(s => Seq("Spec", "Unit").exists(s.endsWith(_))))

If you don't want the specifications to be executed in parallel:

      parallelExecution in Test := false

If you want to pass arguments available for all specifications:

      testOptions in Test += Tests.Argument("nocolor", "neverstore")

If you want the examples results to be displayed as soon as they've been executed you need to add:

      logBuffered := false

##### Test-only arguments

When you execute one test only, you can pass the arguments on the command line:

      > test-only org.specs2.UserGuide -- xonly

##### Output formats

The `html` argument is available with sbt to allow the creation of the html report from the command line.

      > test-only org.specs2.UserGuide -- html

      // in your build.sbt file
      testOptions in Test += Tests.Argument("html")

Similarly, JUnit xml output files can be created by passing the `junitxml` option:

      > test-only org.specs2.examples.HelloWorldUnitSpec -- junitxml

      // in your build.sbt file
      testOptions in Test += Tests.Argument("junitxml")

If you want to get a console output as well, don't forget to add the `console` argument:

      > test-only org.specs2.UserGuide -- html console

      // in your build.sbt file
      testOptions in Test += Tests.Argument("html", "console")

##### Files runner

Any `FilesRunner` object can also be invoked by sbt, but you need to specify `console` or `html` (or both) on the command line:

      > test-only allSpecs -- console

##### Colors

By default, the reporting will output colors. If you're running on windows you might either:

 * use the [following tip](http://www.marioawad.com/2010/11/16/ansi-command-line-colors-under-windows) to install colors in the DOS console
 * or pass `nocolor` as a command line argument

Then, there are different ways to set-up the colors you want to use for the output

*From system properties*

The so-called "SmartColors" argument will check if there are colors defined as specs2 properties. If so, the colors  used
to output text in the Console will be extracted from those properties:

e.g. `-Dspecs2.color.failure=m` will use magenta for failures.

The property names and default values are:

Property        | Default value |
--------------- |  ------------ |
`color.text`    |  white        |
`color.success` |  green        |
`color.failure` |  yellow       |
`color.error`   |  red          |
`color.pending` |  blue         |
`color.skipped` |  cyan         |
`color.stats`   |  blue         |

The default values above are provided for a black background. If you have a white background you can use the `specs2.whitebg` property and then the default values will be:

Property        | Default value |
--------------- |  ------------ |
`color.text`    |  black        |
`color.success` |  green        |
`color.failure` |  magenta      |
`color.error`   |  red          |
`color.pending` |  blue         |
`color.skipped` |  cyan         |
`color.stats`   |  blue         |

All the available colors are listed here, with their corresponding abbreviation which you can use to refer to them as well:

 Color   | Abbreviation |
 ------  | ------------ |
 white   | w            |
 green   | g            |
 yellow  | y            |
 red     | r            |
 blue    | be           |
 cyan    | c            |
 black   | bk           |
 magenta | m            |


*From command-line arguments*

It is also possible to set colors by passing the `colors` argument. This argument must be a list of `key:value` pairs (comma-separated) where keys are taken from the property names above without the `color.` prefix and values from the abbreviated color names.

For example you can pass on the command line:

 `colors text:blue,failure:magenta`

 to have the text colored in blue and the failures in Magenta.

If the `colors` option contains `whitebg` then the default colors are considered to be [`InvertedColors`](http://etorreborre.github.com/specs2/api/index.html#org.specs2.text.Colors)

*Through the API*

Finally you can change the color scheme that's being used on the console by implementing your own [`org.specs2.text.Colors`](http://etorreborre.github.com/specs2/api/index.html#org.specs2.text.Colors) trait or override values in the existing `ConsoleColors` class. For example if you want to output magenta everywhere yellow is used you can write:

      object MyColors = new org.specs2.text.ConsoleColors { override val failureColor = magenta }

      class MyColoredSpecification extends Specification { def is = colors(MyColors) ^
         // the failure message will be magenta
         "this is a failing example" ! failure
      }

Note also that the the color support for sbt on Windows is a bit tricky. You need to follow the instructions [here](http://www.marioawad.com/2010/11/16/ansi-command-line-colors-under-windows) then add to your script launching sbt:

        -Djline.terminal=jline.UnsupportedTerminal

### JUnit

It is possible to have ***specs2*** specifications executed as JUnit tests. This enables the integration of ***specs2*** with Maven and the JUnit runners of your IDE of choice.

There are 2 ways of enabling a Specification to be executed as a JUnit test: the verbose one and the simpler one. The simplest one is to extend `SpecificationWithJUnit`:

       class MySpecification extends SpecificationWithJUnit {
         def is = // as usual....
       }

You can use the second one if your IDE doesn't work with the first one:

      import org.junit.runner._
      import runner._

      @RunWith(classOf[JUnitRunner])
      class MySpecification extends Specification {
         def is = // as usual....
      }

[*some [tricks](http://code.google.com/p/specs/wiki/RunningSpecs#Run_your_specification_with_JUnit4_in_Eclipse) described on the specs website can still be useful there*]

#### Arguments

You can pass arguments to the `JUnitRunner` for generating the html files for the specifications or for displaying the console output. To do that, you can use the `-Dspecs2.commandline` property and pass it the `html` or `console` values.

### IntelliJ

IntelliJ offers a nice integration with ***specs2***. You can:

 * Execute a specification by selecting its name and pressing CTRL+SHIFT+F10
 * Execute a single example by selecting its description and pressing CTRL+SHIFT+F10

 ![specs2 in Intellij](images/intellij.png)

But also:

 * Provide command-line arguments in the "Test options"
 * "Jump to Test" and "Jump to Source"

### Notifier runner

A `NotifierRunner` accepts a `Notifier` to execute a specification and report execution events. The `Notifier` trait notifies of the following:

 * specification start: the beginning of a specification, with its name
 * specification end: the end of a specification, with its name
 * context start: the beginning of a sub-level when the specification is seen as a tree or Fragments
 * context end: the end of a sub-level when the specification is seen as a tree or Fragments
 * text: any Text fragment that needs to be displayed
 * example start
 * example result: success / failure / error / skipped / pending

All those notifications come with a location (to trace back to the originating fragment in the Specification) and a duration when relevant (i.e. for examples only).

### From the console

The `specs2.run` object has an `apply` method to execute specifications from the Scala console:

      scala> specs2.run(spec1, spec2)

      scala> import specs2._  // same thing, importing the run object
      scala> run(spec1, spec2)

If you want to pass specific arguments you can import the `specs2.arguments` object member functions:

      scala> import specs2.arguments._

      scala> specs2.run(spec1)(nocolor)

Or you can set implicit arguments which will be used for any specification execution:

      scala> import specs2.arguments._
      scala> implicit val myargs = nocolor

      scala> specs2.run(spec1)

   - - -

           	                                                                                                            """ ^
                                                                                                                        br ^
  include(xonly, argumentsSpec)                                                                                          ^
                                                                                                                        end

  // User guide examples can be added here
  val argumentsSpec = new Specification { def is =
    { args.report(color=false).color must beFalse } ^
                                                    end
  }
}