package org.specs2
package guide

class Runners extends Specification { def is = freetext ^
  "Runners guide".title ^
                                                                                                                        """
### Presentation

<toc/>

There are 4 ways to execute ***specs2*** specifications:

 * on the command line, with a console output, and the `specs2.run` runner
 * on the command line, with an html output, and the `specs2.html` runner
 * on the command line, with a console or an html output, and the `specs2.files` runner
 * using [sbt](http://code.google.com/p/simple-build-tool)
 * using [JUnit](http://www.junit.org)

### Dependencies

 ***specs2*** is only available with Scala 2.8.1 onwards and uses the following libraries, as specified using the [sbt dsl](http://code.google.com/p/simple-build-tool/wiki/LibraryManagement#Basic_Dependencies):

 <table class="dataTable"><tr><th>Dependency</th><th>Comment</th></tr><tr><td class="info">`"com.googlecode.scalaz" %% "scalaz-core" % "5.1-SNAPSHOT"`</td><td class="info">mandatory</td></tr><tr><td class="info"> `"org.scala-tools.testing" %% "scalacheck" % "1.8"`</td><td class="info">only if using ScalaCheck</td></tr><tr><td class="info">`"org.mockito" % "mockito-all" % "1.8.5"`</td><td class="info">only if using Mockito</td></tr><tr><td class="info">`"org.hamcrest" % "hamcrest-all" % "1.1"`</td><td class="info">only if using Hamcrest matchers with Mockito</td></tr><tr><td class="info">`"junit" % "junit" % "4.7"`</td><td class="info">only if using JUnit</td></tr><tr><td class="info">`"org.scala-tools.testing" % "test-interface" % "0.5"`</td><td class="info">provided by sbt when using it</td></tr><tr><td class="info">`"org.parboiled" % "parboiled4j" % "0.9.9.0"`</td><td class="info">only if using the html runner</td></tr><tr><td class="info">`"org.pegdown" % "pegdown" % "0.8.5.4"`</td><td class="info">only if using the html runner</td></tr></table>

The last 2 jars are the Pegdown and Parboiled libraries for Markdown parsing. They cannot yet be found in an official maven
repository, so you'll need to add the specs temporary Maven repository to your sbt project:

  `val specsRepo = "specs-repo" at "http://specs.googlecode.com/svn/maven2"`

### Arguments

You can specify arguments which will control the execution and reporting. They can be passed on the command line, or declared
inside the specification:

      class MySpec extends Specification { def is = args(noindent=true) ^
        "Clever spec title"                                             ^
        "this will not be indented"                                     ^
        "brilliant expectation"                                         ! success
      }

#### In a Specification

From inside a specification, the available arguments are the following:

  Name           | Default value           | Description
 --------------- | ----------------------- | -------------------------------------------------------------------------------------------
 `ex`            | .*                      | regular expression specifying the examples to execute. Use `ex .*brilliant.*` on the command line
 `xonly`         | false                   | only reports failures and errors
 `include`       | ""                      | execute only the fragments tagged with any of the comma-separated list of tags: "t1,t2,..."
 `exclude`       | ""                      | do not execute the fragments tagged with any of the comma-separated list of tags: "t1,t2,..."
 `plan`          | false                   | only report the text of the specification without executing anything
 `skipAll`       | false                   | skip all the examples
 `failtrace`     | false                   | report the stacktrace for failures
 `color`         | true                    | use colors in the output (`nocolor` can also be used on the command line)
 `noindent`      | false                   | don't indent automatically text and examples
 `showtimes`     | false                   | show individual execution times
 `sequential`    | false                   | don't execute examples concurrently
 `threadsNb`     | 0                       | number of threads to use for concurrent execution
 `markdown`      | true                    | interpret text as Markdown in the html reporter
 `debugMarkdown` | false                   | print more information when Markdown formatting fails
 `fromSource`    | true                    | true takes an AutoExample description from the file, false from the expectation ok message
 `traceFilter`   | DefaultStackTraceFilter | use a StackTraceFilter instance for filtering the reported stacktrace elements


All those arguments are usually set in a specification with `args(name=value)` but there are some available shortcuts:

 Name                                                      | Equivalent                                                           | Description                                                                                      |
 ---------------                                           | -----------------------                                              | -----------                                                                                      |
 `plan`                                                    | `args(plan=true)`                                                    |                                                                                                  |
 `skipAll`                                                 | `args(skipAll=true)`                                                 |                                                                                                  |
 `noindent`                                                | `args(noindent=true)`                                                |                                                                                                  |
 `xonly`                                                   | `args(xonly=true)`                                                   |                                                                                                  |
 `include(tags: String)`                                   | `args(include=tags)`                                                 |                                                                                                  |
 `exclude(tags: String)`                                   | `args(exclude=tags)`                                                 |                                                                                                  |
 `only(examples: String)`                                  | `args(ex=examples)`                                                  |                                                                                                  |
 `sequential`                                              | `args(sequential=true)`                                              |                                                                                                  |
 `literate`                                                | `args(noindent=true, sequential=true)`                               | for specifications where text must not be indented and examples be executed in order             |
 `freetext`                                                | `args(plan=true, noindent=true)`                                     | for specifications with no examples at all and free display of text                              |
 `descFromExpectations`                                    | `args(fromSource=false)`                                             | create the example description for the ok message of the expectation instead of the source file  |
 `fulltrace`                                               | `args(traceFilter=NoStackTraceFilter)`                               | the stacktraces are not filtered                                                                 |
 `diffs(show, separators, triggerSize, shortenSize, full)` | `args(diffs=Diffs(show, separators, triggerSize, shortenSize, full)` | to display the differences when doing equality comparison                                        |

##### Diffs

For the diffs arguments the values you can specify are:

  * `show` will not show anything (default is true)
  * `separators` allows to change the separators used to show the differences (default is "[]")
  * `triggerSize` controls the size above which the differences must be shown (default is 20)
  * `shortenSize` controls the number of characters to display around each difference (default is 5)
  * `full` displays the full original expected and actual strings

##### StackTraceFilter

The `traceFilter` argument takes an instance of the `org.specs2.control.StackTraceFilter` trait to define how stacktraces
should be filtered in a report. By default the `DefaultStackTraceFilter` filter will exclude lines matching the following packages:

 * `org.specs2`
 * `scalaz.concurrent`, `java.util.concurrent`
 * `scala\\.`
 * `sbt\\.`, `com.intellij`, `org.eclipse.jdt`, `org.junit`

If this is not what you want you can either:

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

  Name           | Value format            | Comments
 --------------- | ----------------------- | -------------------------------------------
 `ex`            | regexp                  |
 `xonly`         | boolean                 |
 `include`       | csv                     |
 `exclude`       | csv                     |
 `plan`          | boolean                 |
 `skipall`       | boolean                 |
 `failtrace`     | boolean                 |
 `color`         | boolean                 |
 `noindent`      | boolean                 |
 `showtimes`     | boolean                 |
 `sequential`    | boolean                 |
 `threadsnb`     | int                     |
 `markdown`      | boolean                 |
 `debugmarkdown` | boolean                 |
 `html`          | boolean                 | to get console + html reporting at once
 `fromsource`    | boolean                 |
 `tracefilter`   | regexp-csv/regexp-csv   | comma-separated include patterns separated by `/` with exclude patterns

_[`regexp` is a Java regular expression, csv a list of comma-separated values]_


#### From system properties

You can pass any argument to ***specs2*** from system properties:

 * for a boolean argument, you need to pass `-Dspecs2.name` or `-Dname`
 * for a string argument, you need to pass `-Dspecs2.name=value` or `-Dname=value`

The recommended format is `-Dname=value` but `-Dspecs2.name=value` is also available to avoid conflicts with other libraries.

### Console output

Executing a specification `com.company.SpecName` in the console is very easy:

`scala -cp ... specs2.run com.company.SpecName [argument1 argument2 ...]`

##### Colors

By default, the reporting will output colors. If you're running on windows you might either:

 * use the [following tip](http://www.marioawad.com/2010/11/16/ansi-command-line-colors-under-windows) to install colors in the DOS console
 * or pass `nocolor` as a command line argument

### Html output

If you want html pages to be produced for your specification you'll need to execute:

`scala -cp ... specs2.html com.company.SpecName [argument1 argument2 ...]`

By default the files will be created in the `target/specs-report` directory but you can change that by setting the
`-Dspecs2.outDir` system property.

### Files Runner

The `specs2.files` object will, by default, select and execute Specifications found in the test source directory:

 * the source directory is defined as `src/test/scala` but can be changed by adjusting the system property `specs2.srcTestDir`
 * the specifications files are selected as classes or object which names match `.*Spec`. This value can be changed by
   passing a different `specName` value as a command-line argument
 * `console` or `html` has to be passed on the command-line to specify which kind of output you want

You can also extend the `org.specs2.runner.FilesRunner` trait and override its behavior to implement something more appropriate
to your environment if necessary.

### Simple build tool

In order to use ***specs2*** with sbt you need first to add the following lines to your sbt project:

      def specs2Framework = new TestFramework("org.specs2.runner.SpecsFramework")
      override def testFrameworks = super.testFrameworks ++ Seq(specs2Framework)

Then, depending on the naming of your specification, you have to specify which classes you want to include for reporting:

      override def includeTest(s: String) = { s.endsWith("Spec") || s.contains("UserGuide") }

##### Test-only arguments

When you execute one test only, you can pass the arguments on the command line:

      > test-only org.specs2.UserGuide -- xonly

The `html` argument is also available with sbt to allow the creation of the html report from the command line.

      > test-only org.specs2.UserGuide -- html

##### Files runner

Any `FilesRunner` object can also be invoked by sbt, but you need to specify `console` or `html` on the command line:

      > test-only allSpecs -- console

##### Colors

Note the the color support for sbt on Windows is a bit tricky. You need to follow the instructions [here](http://www.marioawad.com/2010/11/16/ansi-command-line-colors-under-windows) then add to your script launching sbt:

        -Djline.terminal=jline.UnsupportedTerminal

### JUnit

It is possible to have ***specs2*** specifications executed as JUnit tests. This enables the integration of ***specs2*** with
Maven and the JUnit runners of your IDE of choice.

There are 2 ways of enabling a Specification to be executed as a JUnit test: the verbose one and the simpler one. The
simple one is to extend `SpecificationWithJUnit`:

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


   - - -

           	                                                                                                            """ ^
                                                                                                                        br ^
                                                                                                                        end

}