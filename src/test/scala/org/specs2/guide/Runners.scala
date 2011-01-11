package org.specs2
package guide

class Runners extends Specification { def is = freetext ^
  "Runners guide".title ^
                                                                                                                        """
### Presentation

<toc/>

There are 4 ways to execute ***specs2*** specifications:

 * on the command line, with a console output, and the `specs2.run` runner
 * on the command line, with an html output,  and the `specs2.html` runner
 * using [sbt](http://code.google.com/p/simple-build-tool)
 * using [JUnit](http://www.junit.org)

### Dependencies

 ***specs2*** is only available with Scala 2.8.1 onwards and uses the following libraries, as specified using the [sbt dsl](http://code.google.com/p/simple-build-tool/wiki/LibraryManagement#Basic_Dependencies):

 <table class="dataTable"><tr><th>Dependency</th><th>Comment</th></tr><tr><td class="info">`"com.googlecode.scalaz" %% "scalaz-core" % "5.1-SNAPSHOT"`</td><td class="info">mandatory</td></tr><tr><td class="info"> `"org.scala-tools.testing" %% "scalacheck" % "1.8"`</td><td class="info">only if using ScalaCheck</td></tr><tr><td class="info">`"org.mockito" % "mockito-all" % "1.8.5"`</td><td class="info">only if using Mockito</td></tr><tr><td class="info">`"org.hamcrest" % "hamcrest-all" % "1.1"`</td><td class="info">only if using Hamcrest matchers with Mockito</td></tr><tr><td class="info">`"junit" % "junit" % "4.7"`</td><td class="info">only if using JUnit</td></tr><tr><td class="info">`"org.scala-tools.testing" % "test-interface" % "0.5"`</td><td class="info">provided by sbt when using it</td></tr><tr><td class="info">`"org.parboiled" % "parboiled4j" % "0.9.9.0"`</td><td class="info">only if using the html runner</td></tr><tr><td class="info">`"org.pegdown" % "pegdown" % "0.8.5.4"`</td><td class="info">only if using the html runner</td></tr></table>

The last 2 jars are the Pegdown and Parboiled libraries for Markdown parsing. They cannot yet be found in an official maven
repository, so you'll need to add the specs temporary Maven repository to your sbt project:

  `val specsRepo = "specs-repo" at "http://specs.googlecode.com/svn/maven2"`

#### Arguments

You can specify arguments which will control the execution and reporting. They can be passed on the command line, or declared
inside the specification:

      class MySpec extends Specification { def is = args(noindent=true) ^
        "Clever spec title"                                             ^
        "this will not be indented"                                     ^
        "brilliant expectation"                                         ! success
      }

The available arguments are the following:

  Name           | Default value | Description
 --------------- | ------------- | -------------------------------------------------------------------------------------------
 `ex`            | .*            | regular expression specifying the examples to execute. Use `ex .*brilliant.*` on the command line
 `xonly`         | false         | only reports failures and errors
 `plan`          | true          | only report the text of the specification without executing anything
 `failtrace`     | false         | report the stacktrace for failures
 `color`         | true          | use colors in the output (`nocolor` can also be used on the command line)
 `noindent`      | false         | don't indent automatically text and examples
 `showtimes`     | false         | show individual execution times
 `sequential`    | false         | don't execute examples concurrently
 `threadsNb`     | 0             | number of threads to use for concurrent execution
 `markdown`      | true          | interpret text as Markdown in the html reporter
 `debugMarkdown` | false         | print more information when Markdown formatting fails
 `html`          | false         | only to be passed on the command line to get console + html reporting at once
 `fromSource`    | true          | when using AutoExamples takes the description of the Example from the source file


All those arguments are usually set in a specification with `args(name=value)` but there are some available shortcuts:

 Name                                                      | Equivalent                                                           | Description                                                                                      |
 ---------------                                           | -----------------------                                              | -----------                                                                                      |
 `plan`                                                    | `args(plan=true)`                                                    |                                                                                                  |
 `noindent`                                                | `args(noindent=true)`                                                |                                                                                                  |
 `xonly`                                                   | `args(xonly=true)`                                                   |                                                                                                  |
 `only(examples: String)`                                  | `args(ex=examples)`                                                  |                                                                                                  |
 `sequential`                                              | `args(sequential=true)`                                              |                                                                                                  |
 `literate`                                                | `args(noindent=true, sequential=true)`                               | for specifications were text must not be indented and examples be executed in order              |
 `freetext`                                                | `args(plan=true, noindent=true)`                                     | for specifications with no examples at all and free display of text                              |
 `descFromExpectations`                                    | `args(fromSource=false)`                                             | create the example description for the ok message of the expectation instead of the source file  |
 `diffs(show, separators, triggerSize, shortenSize, full)` | `args(diffs=Diffs(show, separators, triggerSize, shortenSize, full)` | to display the differences when doing equality comparison                                        |

For the diffs arguments the values you can specify are:

  * `show` will not show anything (default is true)
  * `separators` allows to change the separators used to show the differences (default is "[]")
  * `triggerSize` controls the size above which the differences must be shown (default is 20)
  * `shortenSize` controls the number of characters to display around each difference (default is 5)
  * `full` displays the full original expected and actual strings

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

### Simple build tool

In order to use ***specs2*** with sbt you need first to add the following lines to your sbt project:

      def specs2Framework = new TestFramework("org.specs2.runner.SpecsFramework")
      override def testFrameworks = super.testFrameworks ++ Seq(specs2Framework)

Then, depending on the naming of your specification, you have to specify which classes you want to include for reporting:

      override def includeTest(s: String) = { s.endsWith("Spec") || s.contains("UserGuide") }

##### arguments

When you execute one test only, you can pass the arguments on the command line:

      > test-only org.specs2.UserGuide -- xonly

The `html` argument is also available with sbt to allow the creation of the html report from the command line.

      > test-only org.specs2.UserGuide -- html

##### Colors

Note the the color support for sbt on Windows is still a bit [tricky](http://code.google.com/p/simple-build-tool/issues/detail?id=134).

##### Parallel execution

From time to time you may experience a stacktrace in sbt output. This is documented [here](http://code.google.com/p/simple-build-tool/issues/detail?id=133).

### JUnit

It is possible to have ***specs2*** specifications executed as JUnit tests. This enables the integration of ***specs2*** with
Maven and the JUnit runners of your IDE of choice.

There are 2 ways of enabling a Specification to be executed as a JUnit test: the verbose one and the simpler one. The
simple one is to extend `SpecificationWithJUnit`:

       class MySpecification extends SpecificationWithJUnit {
         def is = // as usual....
       }

The second one is equivalent and shows what really happens:

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