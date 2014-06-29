package org.specs2
package guide
package old

import data.AlwaysTag
import specification._
import create._
import core._
import execute.{AsResult, Result, ResultExecution, Failure}
import main.Arguments
import matcher.Matcher
import time.SimpleTimer

class HowTo extends UserGuidePage { def is = s2"""

### Declare arguments

Arguments are usually passed on the command line but you can also declare them at the beginning of the specification, to be applied only to that specification.
For example, you can turn off the concurrent execution of examples with the `args(sequential = true)` call (or the shorter alias `sequential`): ${snippet{

class ExamplesOneByOne extends Specification { def is = sequential ^ s2"""

  first example        $e1
  the the second one   $e2
                       """
}
// 8<--
lazy val (e1, e2) = (ok, ok)
}}

For the complete list of arguments and shortcut methods read the [Runners](org.specs2.guide.Runners.html) page.

### Add a title

Usually the title of a specification is derived from the specification class name. However if you want to give a more readable name to your specification report you can do the following: ${snippet{

class MySpec extends Specification { def is = "My beautiful specifications".title ^ s2"""

// The rest of the spec goes here
"""
}
}}

### Use descriptions

The description of an Example can be used to create an expectation in the example body:

${snippet {"This is a long, long, long description" ! ((s: String) => s.size must be_>(10)) }}

### Create an index

Here's something you can do to automatically create an index page for your specifications: ${snippet{

import org.specs2._
import runner.SpecificationsFinder._

class index extends Specification { def is =

  examplesLinks("Example specifications")

  // see the SpecificationsFinder trait for the parameters of the 'specifications' method
  def examplesLinks(t: String) = t.title ^ specifications().map(s => link(s))
}
}}

The specification above creates an index.html file in the `target/specs2-reports` directory. The specifications method
creates specifications using the following parameters:

* `path`: glob pattern to filter specification files. Default value is `**/*.scala`
* `pattern`: pattern to use when trying to retrieve the specification names from the source files. Default value = `.*Spec`
* `filter`: function to keep only some specifications depending on their name. Default value = `(name: String) => true`
* `basePath`: the path where to start the search. Default value: the `specs2.srcTestDir` system value = `src/test/scala`
* `verbose`: boolean indicating if information about finding files and specifications must be printed. Default value = `false`

### Remove implicits

By default, the `Specification` trait imports quite a few implicit definitions (following a "batteries included" approach). However there might be some conflicts with implicits existing in your own user code. Among the usual examples of conflicts are conflicts with the `===` sign in Scalaz and the `Duration` methods in Akka.

An easy way to avoid this situation is to "deactivate" the specs2 implicits by mixing-in the relevant trait from this list:

* `org.specs2.control.NoDebug`: deactivate the `pp` method on objects
* `org.specs2.main.NoArgProperties`: deactivate the `toOption: Option[T]` method on any value of type `T`
* `org.specs2.matcher.NoCanBeEqual`: deactivate the `===` method on any type `T`
* `org.specs2.matcher.NoMustExpectations`: deactivate the `must`, `must_==`,... methods on any value of type `T`
* `org.specs2.matcher.NoShouldExpectations`: deactivate the `should`, `should_==`,... methods on any value of type `T`\
* `org.specs2.matcher.NoExpectationsDescription`: deactivate the `<==>` and `==>` methods on Strings
* `org.specs2.matcher.NoConcurrentExecutionContext`: deactivate the implicit execution context used for `FutureMatchers`
* `org.specs2.specification.NoAutoExamples`: deactivate the conversions from `Boolean/Result/MatchResult/DataTable` to `Fragment` or `Example`. Specific versions of this trait can be selectively used, on either `Boolean` or `Result` or `MatchResult` or `DataTable`. For example: `org.specs2.specification.NoBooleanAutoExamples` can be used to avoid the `^` method being used on booleans
* `org.specs2.specification.NoFragmentsBuilder`: deactivate the implicit conversions from `String` to `Fragment`s
* `org.specs2.specification.mutable.NoFragmentsBuilder`: deactivate the implicit conversions from to remove `in`, <code class="prettyprint">></code><code class="prettyprint">></code>, `should` and `can` methods from `String`s
* `org.specs2.specification.NoToHtmlLinkFragments`: deactivate the use of `~` and `~/` operators on Strings to create html links

### Print execution data


### Capture snippets

It is possible to include pieces of code in your documentation with the `${fullName[Snippets]}` trait using the `snippet` method to capture a block code with marker comments to delimit the parts you want to show.

What does this look like?

#### snippet

Here is an example of using the `snippet` method:

```
s2$triple
This is a multi-line string with a snippet of code: $${ snippet {
def factorial(n: Int): Int = if (n == 1) n else (n * factorial(n - 1))
factorial(3) == 6
}}
$triple
```

When you use the `snippet` method, the reports will show:

This is a multi-line string with a snippet of code:

```
def factorial(n: Int): Int = if (n == 1) n else (n * factorial(n - 1))
factorial(3) == 6
```

#### cuts

Since snippets are compiled code, it might be necessary for you to add many declarations for this code, like imports or variables definitions, to be valid even if you don't want to show them. One way to do this is to delimit the code to show with some comments of the form `// 8<--`:

```
s2$triple
This is a snippet of code with one relevant line: $${ snippet {
// 8<--
def factorial(n: Int): Int = if (n == 1) n else (n * factorial(n - 1))
// 8<--
factorial(3) == 6
// 8<--
}}
$triple
```

The snippet above will only show `factorial(3) == 6`. You can actually repeat this pattern several times:

```
s2$triple
This is a snippet of code with 2 relevant lines: $${ snippet {
// 8<--
def factorial(n: Int): Int = if (n == 1) n else (n * factorial(n - 1))
// 8<--
factorial(3) == 6
// 8<--
val n = 4
// 8<--
factorial(n) == 24
}}
$triple
```

This just displays:

```
factorial(3) == 6
factorial(n) == 24
```

#### evaluation

By default the last value of a Snippet is not shown but you can display it with the `eval` method:

```
s2$triple
This is a snippet of code with a result: $${ snippet {
factorial(3)
}.eval}
$triple
```

This displays:

```
factorial(3)
```
```
> 6
```

#### offsets

It is possible to adjust the margin of captured source code by adding or removing whitespace:

```
s2$triple
This is a snippet of code with a negative offset to align the code to the border of the screen: $${ snippet {
def factorial(n: Int): Int = if (n == 1) n else (n * factorial(n - 1))
factorial(3)
}.offsetIs(-3)}
$triple
```

This displays:

```
factorial(3)
```

#### parameters

All of the settings above: cuts, offset,... are coming from an implicit `SnippetParams` object that is changing the behavior of the created Snippets. You can choose, for a given scope, to replace these parameters with other ones and simply shadow the default parameters with your own, for example to always evaluate the snippets results:

     implicit snippetParams = SnippetParams(eval = true)

The parameters you can set are:

name              | description
----------------- | ----------------
 `trimExpression` | function that is trimming the expression from newlines or accolades
 `cutter`         | function to remove parts which must not be shown
 `asCode`         | function to render the resulting text (as Markdown for example)
 `prompt`         | function to display the evaluated result with a prompt
 `eval`           | boolean indicating if a snippet must be evaluated
 `verify`         | function checking the snippet value

#### names

It is also possible to capture trait/classes or method/attribute names with the following method calls:

<table>
<tr><td>a trait name                  </td><td><code class="prettyprint">$${simpleName[SpecificationLike] } === SpecificationLike                         </code></td></tr>
<tr><td>a fully qualified trait name  </td><td><code class="prettyprint">$${fullName[SpecificationLike] }   === org.specs2.specification.SpecificationLike</code></td></tr>
<tr><td>a method/attribute name       </td><td><code class="prettyprint">$${termName(toString)}             === toString                                  </code></td></tr>
<tr><td>a function name               </td><td><code class="prettyprint">$${termName(factorial(1)}          === factorial                                 </code></td></tr>
</table>

#### outside specs2

These functionalities are accessible outside of specs2 by importing the `${fullName[org.specs2.execute.Snippets]}` trait.


"""

  def machine = Machine()
  case class Machine() { def tickets = Seq() }
  def beOk: Matcher[Any] = (a: Any) => (true, "")
}
