package org.specs2
package guide

import org.specs2.specification.Snippets

class HowTo extends UserGuidePage { def is = s2"""

### Declare arguments

Arguments are usually passed on the command line but you can also declare them at the beginning of the specification, to be applied only to that specification.
For example, you can turn off the concurrent execution of examples with the `args(sequential = true)` call (or the shorter alias `sequential`):

    class ExamplesOneByOne extends Specification { def is = s2$triple ${dollar}sequential

      first example        ${dollar}e1
      the the second one   ${dollar}e2
                           $triple
    }

For the complete list of arguments and shortcut methods read the [Runners](org.specs2.guide.Runners.html) page.

### Use command-line arguments

Some specifications can depend on the arguments passed on the command line, for example to fine-tune the behaviour of some Context objects. If you need to do this, you can add an `Arguments` parameter to the Specification class. This parameter will be setup when the specification is instantiated:

    class DependOnCommandLine(args: Arguments) extends mutable.Specification {
      skipAllUnless(!args.commandLine.contains("DB"))

      "database access" >> { dbAccess must beOk }
    }

Alternatively, if you need to keep your specification as a trait, you can mix-in the `org.specs2.main.CommandLineArguments` trait. This trait has an `arguments` variable which will contain the command-line arguments:

    class CommandedSpecification extends mutable.Specification with CommandLineArguments {
      if (arguments.sequential) "this is" >> ok
      else                      "this is" >> ko
    }

Note that the `arguments` instance gives you access to all the specs2 arguments values like `sequential` but also to any of your own command line argument values:

 * `arguments.commandLine.value("tag"): Option[String]`
 * `arguments.commandLine.int("timeout"): Option[Int]`
 * `arguments.commandLine.boolean("integration"): Boolean`

### Add a title

Usually the title of a specification is derived from the specification class name. However if you want to give a more readable name to your specification report you can do the following:

```
class MySpec extends Specification { def is = s2$triple
  ${dollar}{"My beautiful specifications".title}

  <...The rest of the spec goes here...>
                                                $triple
}
```

The title can be defined either:

 * at the beginning of the specification
 * just after the arguments of the specification

### Use descriptions

The description of an Example can be used to create an expectation in the example body:

```
"This is a long, long, long description" ! ((s: String) => s.size must be_>(10))
```

### Pending until fixed

Some examples may be temporarily failing but you may not want the entire test suite to fail just for those examples. Instead of commenting them out and then forgetting about those examples when the code is fixed, you can append `pendingUntilFixed` to the Example body:

    "this example fails for now" ! {
      1 must_== 2
    }.pendingUntilFixed

    // or, with a more specific message
    "this example fails for now" ! {
      1 must_== 2
    }.pendingUntilFixed("ISSUE-123")


The example above will be reported as `Pending` until it succeeds. Then it is marked as a failure so that you can remember to remove the `pendingUntilFixed` marker.

### Enhance failures

Most of the time, the message displayed in the case of a matcher failure is clear enough. However a bit more information is sometimes necessary to get a better diagnostic on the value that's being checked. Let's say that you want to check a "ticket list":

    // will fail with "List(ticket1, ticket2) doesn't have size 3" for example
    machine.tickets must have size(3) // machine is a user-defined object

If you wish to get a more precise failure message you can set an alias with the `aka` method (*also known as*):

    // will fail with "the created tickets 'List(ticket1, ticket2)' doesn't have size 3"
    machine.tickets aka "the created tickets" must haveSize(3)

There is also a shortcut for `value aka value.toString` which is simply `value.aka`.

And when you want other ways to customize the description, you can use:

 * `post`: `"a" post "is the first letter"` prints `a is the first letter`
 * `as`: `"b" as ((s:String) => "a"+s+"c")` prints `abc`
 * `showAs`: `Seq(1, 2, 3, 4).showAs((_:Seq[Int]).filter(isEven).mkString("|"))` prints `2|4`. This one is especially useful to filter out big data structures (lists, maps, xml...) before the failure display

### Share examples

In a given specification some examples may look similar enough that you would like to "factor" them out and share them between
different parts of your specification. The best example of this situation is a specification for a Stack of limited size: ${ snippet {

class StackSpec extends Specification { def is = s2"""

  ${"Specification for a Stack with a limited capacity".title}

  A Stack with limited capacity can either be:                             $endp
    1. Empty                                                               $anEmptyStack
    2. Normal (i.e. not empty but not full)                                $aNormalStack
    3. Full                                                                $aFullStack
                                                                           """

  def anEmptyStack =                                                       s2"""
    An empty stack should
      have a size == 0                                                     ${empty().e1}
      throw an exception when sent #top                                    ${empty().e2}
      throw an exception when sent #pop                                    ${empty().e3}
                                                                           """

  def aNormalStack =                                                       p^s2"""
    A normal stack should
      behave like a non-empty stack                                        ${nonEmptyStack(newNormalStack)}
      add to the top when sent #push                                       ${nonFullStack().e1}
                                                                           """

  def aFullStack =                                                         p^s2"""
    A full stack should
      behave like a non-empty stack                                        ${nonEmptyStack(newFullStack)}
      throw an exception when sent #push                                   ${fullStack().e1}
                                                                           """

  def nonEmptyStack(stack: =>SizedStack) =                                 t^s2"""
    have a size > 0                                                        ${nonEmpty(stack).size}
    return the top item when sent #top                                     ${nonEmpty(stack).top1}
    not remove the top item when sent #top                                 ${nonEmpty(stack).top2}
    return the top item when sent #pop                                     ${nonEmpty(stack).pop1}
    remove the top item when sent #pop                                     ${nonEmpty(stack).pop2}
  """
/** stacks creation */
def newEmptyStack  = SizedStack(maxCapacity = 10, size = 0)
def newNormalStack = SizedStack(maxCapacity = 10, size = 2)
def newFullStack   = SizedStack(maxCapacity = 10, size = 10)

/** stacks examples */
case class empty() {
val stack = newEmptyStack

def e1 = stack.size must_== 0
def e2 = stack.top must throwA[NoSuchElementException]
def e3 = stack.pop must throwA[NoSuchElementException]
}

def nonEmpty(createStack: =>SizedStack) = new {
val stack = createStack

def size = stack.size > 0

def top1 = stack.top must_== stack.size
def top2 = {
stack.top
stack.top must_== stack.size
}

def pop1 = {
val topElement = stack.size
stack.pop must_== topElement
}

def pop2 = {
stack.pop
stack.top must_== stack.size
}
}

case class nonFullStack() {
val stack = newNormalStack

def e1 = {
stack push (stack.size + 1)
stack.top must_== stack.size
}
}
case class fullStack() {
val stack = newFullStack

def e1 = stack push (stack.size + 1) must throwAn[Error]
}
}
// 8<--
/**
* SizedStack definition
*/
object SizedStack {
def apply(maxCapacity: Int, size: Int) = new SizedStack(maxCapacity).fill(1 to size)
}

class SizedStack(val capacity: Int) extends scala.collection.mutable.Stack[Int] {
override def push(a: Int) = {
if (size == capacity) throw new Error("full stack")
super.push(a)
}
def fill(range: Range) = {
range.foreach(push(_))
this
}
}
// 8<--
}}

### Create an index

Here's something you can do to automatically create an index page for your specifications:

import org.specs2._
import runner.SpecificationsFinder._

class index extends Specification { def is =

examplesLinks("Example specifications")

// see the SpecificationsFinder trait for the parameters of the 'specifications' method
def examplesLinks(t: String) = t.title ^ specifications().map(see)
}

The specification above creates an index.html file in the `target/specs2-reports` directory. The specifications method
creates specifications using the following parameters:

* `path`: glob pattern to filter specification files. Default value is `**/*.scala`
* `pattern`: pattern to use when trying to retrieve the specification names from the source files. Default value = `.*Spec`
* `filter`: function to keep only some specifications depending on their name. Default value = `(name: String) => true`
* `basePath`: the path where to start the search. Default value: the `specs2.srcTestDir` system value = `src/test/scala`
* `verbose`: boolean indicating if information about finding files and specifications must be printed. Default value = `false`

### Tag examples

Tags can be used in a Specification to include or exclude some examples or a complete section of fragments from the execution. Let's have a look at one example:

/**
* use the org.specs2.specification.Tags trait to define tags and sections
*/
class TaggedSpecification extends Specification with Tags { def is = s2$triple
this is some introductory text
and the first group of examples
example 1 ${dollar}success                        ${dollar}{tag("feature1", "unit")}
example 2 ${dollar}success                        ${dollar}{tag("integration")}

and the second group of examples            ${dollar}{section("checkin")}
example 3 ${dollar}success
example 4 ${dollar}success                        ${dollar}{section("checkin")}
$triple
}

In that specification we're defining several tags and sections:

* `feature 1` is a tag that's applied to `example1` (the _preceding_ Fragment)
* `feature 2` is a tag that's applied to `example2` (the _preceding_ Fragment)
* `checkin` marks a section which goes from the Text `and the second group of examples` to `example 4`

Armed with this, it is now easy to include or exclude portions of the specification at execution time:

* `args(include = "feature1")` will only include `example 1`
* `args(exclude = "integration")` will include everything except `example 2`
* `args(include = "checkin,unit")` will include anything having either `checkin` OR `unit`: i.e. `example 1` and the second group of examples (`example 3` and `example 4`)
* `args(include = "feature1 && unit")` will include anything having `feature1` AND `unit`: i.e. `example 1`
* `args(include = "feature1 && unit, checkin")` will include anything having `feature1` AND `unit`, OR having `checkin`: i.e. `example 1`, `example 3`, `example4`

#### In a unit specification

A _unit_ specification will accept the same `tag` and `section` methods but the behavior will be slightly different:

import org.specs2.mutable._

/**
* use the org.specs2.mutable.Tags trait to define tags and sections
*/
class TaggedSpecification extends Specification with Tags {
"this is some introductory text" >> {
"and the first group of examples" >> {
tag("feature 1", "unit")
"example 1" in success
"example 2" in success tag("integration")

}
}
section("checkin")
"and the second group of examples" >> {
"example 3" in success
"example 4" in success
}
section("checkin")

"and the last group of examples" >> {
"example 5" in success
"example 6" in success
} section("slow")
}

For that specification above:

* when the `tag` call is inserted on a new line, the tagged fragment is the one just _after_ the tag method call: `example 1`
is tagged with `feature1 and unit`,

* when the `tag` is appended to an example, it applies to that example: `example 2` is tagged with `integration`

* when the `section` call is inserted on a new line, this opens a section for all the following fragments. This should
be closed by a corresponding `section` call on a new line. For example, `example 3` and `example 4` are part of the
"checkin" section

* when the `section` call is appended to a block of Fragments on the same line, all the fragments of that block are part of
the section: `example 5` and `example 6` are tagged with `slow`

#### Skip examples

You can skip all the examples of a specification by using the `skipAllIf` or `skipAllUnless` methods:

```
class EmailSpecification extends mutable.Specification {
skipAllIf(serverIsOffLine)
"test email" >> { sendEmail must beOk }
}
```

#### Debug statements

When quick and hacky `println` statements are what you want, the `Debug` trait, mixed in every `Specification`, provides useful methods:

* `pp` or "print and pass", prints a value to the console, then return it to be used in the rest of the expression: "graph.pp must haveSize(3)"
* `pp(condition)` prints a value if a condition holds
* `pp(f: T => Boolean)` prints a value if a condition on that value holds

#### Remove implicits

By default, the `Specification` trait imports quite a few implicit definitions (following a "batteries included" approach). However there might be some conflicts with implicits existing in your own user code. Among the usual examples of conflicts are conflicts with the `===` sign in Scalaz and the `Duration` methods in Akka.

An easy way to avoid this situation is to "deactivate" the specs2 implicits by mixing-in the relevant trait from this list:

* `org.specs2.control.NoDebug`: deactivate the `pp` method on objects
* `org.specs2.time.NoTimeConversions`: deactivate the `millis`, `seconds`,... methods on `Int`s and `Long`s
* `org.specs2.main.NoArgProperties`: deactivate the `toOption: Option[T]` method on any value of type `T`
* `org.specs2.matcher.NoCanBeEqual`: deactivate the `===` method on any type `T`
* `org.specs2.matcher.NoMustExpectations`: deactivate the `must`, `must_==`,... methods on any value of type `T`
* `org.specs2.matcher.NoShouldExpectations`: deactivate the `should`, `should_==`,... methods on any value of type `T`
* `org.specs2.matcher.NoExpectationsDescription`: deactivate the `<==>` and `==>` methods on Strings
* `org.specs2.specification.NoAutoExamples`: deactivate the conversions from `Boolean/Result/MatchResult/DataTable` to `Fragment` or `Example`. Specific versions of this trait can be selectively used, on either `Boolean` or `Result` or `MatchResult` or `DataTable`. For example: `org.specs2.specification.NoBooleanAutoExamples` can be used to avoid the `^` method being used on booleans
* `org.specs2.specification.NoFragmentsBuilder`: deactivate the implicit conversions from `String` to `Fragment`s
* `org.specs2.specification.mutable.NoFragmentsBuilder`: deactivate the implicit conversions from to remove `in`, <code class="prettyprint">></code><code class="prettyprint">></code>, `should` and `can` methods from `String`s

### Print execution data

Knowing that an example passed is fine but sometimes you want to display more information, like the time spent executing the example for instance.

This can be done by creating a `Context` object which will update the `Result` of the example execution with whatever you want to display:

import org.specs2._
import time._
import specification._
import execute._

trait Timed extends Around {
def around[T : AsResult](t: =>T): Result = {
// use `ResultExecution.execute` to catch possible exceptions
val (result, timer) = withTimer(ResultExecution.execute(AsResult(t)))

// update the result with a piece of text which will be displayed in the console
result.updateExpected("Execution time: "+timer.time)
}

/** mesure the execution time of a piece of code */
def withTimer[T](t: =>T): (T, SimpleTimer) = {
val timer = (new SimpleTimer).start
val result = t
(result, timer.stop)
}

}

When you execute a specification where each example uses this `Around` context (by implementing the `AroundExampleContext` trait for example) you should see the timing of each example displayed in the console:

```
[info] TimedExecutionSpecification
[info]
[info] + example 1
[info] Execution time: 94 ms
[info] + example 2
[info] Execution time: 11 ms
```

#### With example description

More generally, you can both use the example description and the example body to display custom messages, by creating a new `ExampleFactory`:

// a trait to create an Around context using the example description
trait TimedContext {
def context(exampleDescription: String) = new Timed(exampleDescription)

case class Timed(exampleDescription: String) extends Around {
def around[T : AsResult](t: =>T): Result = {
val (result, timer) = withTimer(ResultExecution.execute(AsResult(t)))
result.updateExpected(s"Execution time for example ${dollar}exampleDescription: ${dollar}{timer.time}")
}

def withTimer[T](t: =>T): (T, SimpleTimer) = {
val timer = (new SimpleTimer).start
val result = t
(result, timer.stop)
}
}
}

class MutableTimedSpecification extends mutable.Specification with TimedContext {

"Example 1" in ok
"Example 2" in ok

// create a new MutableExampleFactory where the body of the example uses
// the current example description
override lazy val exampleFactory = new MutableExampleFactory {
override def newExample[T : AsResult](description: String, t: =>T): Example =
super.newExample(description, context(description)(AsResult(t)))
}

}

class TimedSpecification extends Specification with TimedContext { def is = s2$triple
Example 1 ${dollar}ok
Example 2 ${dollar}ok
       $triple

// create a new DefaultExampleFactory where the body of the example uses
// the current example description
override lazy val exampleFactory = new DefaultExampleFactory {
override def newExample[T : AsResult](description: String, t: =>T): Example =
super.newExample(description, context(description)(AsResult(t)))
}

}

### Capture snippets

It is possible to include pieces of code in your documentation with the `${fullName[Snippets]}` trait using the `snippet` method to capture a block code with marker comments to delimit the parts you want to show.

What does this look like?

#### snippet

Here is an example of using the `snippet` method:

```
s2$triple
This is a multi-line string with a snippet of code: ${dollar}{ snippet {
def factorial(n: Int): Int = if (n == 1) n else (n * factorial(n - 1))
factorial(3) == 6
} }
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
This is a snippet of code with one relevant line: ${dollar}{ snippet {
// 8<--
def factorial(n: Int): Int = if (n == 1) n else (n * factorial(n - 1))
// 8<--
factorial(3) == 6
// 8<--
} }
$triple
```

The snippet above will only show `factorial(3) == 6`. You can actually repeat this pattern several times:

```
s2$triple
This is a snippet of code with 2 relevant lines: ${dollar}{ snippet {
// 8<--
def factorial(n: Int): Int = if (n == 1) n else (n * factorial(n - 1))
// 8<--
factorial(3) == 6
// 8<--
val n = 4
// 8<--
factorial(n) == 24
} }
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
This is a snippet of code with a result: ${dollar}{ snippet {
factorial(3)
}.eval }
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
This is a snippet of code with a negative offset to align the code to the border of the screen: ${dollar}{ snippet {
def factorial(n: Int): Int = if (n == 1) n else (n * factorial(n - 1))
factorial(3)
}.offsetIs(-3) }
$triple
```

This displays:

```
factorial(3)
```
#### names

It is also possible to capture trait/classes or method/attribute names with the following method calls:

<table>
<tr><td>a trait name                  </td><td><code class="prettyprint">${dollar}{simpleName[SpecificationLike] } === SpecificationLike                         </code></td></tr>
<tr><td>a fully qualified trait name  </td><td><code class="prettyprint">${dollar}{fullName[SpecificationLike] }   === org.specs2.specification.SpecificationLike</code></td></tr>
<tr><td>a method/attribute name       </td><td><code class="prettyprint">${dollar}{termName(toString)}             === toString                                  </code></td></tr>
<tr><td>a function name               </td><td><code class="prettyprint">${dollar}{termName(factorial(1)}          === factorial                                 </code></td></tr>
</table>

#### outside specs2

These functionalities are accessible outside of specs2 by importing the `${fullName[org.specs2.execute.Snippets]}` trait.


"""



}
