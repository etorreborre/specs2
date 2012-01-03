package org.specs2
package guide

import _root_.examples._
import specification._
import execute.Result

class SpecStructure extends Specification { def is =
  "Specification structure".title                                                                                       ^
                                                                                                                        """
### Presentation

In this chapter you will learn how to:

 * declare examples
 * share examples
 * add arguments for execution and reporting
 * format the layout of your specification
 * include or link specifications
 * give a title to your specification
 * define contexts and actions to execute before/after examples
 * tag examples or sections of the Specification
 * adding debug statements
 * remove some implicit definitions conflicting with your code

### Declare examples

#### Structure of a Specification

The [Quick Start](org.specs2.guide.QuickStart.html) guide describes 2 styles of specifications, the _unit_ style and the _acceptance_ style.
Both styles actually build a specification as a list of *fragments*.

##### _Acceptance_ specification

In an _acceptance_ specification you build a list of _fragments_ with the `^` operator:

      "this is my specification"                          ^
        "and example 1"                                   ! e1^
        "and example 2"                                   ! e2

      def e1 = success
      def e2 = success

What we have here is a list of 3 fragments, a Text fragment and 2 Example fragments. The examples are declared using the
format `"description" ! body`. Their "bodies" are provided by 2 methods returning a `Result`, separated from the specification text.

There is no specific recommendation on how you should name those methods but you can either use short names or use the backtick
notation for better readability:

      "this is my specification"                          ^
        "and example 1"                                   ! `first example`^
        "and example 2"                                   ! `second example`

      def `first example` = success
      def `second example` = success

You can even push this idea further by writing:

      "this is my specification"                          ^
        `and example 1`                                   ^
        `and example 2`

      def `and example 1` = success
      def `and example 2` = success

*(an IDE with good refactoring capabilities is a must-have in that case,...)*

##### _Unit_ specification

A _unit_ specification uses `should/in` blocks which actually build the Fragments by adding them to a mutable variable:

      "The 'Hello world' string" should {
        "contain 11 characters" in {
          "Hello world" must have size(11)
        }
        "start with 'Hello'" in {
          "Hello world" must startWith("Hello")
        }
        "end with 'world'" in {
          "Hello world" must endWith("world")
        }
      }

In that specification the following methods are used:

 * `in` to create an Example containing a `Result`
 * `should` to create a group of Examples, with a the preceding Text fragment appended with `should`

It is completely equivalent to writing this in an `org.specs2.Specification`:

      def is =

      "The 'Hello world' string should" ^
        "contain 11 characters" ! {
          "Hello world" must have size(11)
        }^
        "start with 'Hello'" ! {
          "Hello world" must startWith("Hello")
        }^
        "end with 'world'" ! {
          "Hello world" must endWith("world")
        }

You can look at the bottom of this page for the other methods which are used to build unit specifications.

#### Results

An Example is created by following a piece of text with `!` and providing anything convertible to an `org.specs2.execute.Result`:

 * a standard result
 * a Matcher result
 * a boolean value

##### Standard results

The simplest `Result` values are provided by the `StandardResults` trait (mixed-in with `Specification`), and match the 5
types of results provided by ***specs2***:

  * success: the example is ok
  * failure: there is a non-met expectation
  * anError: a unexpected exception occurred
  * skipped: the example is skipped possibly at runtime because some conditions are not met
  * pending: usually means "not implemented yet"

Two additional results are also available to track the progress of features:

  * done: a Success with the message "DONE"
  * todo: a Pending with the message "TODO"

##### Matcher results

Usually the body of an example is made of *expectations* using matchers:

     def e1 = 1 must_== 1

You can refer to the [Matchers](org.specs2.guide.Matchers.html)  guide to learn all about matchers and how to create expectations.

##### Functional expectations

The default `Specification` trait in ***specs2*** is functional: the Result of an example is always given by the last statement
of its body. This example will never fail because the first expectation is "lost":

      "my example on strings" ! e1                // will never fail!

      def e1 = {
        "hello" must have size(10000)             // because this expectation will not be returned,...
        "hello" must startWith("hell")
      }

So the correct way of writing the example is:

      "my example on strings" ! e1               // will fail

      def e1 = "hello" must have size(10000) and
                            startWith("hell")

##### Thrown Expectations

The above functionality encourages a specification style where every expectation is carefully specified and is considered good practice
by some. However you might see it as an annoying restriction. You can avoid it by extending the `org.specs2.matcher.MustThrownMatchers`
trait. With that trait, any failing expectation will throw a `FailureException` and the rest of the example will not be executed.

There is also an additional method `failure(message)` to throw a `FailureException` at will.

[Note that the `ThrownMatchers` traits are mixed in the `mutable.Specification` trait used for _unit_ specifications].

#### Set an example as "Pending until fixed"

Some examples may be temporarily failing but you don't want the entire test suite to fail just for those examples.
Instead of commenting them out and then forgetting about those examples when the code is fixed, you can append `pendingUntilFixed`
to the Example body:

      "this example fails for now" ! {
        1 must_== 2
      }.pendingUntilFixed

      // or, with a more specific message
      "this example fails for now" ! {
        1 must_== 2
      }.pendingUntilFixed("ISSUE-123")


The example above will be reported as `Pending` until it succeeds. Then it is marked as a failure so that you can remember
to remove the `pendingUntilFixed` marker.

#### Auto-Examples

If your specification is about showing the use of a DSL or of an API, you can elid a description for the Example. This functionality is used in ***specs2*** to specify matchers:

     "beNone checks if an element is None"                             ^
     { None must beNone }                                              ^
     { Some(1) must not be none }                                      ^

In that case, the text of the example will be extracted from the source file and the output will be:

     beNone checks if an element is None
       + None must beNone
       + Some(1) must not be none

A few things to remember about this feature:

 * the source file is expected to be found in the `src/test/scala` directory.
   This can be overriden by specifying the `specs2.srcTestDir` system property

 * the extraction of the source code is rudimentary and may fail on specifications which are built dynamically

 * several lines of code can be extracted provided that the block ends with a `Result` and that there is a `Fragment` following the block to be extracted. The best way to ensure that is to always add an `end` fragment at the end of the `Specification`

 * the code to extract must be in the same directory as the package of the specification class it belongs to. If a Specification
   is declared in `package com.mycompany.accounting` then its source file has to be in the `com/mycompany/accounting` directory
   for Auto-Examples to be working

 * for more robustness, but different results, you can use the `descFromExpectations` argument (creates an
   `args(fromSource=false)` argument) to take the "ok message" from the expectation as the example description:

         // outputs: List(1, 2) must contain(1)
         { List(1, 2) must contain(1) }

         // outputs: 'List(1, 2)' contains '1'
         descFromExpectations ^
         { List(1, 2) must contain(1) }

#### Using the Example description

The Example description can be used to create an expectation in the Example body:

      "This is a long, long, long description" ! ((s: String) => s.size must be_>(10))

#### Given / When / Then

More sophisticated is the Given/When/Then style of writing specifications. This style is supported by interspersing Text fragments,
with Given/When/Then `RegexSteps` which extract meaningful values from the text. Here's an example specification for a simple
calculator:

        "A given-when-then example for the addition"                 ^
          "Given the following number: ${1}"                         ^ number1 ^
          "And a second number: ${2}"                                ^ number2 ^
          "Then I should get: ${3}"                                  ^ result ^
                                                                     end

        object number1 extends Given[Int] {
          def extract(text: String): Int = extract1(text).toInt
        }
        case class Addition(n1: Int, n2: Int) {
          def add: Int = n1 + n2
        }
        object number2 extends When[Int, Addition] {
          def extract(number1: Int, text: String) = Addition(number1, extract1(text).toInt)
        }
        object result extends Then[Addition] {
          def extract(addition: Addition, text: String): Result = addition.add must_== extract1(text).toInt
        }

Here's some explanation of the object definitions that support the G/W/T style:

 * `number1` is a `Given` step. It is parametrized with the type `Int` meaning that its `extract` method is supposed to extract
   an Int from the preceding text. It does so by using the `extract1` inherited method, which parses the text for `${}` expressions
   and return a tuple (with 1 element here) containing all the values enclosed in `${}`.

 * `number2` is a `When` step. It is paramerized with an `Int`, the result from the previous extraction, and an `Addition`
   which is the result of extracting the second number and putting the 2 together. In that case the method which must be
   defined is `extract(Int, String): Addition`.

 * finally the `result` object defines the outcome of the Addition. Its `extract` method takes an `Addition` and the current
   text to return a `Result`

##### Multiple steps

A G/W/T sequence can contain more than just 3 steps. However the compiler will check that:

   * only a `Given[T]` extractor can start a sequence
   * only a `When[T, S]` or a `Then[T]` extractor can follow a `Given[T]` extractor
   * only a `When[S, U]` extractor or a `Then[S]` can follow a `When[T, S]` extractor
   * only a `Then[S]` can follow a `Then[S]` extractor

To be more concrete, here are a few valid sequences:

   * Given[T] / When[T, S] / Then[S]
   * Given[T] / When[T, S] / Then[S] / Then[S]
   * Given[T] / Then[T] / Then[T]
   * Given[T] / When[T, S] / When[S, U] / Then[U]

##### Extract methods

The `Given`, `When`, `Then` classes provide several convenience methods to extract strings from the preceding text: the
`extract1, extract2,...` methods will extracts the values delimited by `${}` for up to 10 values.

##### User regexps

In the original way of declaring Given/When/Then steps, the text is left completely void of markers to extract meaningful
values. The user then needs to specify a regular expression where groups are used to show where those values are:

        object number1 extends Given[Int]("Given the following number: (.*)") {
          def extract(text: String): Int = extract1(text).toInt
        }

The advantage of using this way is that the text is left in it's pristine form, the drawback is that most of the text is
duplicated in 2 places, adding more maintenance burden.

##### Several G/W/T blocks

Given the rule saying that only a `Then` block can follow another `Then` block you might think that it is not possible to
start another G/W/T sequence in the same specification! Fortunately it is possible by just terminating the first sequence
with an `end` fragment:

        "A given-when-then example for the addition"                 ^
          "Given the following number: ${1}"                         ^ number1 ^
          "And a second number: ${2}"                                ^ number2 ^
          "Then I should get: ${3}"                                  ^ addition ^
                                                                     end^
        "A given-when-then example for the multiplication"           ^
          "Given the following number: ${1}"                         ^ number1 ^
          "And a second number: ${2}"                                ^ number2 ^
          "Then I should get: ${2}"                                  ^ multiplication ^
                                                                     end

##### ScalaCheck values

Once you've created a given G/W/T sequence, you can be tempted to copy and paste it in order to check the same scenario
with different values. The trouble with this is the duplication of text which leads to more maintenance down the road.

This can be avoided and even enhanced by using ScalaCheck to generate more values for the same scenario. For the calculator
above you could write:

        import org.scalacheck.Gen._
        import specification.gen._

        class GivenWhenThenScalacheckSpec extends Specification with ScalaCheck { def is =

          "A given-when-then example for a calculator"                                   ^
            "Given a first number n1"                                                    ^ number1 ^
            "And a second number n2"                                                     ^ number2 ^
            "When I add them"                                                            ^ add ^
            "Then I should get n1 + n2"                                                  ^ result ^
                                                                                         end^

          object number1 extends Given[Int] {
            def extract(text: String) = choose(-10, 10)
          }
          object number2 extends When[Int, (Int, Int)] {
            def extract(number1: Int, text: String) = for { n2 <- choose(-10, 10) } yield (number1, n2)
          }
          object add extends When[(Int, Int), Addition] {
            def extract(numbers: (Int, Int), text: String) = Addition(numbers._1, numbers._2)
          }
          object mult extends When[(Int, Int), Multiplication] {
            def extract(numbers: (Int, Int), text: String) = Multiplication(numbers._1, numbers._2)
          }
          object result extends Then[Addition] {
            def extract(text: String)(implicit op: Arbitrary[Addition]) = {
              check { (op: Addition) => op.calculate must_== op.n1 + op.n2 }
            }
          }
          case class Addition(n1: Int, n2: Int) extends Operation { def calculate: Int = n1 + n2 }
        }

The main differences with a "normal" G/W/T sequence are:

 * the import of step classes from `org.specs2.specification.gen` instead of `org.specs2.specification`
 * the return values from the `extract` methods of the `Given` and `When` steps which must return ScalaCheck generators
   (cf `number1` and `number2`. For the `add` step there is an implicit conversion transforming any value of type `T` to a
   `Gen[T]`
 * the use of the ScalaCheck trait to access the `check` function transforming a function to a `org.scalacheck.Prop` and then
   to a `Result`
 * the `extract` method of the `Then` step takes an implicit `Arbitrary[T]` parameter which is used by the `check` method
   to create a ScalaCheck property

##### Single step

A `GivenThen` step can be used to extract values from a single piece of text and return a `Result`:

    "given the name: ${eric}, then the age is ${18}" ! new GivenThen {
      def extract(text: String) = {
        val (name, age) = extract2(text)
        age.toInt must_== 18
      }
    }

You can also use the `so` object. This object provides an `apply` method expecting a `PartialFunction` and does the value
extraction:

    import org.specs2.specification.so

    "given the name: ${eric}, then the age is ${18}" ! so { case (name: String, age: String) =>
      age.toInt must_== 18
    }

##### Conversions

Given / When / Then steps are invariant in their type parameters. This might be detrimental to reuse. For example, if you've defined a `Then[X]` step to check something about a value of type `X`, it would make sense to reuse the same step with a value of type `Y` when `Y <: X`. In order to do this you can use some implicit conversions which will translate steps between types when it makes sense:

      val thenX = new Then[X] {
        def extract(x: X, s: String) = success // check something about x
      }
      // thenX can be reused as a Then[Y] step because Y <: X
      val thenY: Then[Y] = thenX

### Shared examples

In a given specification some examples may look similar enough that you would like to "factor" them out and share them between
different parts of your specification. The best example of this situation is a specification for a Stack of limited size:

        class StackSpec extends Specification { def is =
          "Specification for a Stack with a limited capacity".title                 ^
                                                                                    p^
          "An empty stack should"                                                   ^
            "behave like an empty stack"                                            ^ isEmpty^
                                                                                    endp^
          "A non-empty stack should"                                                ^
            "behave like a non empty stack"                                         ^ isNonEmpty(normal)^
                                                                                    endp^
          "A stack below full capacity should"                                      ^
            "behave like a non empty stack"                                         ^ isNonEmpty(normal)^
            "behave like a stack below capacity"                                    ^ isNotFull(normal)^
                                                                                    endp^
          "A full stack should"                                                     ^
            "behave like a non empty stack"                                         ^ isNonEmpty(full)^
            "behave like a full stack"                                              ^ isFull(full)^
                                                                                    end

          def normal = Stack(10, 2)
          def full = Stack(10, 10)

          def isEmpty =
            "throw an exception when sent #top"                                     ! empty().e1^
            "throw an exception when sent #pop"                                     ! empty().e2

          def isNonEmpty(s: =>SizedStack) =
            "not be empty"                                                          ! nonempty(s).size^
            "return the top item when sent #top"                                    ! nonempty(s).top1^
            "not remove the top item when sent #top"                                ! nonempty(s).top2^
            "return the top item when sent #pop"                                    ! nonempty(s).pop1^
            "remove the top item when sent #pop"                                    ! nonempty(s).pop2

          def isNotFull(s: =>SizedStack) =
            "add to the top when sent #push"                                        ! notfull(s).e1

          def isFull(s: =>SizedStack) =
            "throw an exception when sent #push"                                    ! fullStack(s).e1

          case class empty() {
            val stack = new SizedStack(10)
            def e1 = stack.top must throwA[NoSuchElementException]
            def e2 = stack.pop must throwA[NoSuchElementException]
          }
          case class nonempty(stack: SizedStack) {
            def size = !stack.isEmpty
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
          case class notfull(stack: SizedStack) {
            def e1 = {
              stack push (stack.size + 1)
              stack.top must_== stack.size
            }
          }
          case class fullStack(stack: SizedStack) {
            def e1 = stack push (stack.size + 1) must throwAn[Error]
          }
        }

### Declare arguments

At the beginning of a specification you can declare arguments which configure the execution and reporting of the specification.
For example, you can turn off the concurrent execution of examples with the `args` method:

       class ExamplesOneByOne extends Specification { def is =

         // there is a shortcut for this argument called 'sequential'
         args(sequential=true)              ^
         "first example"                    ! e1 ^
         "the the second one"               ! e2 ^
                                            end
       }

For the complete list of arguments and shortcut methods read the [Runners](org.specs2.guide.Runners.html) page.

### Layout

For an _acceptance_ specification you can tweak the layout of Texts and Examples.

##### The rules

The layout of text in ***specs2*** is mostly done automatically so that the text in the source code should look like the displayed text after execution.

By default the layout of a specification will be computed automatically based on intuitive rules:

  * when an example follows a text, it is indented
  * 2 successive examples will be at the same indentation level
  * when a text follows an example, this means that you want to describe a "subcontext", so the next examples will be indented with one more level

Let's see a standard example of this. The following fragments:

    "this is some presentation text"      ^
      "and the first example"             ! success^
      "and the second example"            ! success

will be executed and displayed as:

    this is some presentation text
    + and the first example
    + and the second example

If you specify a "subcontext", you will get one more indentation level:

    "this is some presentation text"      ^
      "and the first example"             ! success^
      "and the second example"            ! success^
      "and in this specific context"      ^
        "one more example"                ! success^

will be executed and displayed as:

    this is some presentation text
    + and the first example
    + and the second example
      and in this specific context
      + one more example

##### The formatting fragments

Given the rules above, you might need to use some *formatting fragments* to adjust the display

###### Separating groups of examples

The best way to separate blocks of examples is to add a blank line between them by using `p` (as in "paragraph"):

    "this is some presentation text"      ^
      "and the first example"             ! success^
      "and the second example"            ! success^
                                          p^
    "And another block of examples"       ^
      "with this example"                 ! success^
      "and that example"                  ! success

This will be displayed as:

    this is some presentation text
    + and the first example
    + and the second example

    And another block of examples
    + with this example
    + and that example

That looks remarkably similar to the specification code, doesn't it? What `p` does is:

 * add a blank line (this can also be done with a simple `br`)
 * decrement the current indentation level by 1 (Otherwise the new Text would be seen as a subcontext)

###### Reset the levels

When you start having deep levels of indentation, you might need to start the next group of examples at level 0. For example, in this specification

    "There are several options for displaying the text"      ^
      "xonly displays nothing but failures"                  ! success^
      "there is also a color option"                         ^
        "rgb=value uses that value to color the text"        ! rgb^
        "nocolor dont color anything"                        ! nocolor^
                                                             p^
    "There are different ways of hiding the text"            ^
        "by tagging the text"                                ! hideTag

Even with `p` the next group of examples will not start at level 0. What you need to do in that case is use `end`:

    "There are several options for displaying the text"      ^
      "xonly displays nothing but failures"                  ! success^
      "there is also a color option"                         ^              // this text will be indented
        "rgb=value uses that value to color the text"        ! rgb^         // and the following examples as well
        "nocolor dont color anything"                        ! nocolor^
                                                             end^
    "There are different ways of hiding the text"            ^              // this text will be properly indented now
      "by tagging the text"                                  ! hideTag^
                                                             end

This will be displayed as:

    There are several options for displaying the text
    + xonly displays nothing but failures
      there is also a color option
      + rgb=value uses that value to color the text
      + nocolor dont color anything
    There are different ways of hiding the text
    + by tagging the text

And if you want to reset the indentation level *and* add a blank line you can use `end ^ br` (or `endbr` as seen in "Combinations" below).

###### Changing the indentation level

If, for whatever reason, you wish to have more or less indentation, you can use the `t` and `bt` fragments (as in "tab" and "backtab"):

    "this text"                                     ^ bt^
    "doesn't actually have an indented example"     ! success

    "this text"                                     ^ t^
        "has a very indented example"               ! success

 The number of indentation levels (characterized as 2 spaces on screen) can also be specified by using `t(n)` or `bt(n)`.

###### Combinations

Some formatting elements can be combined:

 * `p` is actually `br ^ bt`
 * `endbr` is `end ^ br`
 * `endp` is `end ^ p`  (same effect as `endbr` but shorter :-))

###### Turning-off the automatic layout

You can turn off that automatic layout by adding the `noindent` argument at the beginning of your specification:

      class MySpecWithNoIndent extends Specification {
        def is = noindent ^ ....
      }

### Include or link specifications

###### Include specifications

There is a simple mechanism for including "children" specification in a given specification. You can simply add the child specification as if it was a simple fragment:

    "This is an included specification"     ^
      childSpec

Otherwise, if you want to include several specifications at once you can use the `include` method:

    "This is the included specifications"         ^
      include(childSpec1, childSpec2, childSpec3)


The effect of doing so is that all the fragments of the children specification will be inlined in the parent one. This is exactly what is done in this page of the user guide, but with a twist

    include(xonly, new GivenWhenThenSpec)        ^
    include(xonly, exampleTextIndentation)       ^
    include(xonly, resetTextIndentation)         ^

In this case I give specific arguments to the included specification so that it is only displayed when there are failures.

###### Link specifications

In order to create a User Guide such as this one, you might want to have the "included" specification being written to another html file. The syntax to do this is the following:

    "a " ~ ("quick start guide", new QuickStart)                                            ^
    "how to " ~ ("structure your specification", new SpecStructure)                         ^
    "how to use " ~ ("matchers", new Matchers)                                              ^
    "how to use " ~ ("mock objects", new Mocks)                                             ^

In this case the `~` operator is used to create a `HtmlLink` where:

 * "a" is the beginning of the text
 * "quick start guide" is the text that will be highlighted as a url link
 * `new QuickStart` is the specification to include, the url being derived from the specification class name

Several variations are possible on this pattern, depending which part of the link you want to be highlighted:

     "before text" ~ ("text to highlight", specification, "after text")
     "before text" ~ ("text to highlight", specification, "after text", "tooltip")
     "text to highlight" ~ specification
     "text to highlight" ~ (specification, "after text")
     "text to highlight" ~ (specification, "after text", "tooltip")
     link(specification)

It is also desirable sometimes to create a page with links to other specifications where the linked specifications will not give raise to the creation of subsequent pages. There will be just html links:

     "before text" ~/ ("text to highlight", specification, "after text")
     "before text" ~/ ("text to highlight", specification, "after text", "tooltip")
     "text to highlight" ~/ specification
     "text to highlight" ~/ (specification, "after text")
     "text to highlight" ~/ (specification, "after text", "tooltip")
     see(specification)

### Specification title

Usually the title of a specification is derived from the specification class name. However if you want to give a more readable name to your specification report you can do the following:

     class MySpec extends Specification { def is =
        "My beautiful specifications".title                           ^
                                                                      p^
        "The rest of the spec goes here"                              ^ end
     }

The title can be defined either:

 * at the beginning of the specification
 * just after the arguments of the specification

### Index page

Here's something you can do to automatically create an index page for your specifications:

      import org.specs2._
      import runner.SpecificationsFinder._

      class index extends Specification { def is =

        examplesLinks("Example specifications")
        
				// see the SpecificationsFinder trait for the parameters of the 'specifications' method
        def examplesLinks(t: String) = specifications().foldLeft(t.title) { (res, cur) => res ^ see(cur) }
        
      }

The specification above creates an index.html file in the `target/specs2-reports` directory. The specifications method
creates specifications using the following parameters:

 * `path`: glob pattern to filter specification files. Default value is `**/*.scala`
 * `pattern`: pattern to use when trying to retrieve the specification names from the source files. Default value = `.*Spec`
 * `filter`: function to keep only some specifications depending on their name. Default value = `(name: String) => true`
 * `basePath`: the path where to start the search. Default value: the `specs2.srcTestDir` system value = `src/test/scala`
 * `verbose`: boolean indicating if information about finding files and specifications must be printed. Default value = `false`

### Contexts

In a specification some examples are very simple and just check that a function is behaving as expected. However other examples can be more complex and require a more elaborate set-up of data to:

 * to create inter-related domain objects
 * to put the environment (database, filesystem, external system) in the appropriate state

And there are usually 3 difficulties in doing that:

 1. _Variables isolation_: making sure that each example can be executed with its own data without being impacted by the undesired side-effects of other examples
 1. _Before/After code_: running code before or after every example without repeating that code in the body of each example
 1. _Global setup/teardown code_: setting some state when this could take lots of resources, so you need to do it just once before anything runs

How does a library like [JUnit](http://junit.org/) solves this?

 1. _Variables isolation_: for each test run a new class instance is created so that there are new "fresh" variables for the current test case
 1. _Before/After code_: there are `@Before` and `@After` annotations to declare once the code that must be executed before or after each example
 1. _Global setup/teardown code_: there are `@BeforeClass` and `@AfterClass` annotations dedicated to that kind of code

Now let's see how this can be achieved with ***specs2***.

#### Variables isolation

***specs2*** solves this issue simply by relying on Scala features: creating a new Trait or a case class opens a new scope with fresh variables!

Let's see an example with a mutable specification:

       class ContextSpec extends mutable.Specification {
         "this is the first example" in new trees {
           tree.removeNodes(2, 3) must have size(2)
         }
         "this is the first example" in new trees {
           tree.removeNodes(2, 3, 4) must have size(1)
         }
       }

       /** the `trees` context */
       trait trees extends Scope {
         val tree = createATreeWith4Nodes
       }

Each example of that specification gets a new instance of the `trees` trait. So it will have a brand new `tree` variable and even if this data is mutated by an example, other examples will be isolated from these changes.

##### `Scope`

You might wonder why the `trees` trait is extending the `org.specs2.specification.Scope` trait? The reason is that the body of an Example only accepts objects which are convertible to a `Result`. By extending `Scope` we can take advantage of an implicit conversion provided by the `Specification` trait to convert our context object to a `Result`.

##### With acceptance specifications

The same kind of variable isolation can be achieved in acceptance specifications by using case classes:

       class ContextSpec extends Specification { def is =
         "this is the first example" ! trees().e1 ^
         "this is the first example" ! trees().e2
       }

       case class trees() {
         val tree = createATreeWith4Nodes

         def e1 = tree.removeNodes(2, 3) must have size(2)
         def e2 = tree.removeNodes(2, 3, 4) must have size(1)
       }

In this case we don't need to extend the `Scope` trait because the examples `e1` and `e2` already return `Result`s.

##### Contexts inheritance

One very cool property of using traits to define context variables is that we can use inheritance to describe more and more specific contexts:

      trait LoggedIn extends Scope {
        val user = logInUser
        // do something with the user
      }

      trait HasAPendingOrder extends LoggedIn {
        val order = createPendingOrder
        // the user is logged in
        // now do something with the user and his order
      }

#### Before/After code

If you want to run some code before or after each example, the `Before` and `After` traits are there to help you (they both extend the `Scope` trait). In the following examples we'll only show the use of `After` because `Before` most of the time unnecessary:

       class ContextSpec extends mutable.Specification {
         "this is the first example" in new trees {
           tree.removeNodes(2, 3) must have size(2)
         }
         "this is the first example" in new trees {
           tree.removeNodes(2, 3, 4) must have size(1)
         }
       }

       trait trees {
         setupDB
         lazy val tree = getATreeWith4NodesFromTheDatabase
       }

Indeed when you have setup code you can do anything you want in the body of your context trait and this will be executed before the example body. However this wouldn't work with teardown code, so let's see how to use the `After` trait.

##### In a mutable specification

You make your context trait extend the `mutable.After` trait:

       class ContextSpec extends mutable.Specification {
         "this is the first example" in new trees {
           tree.removeNodes(2, 3) must have size(2)
         }
         "this is the first example" in new trees {
           tree.removeNodes(2, 3, 4) must have size(1)
         }
       }

       trait trees extends mutable.After {
         lazy val tree = getATreeWith4NodesFromTheDatabase
         def after = cleanupDB
       }

In this case, the clean-up code defined in the `after` method will be executed after each example. This is possible because the `mutable.After` trait extends the Scala `DelayedInit` trait allowing to insert code around the execution of the body of an object.

**Note**: the `org.specs2.mutable.{ Before, After, BeforeAfter }` traits only work for scala > 2.9.0 because previous Scala versions don't provide the `DelayedInit` trait.

##### In an acceptance specification

In that case you would extend the `specification.After` trait and use the `apply` method:

       class ContextSpec extends Specification { def is =
         "this is the first example" ! trees().e1 ^
         "this is the first example" ! trees().e2
       }

       case class trees() extends specification.After {
         lazy val tree = getATreeWith4NodesFromTheDatabase
         def after = cleanupDB

         // this is equivalent to: def e1 = this.apply { ... }
         def e1 = this { tree.removeNodes(2, 3) must have size(2) }
         def e2 = this { tree.removeNodes(2, 3, 4) must have size(1) }
       }

Now we have both variable isolation and non-duplication of set-up code!

But there is more to it. The next paragraphs will show how to:

 1. execute the body of each example inside a specific context: `Around`
 1. set-up a context object (say a http query) and pass it to each example: `Outside`
 1. declare a `before` method for all the examples of a Specification without even having to create a context object
 1. use an implicit context to avoid duplication
 1. create a new context object by combining existing ones

#### `Around` actions

Some examples need to be executed in a given context. For example you're testing a web application and your specification code needs to have your example executed inside an Http session.

In that case you can extend the `Around` trait and specify the `around` method:

      object http extends Around {
        def around[T <% Result](t: =>T) = openHttpSession("test") {
          t  // execute t inside a http session
        }
      }

      "this is a first example where the code executes inside a http session" ! http(e1)
      "and another one"                                                       ! http(e2)

Note that the context here is an object instead of a trait or case class instance because in this specification we don't need any variable isolation. We also take the advantage that objects extending `Context` traits (like `Before` / `After` / `Around`,...) have an `apply` method so we can directly write `http(e1)` meaning `http.apply(e1)`.

#### `Outside` actions

`Outside` is bit like `Around` except that you can get access to the application state that you're setting in your Context object. Let's see that with an example (with a mutable Specification for a change):

      object http extends Outside[HttpReq] with Scope {
        // prepare a valid HttpRequest
        def outside: HttpReq = createRequest
      }

      // use the http request in each example
      "this is a first example where the code executes uses a http request" in http { (request: HttpReq) =>
        success
      }
      "and another one" in http { (request: HttpReq) =>
        success
      }

##### `AroundOutside` actions

We can also combine both the `Around` and the `Outside` behaviors with the `AroundOutside` trait:

      object http extends AroundOutside[HttpReq] {
        // create a context
        def around[T <% Result](t: =>T) = {
          createNewDatabase
          // execute the code inside a databaseSession
          inDatabaseSession { t }
        }
        // prepare a valid HttpRequest
        def outside: HttpReq = createRequest
      }

      "this is a first example where the code executes uses a http request" ! http((request: HttpReq) => success)
      "and another one"                                                     ! http((request: HttpReq) => success)

#### Without creating a context object: `BeforeExample`

When you just need to have set-up code executed before each example and if you don't need to have variable isolation, you can simply use the `BeforeExample` trait.

The `BeforeExample` trait allows you to define a `before` method exactly like the one you define in the `Before` trait and apply it to all the examples of the specification:

        class MySpecification extends mutable.Specification with BeforeExample {
          def before = cleanDatabase

          "This is a specification where the database is cleaned up before each example" >> {
            "first example" in { success }
            "second example" in { success }
          }
        }

As you can guess, the `AfterExample`, `AroundExample`,... traits work similarly by requiring the corresponding `after`, `around`,... methods to be defined.

#### Implicit context

The `BeforeExample` trait is a nice shortcut to avoid the creation of a context object, but there is another possibility to avoid the repetition of the context name for each example. If your specification is:

        class ContextSpec extends mutable.Specification {
          object myContext = new Before { def before = cleanUp }

          "This is a specification where the database is cleaned up before each example" >> {
            "first example" in myContext { 1 must_== 1 }
            "second example" in myContext { 1 must_== 1 }
          }
        }

You can simply mark your context object as `implicit` and it will be automatically passed to each example:

        class ContextSpec extends mutable.Specification {
          implicit object myContext = new Before { def before = cleanUp }

          "This is a specification where the database is cleaned up before each example" >> {
            "first example"  in { 1 must_== 1 }
            "second example" in { 1 must_== 1 }
          }
        }

There is just one gotcha that you need to be aware of. If your implicit context is an `Outside[String]` context this will not work:

        class ContextSpec extends mutable.Specification {
          implicit object myContext = new Outside[String] { def outside = "hello" }

          "This is a specification uses a new String in each example" >> {
            "first example"  in { (s: String) => s must_== s }
            "second example" in { (s: String) => s must_== s }
          }
        }

Indeed in both examples above the `s` string that will be passed is the Example description as specified [here](http://etorreborre.github.com/specs2/guide/org.specs2.guide.SpecStructure.html#Using+the+Example+description).

#### Composing contexts

##### Combinations

***specs2*** contexts can be combined in several ways. When you want to define both `Before` and `After` behavior, you can do it by simply extending those 2 traits:

    case class withFile extends Before with After {
      def before = createFile("test")
      def after  = deleteFile("test")
    }

But, as we've seen with the `AroundOutside` example, ***specs2*** likes to help save keystrokes so you can directly extend the `BeforeAfter` trait:

    case class withFile extends BeforeAfter {
      def before = createFile("test")
      def after  = deleteFile("test")
    }

Similarly you can use `BeforeAfterAround` instead of `Before with After with Around`.

##### Composition

Contexts can be also be _composed_ but only if they are of the same type, `Before` with `Before`, `After` with `After`,...

    case class withFile extends Before {
      def before = createFile("test")
    }
    case class withDatabase extends Before {
      def before = openDatabase("test")
    }
    val init = withFile() compose withDatabase()

    "Do something on the full system" ! init(success)

#### Steps and Actions

##### Steps

Some set-up actions are very time-consuming and should be executed only once for the whole specification. This can be achieved by inserting some silent `Step`s in between fragments:

    class DatabaseSpec extends Specification { def is =

      "This specification opens a database and execute some tests"     ^ Step(openDatabase) ^
        "example 1"                                                    ! success ^
        "example 2"                                                    ! success ^
                                                                       Step(closeDatabase)^
                                                                       end
    }

The examples are (by default) executed concurrently between the 2 steps and the "result" of those steps will never be reported unless if there is a failure.

##### Actions

`Step`s are very useful because they will really be executed sequentially, before anything else, but if you need to execute some actions which are completely independent of the rest of the specification, there is an equivalent to `Step` adequately called `Action`:

    class DatabaseSpec extends Specification { def is =

      "This specification opens a database and execute some tests"     ^ Step(openDatabase) ^
        "example 1"                                                    ! success ^
        "add 1 to the number of specification executions"              ^ Action(db.executionsNb += 1)^
        "example 2"                                                    ! success ^
                                                                       Step(closeDatabase)^
                                                                       end
    }

Of course, `Step`s and `Action`s are not the privilege of acceptance specifications:

    class DatabaseSpec extends mutable.Specification {

      textFragment("This specification opens a database and execute some tests")
      step(openDatabase)

      "example 1" in success

      textFragment("add 1 to the number of specification executions")
      action(db.executionsNb += 1)

      "example 2" in success
      step(closeDatabase)
    }


#### Generic specification with setup and teardown steps

There may still be some duplication of code if you have to use the same kind of set-up procedure for several specifications.

If that's the case you can define your own Specification trait doing the job:

        import org.specs2._
        import specification._

        trait DatabaseSpec extends Specification {
          /** the map method allows to "post-process" the fragments after their creation */
          override def map(fs: =>Fragments) = Step(startDb) ^ fs ^ Step(cleanDb)
        }

The `DatabaseSpec` above will insert, in each inherited specification, one `Step` executed before all the fragments, and one executed after all of them.

### Other unit specification methods

Other methods can be used to create fragments in a unit specification:

 * `can` to create a group of Examples, with the preceding Text fragment appended with `can`
 * <code>>></code> to create an Example or a group of Examples (with no appended text)
 * `"My spec title".title` to give a title to the Specification
 * `args(...)` to create arguments for the specification
 * `textFragment(s)` to create a `Text` fragment
 * `step(s)` to create a `Step`
 * `action(a)` to create an `Action`
 * `link("how" ~ ("to do hello world", new HelloWorldSpec))` to create a link to another specification
 * `see(new HelloWorldSpec)` to add a link to another specification without including its fragments for execution
 * `include(new HelloWorldSpec)` to include another specification

To make things more concrete here is a full example:

      import mutable._
      import specification._
      import execute.Success

      /**
       * This specification shows how to use the mutable.Specification trait to create a unit Specification
       * where the fragments are built using a mutable variable
       */
      class MutableSpec extends Specification {

        // A title can be added at the beginning of the specification
        "MutableSpec".title
        // arguments are simply declared at the beginning of the specification if needed
        args(xonly=true)

        // a step to execute before the specification must be declared first
        step {
          // setup your data or initialize your database here
          success
        }

        "'Hello world'" should {
          "contain 11 characters" in {
            "Hello world" must have size(11)
          }
          "start with 'Hello'" in {
            "Hello world" must startWith("Hello")
          }
          /**
           * a failing example will stop right away, without having to "chain" expectations
           */
          "with 'world'" in {
            // Expectations are throwing exception by default so uncommenting this line will
            // stop the execution right away with a Failure
            // "Hello world" must startWith("Hi")

            "Hello world" must endWith("world")
          }
        }
        /**
         * "Context management" is handled through the use of traits or case classes
         */
        "'Hey you'" should {
          // this one uses a "before" method
          "contain 7 characters" in context {
            "Hey you" must have size(7)
          }
          // System is a Success result. If the expectations fail when building the object, the example will fail
          "contain 7 characters" in new system {
            string must have size(7)
          }
          // otherwise a case class can be used but the example body will be further down the file
          "contain 7 characters" in system2().e1
        }
        // you can add links to other specifications with `link`
        // they will be executed when this one is executed. If you don't want this to happen
        // you can use `see` instead of `link`
        link("how" ~ ("to do hello world", new HelloWorldSpec))
        // you can include other specifications with `include`
        include(new HelloWorldSpec)

        // a step to execute after the specification must be declared at the end
        step {
          // close the database here
          success
        }


        object context extends Before {
          def before = () // do something to setup the context
        }
        // we need to extend Scope to be used as an Example body
        trait system extends Scope {
          val string = "Hey you"
        }
        case class system2() {
          val string = "Hey you"
          def e1 = string must have size(7)
        }
      }

### Tags

Tags can be used in a Specification to include or exclude some examples or a complete section of fragments from the execution. Let's have a look at one example:

        /**
         * use the org.specs2.specification.Tags trait to define tags and sections
         */
        class TaggedSpecification extends Specification with Tags { def is =
          "this is some introductory text"                          ^
          "and the first group of examples"                         ^
            "example 1"                                             ! success ^ tag("feature 1", "unit")^
            "example 2"                                             ! success ^ tag("integration")^
                                                                    ^ p^
          "and the second group of examples"                        ^          section("checkin")^
            "example 3"                                             ! success^
            "example 4"                                             ! success^ section("checkin")
        }

In that specification we're defining several tags and sections:

 * `feature 1` is a tag that's applied to `example1` (the _preceding_ Fragment)
 * `feature 2` is a tag that's applied to `example2` (the _preceding_ Fragment)
 * `checkin` marks a section which goes from the Text `and the second group of examples` to `example 4`

Armed with this, it is now easy to include or exclude portions of the specification at execution time:

 * `args(include="feature 1")` will only include `example 1`
 * `args(exclude="integration")` will include everything except `example 2`
 * `args(include="checkin,unit")` will include `example 1` and the second group of examples (`example 3` and `example 4`)

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

### Adding debug statements

When quick and hacky `println` statements are what you want, the `Debug` trait, mixed in every `Specification`, provides useful methods:

 * `pp` or "print and pass", prints a value to the console, then return it to be used in the rest of the expression: "graph.pp must haveSize(3)"
 * `pp(condition)` prints a value if a condition holds
 * `pp(f: T => Boolean)` prints a value if a condition on that value holds

### Removing implicit definitions

By default, the `Specification` trait imports quite a few implicit definitions (following a "batteries included" approach). However there might be some conflicts with implicits existing in your own user code. Among the usual examples of conflicts are conflicts with the `===` sign in Scalaz and the `Duration` methods in Akka.

An easy way to avoid this situation is to "deactivate" the specs2 implicits by mixing-in the relevant trait from this list:

 * `org.specs2.control.NoDebug`: deactivate the `pp` method on objects
 * `org.specs2.time.NoTimeConversions`: deactivate the `millis`, `seconds`,... methods on `Int`s and `Long`s
 * `org.specs2.main.NoArgProperties`: deactivate the `toOption: Option[T]` method on any value of type `T`
 * `org.specs2.matcher.NoCanBeEqual`: deactivate the `===` method on any type `T`
 * `org.specs2.matcher.NoMustExpectations`: deactivate the `must`, `must_==`,... methods on any value of type `T`
 * `org.specs2.matcher.NoShouldExpectations`: deactivate the `should`, `should_==`,... methods on any value of type `T`
 * `org.specs2.specification.NoBooleanAutoExamples`: deactivate the conversions from `Boolean` to `Fragment` or `Example`, which allows to use the `^` method on booleans for example
 * `org.specs2.specification.NoFragmentsBuilder`: deactivate the implicit conversions from `String` to `Fragment`s
 * `org.specs2.specification.mutable.NoFragmentsBuilder`: deactivate the implicit conversions from to remove `in`, `>>`, `should` and `can` methods from `String`s

  - - -
                                                                                                                        """^
                                                                                                                        br^
  include(xonly, new GivenWhenThenSpec)                                                                                 ^
  include(xonly, exampleTextIndentation)                                                                                ^
  include(xonly, resetTextIndentation)                                                                                  ^
  include(xonly, pTextIndentation)                                                                                      ^
  include(xonly, databaseSpec)                                                                                          ^
                                                                                                                        end

  val exampleTextIndentation = new Specification { def is =
    "Text indentation".title              ^
    "this is some presentation text"      ^
      "and the first example"             ! success^
      "and the second example"            ! success
  }

  val resetTextIndentation = new Specification { def is =
    "Reset indentation".title                                ^
    "There are several options for displaying the text"      ^
      "xonly displays nothing but failures"                  ! success^
      "there is also a color option"                         ^              // this text will be indented
        "rgb=value uses that value to color the text"        ! rgb^         // and the following examples as well
        "nocolor dont color anything"                        ! nocolors^ end^
    "There are different ways of hiding the text"            ^              // this text will be properly indented now
        "by tagging the text"                                ! hideTag^
                                                             end
    def rgb = success
    def nocolors = success
    def hideTag = success
  }

  val pTextIndentation = new Specification { def is =
    "Text paragraph".title                ^
    "this is some presentation text"      ^
      "and the first example"             ! success^
      "and the second example"            ! success^
                                          p^
    "And another block of examples"       ^
      "with this example"                 ! success^
      "and that example"                  ! success^
                                          end
  }

  val databaseSpec = new Specification { def is =
    "Database specification".title                                   ^
    "This specification opens a database and execute some tests"     ^
                                                                     Step(openDatabase) ^
      "example 1"                                                    ! success ^
      "example 2"                                                    ! success ^
                                                                     Step(closeDatabase)^
                                                                     end
    def openDatabase = success
    def closeDatabase = success
  }
  val pendindUntilFixedSpec = new Specification { def is =
    "this example fails for now" ! {
      1 must_== 2
    }.pendingUntilFixed^
    "this example fails for now" ! {
      1 must_== 2
    }.pendingUntilFixed("ISSUE-123")^
    end
  }
}
