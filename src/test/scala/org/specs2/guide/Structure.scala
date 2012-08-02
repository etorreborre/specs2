package org.specs2
package guide

import _root_.examples._
import matcher.{ ResultMatchers => RM }
import specification.{Step, Given, When, Then}

class Structure extends UserGuidePage { def is =
  """
### Presentation

In this page you will learn how to:

 * declare examples and expectations
 * link specifications together
 * define contexts and actions to execute before/after examples
 * specify the execution strategy
 * layout the specification text

### Declare examples

#### Styles

The [Quick Start](org.specs2.guide.QuickStart.html) guide describes 2 styles of specifications, the _unit_ style and the _acceptance_ style. Both styles actually build a specification as a list of *fragments*.

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

A _unit_ specification uses `should/in` blocks which build the Fragments by adding them to a mutable protected variable:

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

The [Unit specifications](#Unit+specifications) section shows all the methods which can be used to build unit specifications fragments.

#### Results

An Example is created by following a piece of text with `!` and providing anything convertible to an `org.specs2.execute.Result`:

 * a standard result
 * a Matcher result
 * a boolean value

##### Standard

The simplest `Result` values are provided by the `StandardResults` trait (mixed-in with `Specification`), and match the 5
types of results provided by ***specs2***:

  * `success`: the example is ok
  * `failure`: there is a non-met expectation
  * `anError`: a unexpected exception occurred
  * `skipped`: the example is skipped possibly at runtime because some conditions are not met
  * `pending`: usually means "not implemented yet"

Two additional results are also available to track the progress of features:

  * `done`: a `Success` with the message "DONE"
  * `todo`: a `Pending` with the message "TODO"

##### Matchers

Usually the body of an example is made of *expectations* using matchers:

     def e1 = 1 must_== 1

You can refer to the [Matchers](org.specs2.guide.Matchers.html)  guide to learn all about matchers and how to create expectations.

#### Expectations

##### Functional

The default `Specification` trait in ***specs2*** is functional: the Result of an example is always given by the last statement of its body. This example will never fail because the first expectation is "lost":

      "my example on strings" ! e1                // will never fail!

      def e1 = {
        "hello" must have size(10000)             // because this expectation will not be returned,...
        "hello" must startWith("hell")
      }

So the correct way of writing the example is:

      "my example on strings" ! e1               // will fail

      def e1 = "hello" must have size(10000) and
                            startWith("hell")

##### Thrown

The above functionality encourages a specification style where every expectation is carefully specified and is considered good practice by some. However you might see it as an annoying restriction. You can avoid it by mixing-in the `org.specs2.matcher.ThrownExpectations` trait. With that trait, any failing expectation will throw a `FailureException` and the rest of the example will not be executed.

There is also an additional method `failure(message)` to throw a `FailureException` at will.

Note that the `ThrownExpectations` traits is mixed in the `mutable.Specification` trait used for _unit_ specifications and, if you wish, you revert back to *not* throwing exceptions on failed expectations by mixing-in the `org.specs2.matcher.NoThrownExpectations` trait.

##### All

The `org.specs2.specification.AllExpectations` trait goes further and gives you the possibility to have all the failures of an Example to be reported without stopping at the first one. This enables a type of specification where it is possible to define lots of expectations inside the body of an example and get a maximum of information on what fails and what passes:

      import org.specs2._
      import specification._

      class AllExpectationsSpec extends mutable.Specification with AllExpectations {
        "In this example all the expectations are evaluated" >> {
          1 === 2  // this fails
          1 === 3  // this also fails
          1 === 1
        }
        "There is no collision with this example" >> {
          10 === 11 // this fails
          12 === 12
          13 === 31 // this also fails
        }
      }

The second example above hints at a restriction for this kind of Specification. The failures are accumulated for each example by mutating a shared variable. "Mutable" means that the concurrent execution of examples will be an issue if done blindly. To avoid this, the `AllExpectations` trait overrides the Specification arguments so that the Specification becomes [isolated](#Isolated+variables) unless it is already `isolated` or `sequential`.

###### Short-circuit

Ultimately, you may want to stop the execution of an example if one expectation is not verified. This is possible with `orThrow`:

      "In this example all the expectations are evaluated" >> {
        1 === 1           // this is ok
       (1 === 3).orThrow  // this fails but is never executed
        1 === 4
      }

Alternatively, `orSkip` will skip the rest of the example in case of a failure.

#### Pending until fixed

Some examples may be temporarily failing but you may not want the entire test suite to fail just for those examples. Instead of commenting them out and then forgetting about those examples when the code is fixed, you can append `pendingUntilFixed` to the Example body:

      "this example fails for now" ! {
        1 must_== 2
      }.pendingUntilFixed

      // or, with a more specific message
      "this example fails for now" ! {
        1 must_== 2
      }.pendingUntilFixed("ISSUE-123")


The example above will be reported as `Pending` until it succeeds. Then it is marked as a failure so that you can remember to remove the `pendingUntilFixed` marker.

#### Auto-Examples

If your specification is about showing the use of a DSL or of an API, you can elid a description for the Example. This functionality is used in ***specs2*** to specify matchers:

     "beNone checks if an element is None"                             ^
     { None must beNone }                                              ^
     { Some(1) must not be none }                                      ^

In that case, the text of the example will be extracted from the source file and the output will be:

     beNone checks if an element is None
       + None must beNone
       + Some(1) must not be none

Auto-Examples can also be used in mutable specifications but the need to be declared by using the `eg` (*exempli gratia*, the latin abbreviation for "for example"):

     class SomeExamples extends mutable.Specification {
       { None must beNone }.eg
       { Some(1) must not be none }.eg
     }

A few things to remember about this feature:

 * the source file is expected to be found in the `src/test/scala` directory. This can be overriden by specifying the `specs2.srcTestDir` system property

 * the extraction of the source code is rudimentary and may fail on specifications which are built dynamically

 * several lines of code can be extracted provided that the block ends with a `Result` and that there is a `Fragment` following the block to be extracted. The best way to ensure that is to always add an `end` fragment at the end of the `Specification`

 * the code to extract must be in the same directory as the package of the specification class it belongs to. If a Specification is declared in `package com.mycompany.accounting` then its source file has to be in the `com/mycompany/accounting` directory for Auto-Examples to be working

 * for more robustness, but different results, you can use the `descFromExpectations` argument (creates an `args(fromSource=false)` argument) to take the "ok message" from the expectation as the example description:

         // outputs: List(1, 2) must contain(1)
         { List(1, 2) must contain(1) }

         // outputs: 'List(1, 2)' contains '1'
         descFromExpectations ^
         { List(1, 2) must contain(1) }
  """^"""
#### G/W/T

More sophisticated is the Given/When/Then style of writing specifications. This style is supported by interspersing Text fragments, with Given/When/Then `RegexSteps` which extract meaningful values from the text. Here's an example specification for a simple calculator:

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

 * `number1` is a `Given` step. It is parametrized with the type `Int` meaning that its `extract` method is supposed to extract an Int from the preceding text. It does so by using the `extract1` inherited method, which parses the text for `${}` expressions and return a tuple (with 1 element here) containing all the values enclosed in `${}`.

 * `number2` is a `When` step. It is paramerized with an `Int`, the result from the previous extraction, and an `Addition` which is the result of extracting the second number and putting the 2 together. In that case the method which must be defined is `extract(Int, String): Addition`.

 * finally the `result` object defines the outcome of the Addition. Its `extract` method takes an `Addition` and the current text to return a `Result`

##### Sequencing

A G/W/T sequence can contain more than just 3 steps. However the compiler will check that:

 * only a `Given[T]` extractor can start a sequence
 * only a `Given[S]`, a `When[T, S]` or a `Then[T]` extractor can follow a `Given[T]` extractor
 * only a `When[T1, T2, S]` or a `Then[T1, T2]` can follow a sequence of `Given[T1], Given[T2]` extractors (up to 8 Given steps, after that types are paired)
 * only a `When[S, U]` extractor or a `Then[S]` can follow a `When[T, S]` extractor
 * only a `Then[S]` can follow a `Then[S]` extractor

To be more concrete, here are a few valid sequences:

 * Given[T] / When[T, S] / Then[S]
 * Given[T] / Given[T2] / Given[T2] / When[T, T1, T2, R] / Then[R]
 * Given[T] / Given[T2] / Given[T3] / Given[T4] / Then[T, T1, T2, T3, T4]
 * Given[T] / Given[T2] / ... / Given[T8] / Then[T, T1, T2, T3, T4, T5, T6, (T7, T8)]
 * Given[T] / When[T, S] / Then[S] / Then[S]
 * Given[T] / Then[T] / Then[T]
 * Given[T] / When[T, S] / When[S, U] / Then[U]

##### Extract methods

The `Given`, `When`, `Then` classes provide several convenience methods to extract strings from the preceding text: the `extract1, extract2,...`
 methods will extracts the values delimited by `${}` for up to 10 values.

##### User regexps

In the original way of declaring Given/When/Then steps, the text is left completely void of markers to extract meaningful values. The user then
 needs to specify a regular expression where groups are used to show where those values are:

      object number1 extends Given[Int]("Given the following number: (.*)") {
        def extract(text: String): Int = extract1(text).toInt
      }

The advantage of using this way is that the text is left in it's pristine form, the drawback is that most of the text is duplicated in 2 places, adding more maintenance burden.

##### Factory methods

There are some factory and implicit conversion methods to create Given/When/Then steps by passing functions and / or regular expressions:

 * convert a function `String... => T` to a `Given[T]` step (*note the use of `and` after `readAs` and `groupAs`*)

        // this assumes that the Int to extract is delimited with ${}
        val number1: Given[Int] = (s: String) => s.toInt
        number1.extract("pay ${100} now") === 100

        // this uses a regular expression with capturing groups matching the full text
        val number1: Given[Int] = readAs(".*(\d+).*") and { (s: String) => s.toInt }
        number1.extract("pay 100 now") === 100

        // this uses capturing groups directly
        val number1: Given[Int] = groupAs("\d+") and { (s: String) => s.toInt }
        number1.extract("pay 100 now") === 100

        // if the Given step is only side-effecting we can omit the `and` call
        // this simplifies the use of Given steps in Unit Specifications
        val number1: Given[Unit] = groupAs("\d+") { (s: String) => value = s.toInt }

 * convert a function `T => String... => S` to a `When[T, S]` step (*note the use of `and` after `readAs` and `groupAs`*)

        // this assumes that the Int to extract is delimited with ${}
        val number2: When[Int, (Int, Int)] = (n1: Int) => (s: String) => (n1, s.toInt)
        number2.extract(100, "with a discount of ${10}%") === (100, 10)

        // this uses a regular expression with capturing groups matching the full text
        val number2: When[Int, (Int, Int)] = readAs(".*(\d+).*") and { (n1: Int) => (s: String) => (n1, s.toInt) }
        number2.extract(100, "with a discount of 10%") === (100, 10)

        // this uses capturing groups directly
        val number2: When[Int, (Int, Int)] = groupAs("\d+") and { (n1: Int) => (s: String) => (n1, s.toInt) }
        number2.extract(100, "with a discount of 10%") === (100, 10)

 * convert a function `T => String... => Result` to a `Then[T]` step (*note the use of `then` after `readAs` and `groupAs`*)

        // this assumes that the Int to extract is delimited with ${}
        val number3: Then[(Int, Int)] = (n: (Int, Int)) => (s: String) => discount(n._1, n._2) must_== s.toInt
        number3.extract((100, 10), "the result is ${90}") must beSuccessful

        // this uses a regular expression with capturing groups matching the full text
        val number3: Then[(Int, Int)] = readAs(".*(\d+).*") then { (n: (Int, Int)) => (s: String) => discount(n._1, n._2) must_== s.toInt }
        number3.extract((100, 10), "the result is 90") must beSuccessful

        // this uses capturing groups directly
        val number3: Then[(Int, Int)] = groupAs("\d+") then { (n: (Int, Int)) => (s: String) => discount(n._1, n._2) must_== s.toInt }
        number3.extract((100, 10), "the result is 90") must beSuccessful

        // if the Then step is only side-effecting we can omit the `then` call
        // this simplifies the use of Then steps in Unit Specifications
        val number3: Then[Unit] = groupAs("\d+") { (s: String) => value must_== s.toInt }

##### G/W/T sequences

Given the rule saying that only a `Then` block can follow another `Then` block you might think that it is not possible to start another G/W/T
sequence in the same specification! Fortunately it is possible by just terminating the first sequence with an `end` fragment:

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

##### Multiple steps

If there are lots of consecutive `When` steps collecting the same kind of arguments, it will be easier to collect them in a `Seq[T]` rather than a `TupleN[T]`:

      "A given-when-then example for the addition"                 ^
        "Given the following number: ${1}"                         ^ number1 ^
        "And a second number: ${2}"                                ^ number2 ^
        "And a third number: ${3}"                                 ^ number3

      val number1: Given[Int]               = (_:String).toInt
      val number2: When[Int, (Int, Int)]    = (n1: Int) => (s: String) => (n1, s.toInt)
      val number3: When[Seq[Int], Seq[Int]] = (numbers: Seq[Int]) => (s: String) => numbers :+ s.toInt

##### ScalaCheck

Once you've created a given G/W/T sequence, you can be tempted to copy and paste it in order to check the same scenario with different values. The trouble with this is the duplication of text which leads to more maintenance down the road.

This can be avoided and even enhanced by using ScalaCheck to generate more values for the same scenario. For the calculator above you could write:

      import org.scalacheck.Gen._
      import specification.gen._

      class GivenWhenThenScalacheckSpec extends Specification with ScalaCheck { def is =

        "A given-when-then example for a calculator"                                   ^
          "Given a first number n1"                                                    ^ number1 ^
          "And a second number n2"                                                     ^ number2 ^
          "When I add them"                                                            ^ add ^
          "Then I should get n1 + n2"                                                  ^ result ^
                                                                                       end

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
 * the return values from the `extract` methods of the `Given` and `When` steps which must return ScalaCheck generators (cf `number1` and `number2`). For the `add` step there is an implicit conversion transforming any value of type `T` to a `Gen[T]`
 * the use of the ScalaCheck trait to access the `check` function transforming a function to a `org.scalacheck.Prop` and then to a `Result`
 * the `extract` method of the `Then` step takes an implicit `Arbitrary[T]` parameter which is used by the `check` method to create a ScalaCheck property

##### Single step

A `GivenThen` step can be used to extract values from a single piece of text and return a `Result`:

      "given the name: ${eric}, then the age is ${18}" ! new GivenThen {
        def extract(text: String) = {
          val (name, age) = extract2(text)
          age.toInt must_== 18
        }
      }

You can also use the `so` object. This object provides an `apply` method expecting a `PartialFunction` and does the value extraction:

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

##### Unit specification

Given / When / Step can also be used in a unit specification by using the &lt;&lt; operator and local variables:

        "A given-when-then example for a calculator".txt.br

          "Given the following number: ${1}" << { s: String =>
            a = s.toInt
          }
          "And a second number: ${2}" << { s: String =>
            b = s.toInt
          }
          "When I use this operator: ${+}" << { s: String =>
            result = Operation(a, b, s).calculate
          }
          "Then I should get: ${3}" << { s: String =>
            result === s.toInt
          }
          "And it should be > ${0}" << { s: String =>
            result must be_>(s.toInt)
          }

        var a, b, result: Int = 0

        case class Operation(n1: Int, n2: Int, operator: String) {
          def calculate: Int = if (operator == "+") n1 + n2 else n1 * n2
        }

If you want to use your own regular expression parsing, the &lt;&lt; operator also accepts `Given[Unit]` and `Then[Unit]` steps:

        "Given the following number: 1" << readAs(".*(\\d).*") { s: String =>
          a = s.toInt
        }
        "And a second number: 2" << groupAs("\\d") { s: Seq[String] =>
          b = s.head.toInt
        }
        "When I use this operator: +" << groupAs("[\\+\\-]") { s: String =>
          result = Operation(a, b, s).calculate
        }
        "Then I should get: 3" << groupAs("\\d") { s: String =>
          result === s.toInt
        }
        "And it should be > 0" << groupAs("\\d") { s: String =>
          result must be_>(s.toInt)
        }

Similarly, ScalaCheck generator and properties are supported:

        "Given a first number n1" << {
          n1 = choose(-10, 10)
        }
        "And a second number n2" << {
          n2 = choose(-10, 10)
        }
        "When I add them" << {
          operation = Arbitrary {
            for (a1 <- n1; a2 <- n2) yield Addition(a1, a2)
          }
        }
        "Then I should get n1 + n2" << check { (op: Addition) =>
          op.calculate must_== op.n1 + op.n2
        }

        var n1, n2: Gen[Int] = null
        implicit var operation: Arbitrary[Addition] = null

#### DataTables

[DataTables](org.specs2.guide.Matchers.html#DataTables) are generally used to pack lots of expectations inside one example. A DataTable which is used as a `Result` in the body of an Example will only be displayed when failing. If, on the other hand you want to display the table even when successful, to document your examples, you can omit the example description and inline the DataTable directly in the specification:

      class DataTableSpec extends Specification with DataTables { def is =

        "adding integers should just work in scala"  ^ {
          "a"   | "b" | "c" |
           2    !  2  !  4  |
           1    !  1  !  2  |> {
           (a, b, c) =>  a + b must_== c
        }
      }

This specification will be rendered as:

      adding integers should just work in scala
      +  a | b | c |
         2 | 2 | 4 |
         1 | 1 | 2 |

### Links

There are 2 ways to "link" specifications:

 * by including another specification, to create a parent-child relationship
 * by creating a reference to another specification, to create a peer relationship

#### Inclusion

There is a simple mechanism for including a "children" specification in a given specification. You can simply add the child specification as if it was a simple fragment:

      "This is an included specification"     ^
        childSpec

Otherwise, if you want to include several specifications at once you can use the `include` method:

      "This is the included specifications"         ^
        include(childSpec1, childSpec2, childSpec3)


The effect of doing so is that all the fragments of the children specification will be inlined in the parent one. This is exactly what is done in this page of the user guide, but with a twist

      include(xonly, new GivenWhenThenSpec)        ^
      include(xonly, exampleTextIndentation)       ^
      include(xonly, resetTextIndentation)         ^

In the code above there are specific arguments to the included specifications so that they are only displayed when there are failures.

##### Inline

When you include a specification in another one the console will display the beginning and end statistics of the included specification. If you just want to insert the "middle" fragments of the included specification you can use `inline`:

     inline(otherSpecification)

##### Html link

In order to create a User Guide such as this one, you might want the included specification to be written to another html file. In this case, you need a "Link":

      link(new QuickStart)

This declaration will include the child specification so it is executed when the parent specification is executed. However during the reporting, only a Html link will be created in the parent file, referencing a separate file for the children specification. On the other hand if you "hide" the specification, the link will not be printed out:

      link((new QuickStart).hide)

###### Html Link

It is possible to customize the generated Html link with the following syntax:

      "a " ~ ("quick start guide", new QuickStart)

The `~` operator is used to create a `HtmlLink` where:

 * "a" is the beginning of the text
 * "quick start guide" is the text that will be highlighted as a url link
 * `new QuickStart` is the specification to include, the url being derived from the specification class name

Several variations are possible on this pattern, depending which part of the link you want to be highlighted:

      "before text" ~ ("text to highlight", specification, "after text")
      "before text" ~ ("text to highlight", specification, "after text", "tooltip")
      "text to highlight" ~ specification
      "text to highlight" ~ (specification, "after text")
      "text to highlight" ~ (specification, "after text", "tooltip")

#### Reference

Sometimes you just want to reference another specification without triggering its execution. For example when [creating an index page](#Create+an+index):

      see(new MailSenderSpec)

This will generate a html link in the main specification based on the referenced specification name. If you want to customize that link you can use the following syntax:

      "before text" ~/ ("text to highlight", specification, "after text")
      "before text" ~/ ("text to highlight", specification, "after text", "tooltip")
      "text to highlight" ~/ specification
      "text to highlight" ~/ (specification, "after text")
      "text to highlight" ~/ (specification, "after text", "tooltip")

#### Markdown url

If you just want to reference the url of the html page that's being generated for a given specification in a paragraph of text, you can use the `markdownUrl` method:

      "For more information you can read "+DetailedSpec.markdownUrl
      // or
      "For more information you can read "+DetailedSpec.markdownUrl("the detailed specification")
      // or
      "For more information you can read "+"the detailed specification".markdownUrl(DetailedSpec)

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

#### Isolation

***specs2*** solves this issue in 2 ways:

 * simply by relying on Scala features, by creating a new trait or a case class to open a new `Scope` with fresh variables
 * by cloning the specification on each example execution when the `isolated` argument is provided

##### Scope

Let's see an example of using a `Scope` with a mutable specification:

      import org.specs2.specification.Scope

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
        val tree = new Tree(1, 2, 3, 4)
      }

Each example of that specification gets a new instance of the `trees` trait. So it will have a brand new `tree` variable and even if this data is mutated by an example, other examples will be isolated from these changes.

Now you might wonder why the `trees` trait is extending the `org.specs2.specification.Scope` trait? The reason is that the body of an Example only accepts objects which are convertible to a `Result`. By extending `Scope` we can take advantage of an implicit conversion provided by the `Specification` trait to convert our context object to a `Result`.

Scopes are a way to create a "fresh" object and associated variables for each example being executed. The advantages are that:

 - those classes can be reused and extended
 - the execution behavior only relies on language constructs

However, sometimes, we wish to go for a more concise way of getting fresh variables, without having to create a specific trait to encapsulate them. That's what the `isolated` argument is for.

##### Isolated variables

The `isolated` argument changes the execution method so that each example is executed in a brand new instance of the Specification:

      class IsolatedSpec extends mutable.Specification {
        isolated

        "Each example should be executed in isolation" >> {

          val tree = new Tree(1, 2, 3, 4)
          "the first example modifies the tree" >> {
            tree.removeNodes(2, 3) must have size(2)
          }
          "the second example gets an unmodified version of the tree" >> {
            tree.removeNodes(2, 3, 4) must have size(1)
          }
        }
      }

Since there is a new Specification for each example, then all the variables accessible to the example will be seen as new.

_Note_: this technique will not work if the Specification is defined with a constructor having parameters because it won't be possible to create a new instance.

##### Case classes

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

#### Before/After

If you want to run some code before or after each example, the `Before` and `After` traits are there to help you (they both extend the `Scope` trait). In the following examples we'll only show the use of `After` because `Before` most of the time unnecessary:

      class ContextSpec extends mutable.Specification {
        "this is the first example" in new trees {
          tree.removeNodes(2, 3) must have size(2)
        }
        "this is the first example" in new trees {
          tree.removeNodes(2, 3, 4) must have size(1)
        }
      }

      trait trees extends Scope {
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

        case class trees() extends specification.After {
          lazy val tree = getATreeWith4NodesFromTheDatabase
          def after = cleanupDB

          // this is equivalent to: def e1 = this.apply { ... }
          def e1 = this { tree.removeNodes(2, 3) must have size(2) }
          def e2 = this { tree.removeNodes(2, 3, 4) must have size(1) }
        }
      }

Now we have both variable isolation and non-duplication of set-up code!

But there is more to it. The next paragraphs will show how to:

 1. execute the body of each example inside a specific context: `Around`
 1. set-up a context object (say a http query) and pass it to each example: `Outside`
 1. declare a `before` method for all the examples of a Specification without even having to create a context object
 1. use an implicit context to avoid duplication
 1. create a new context object by combining existing ones

#### Around

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

#### Outside

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

##### AroundOutside

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

#### BeforeExample

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

Indeed in both examples above the `s` string that will be passed is the Example description as specified [here](#Use+descriptions).

#### Composition

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

#### Steps/Actions

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


#### Template

There may still be some duplication of code if you have to use the same kind of set-up procedure for several specifications.

If that's the case you can define your own `Specification` trait doing the job:

      import org.specs2._
      import specification._

      trait DatabaseSpec extends Specification {
        /** the map method allows to "post-process" the fragments after their creation */
        override def map(fs: =>Fragments) = Step(startDb) ^ fs ^ Step(cleanDb)
      }

The `DatabaseSpec` above will insert, in each inherited specification, one `Step` executed before all the fragments, and one executed after all of them.

#### For fragments

When using a Unit Specification, it can be useful to use variables which are only used for a given set of examples. This can be easily done by declaring local variables, but this might lead to duplication. One way to avoid that is to use the `org.specs2.mutable.NameSpace` trait:

      trait context extends mutable.NameSpace {
        var variable1 = 1
        var variable2 = 2
      }

      "this is the first block" >> new context {
        "using one variable"      >> { variable1 === 1 }
        "using a second variable" >> { variable2 === 2 }
      }
      "this is the second block" >> new context {
        "using one variable"      >> { variable1 === 1 }
        "using a second variable" >> { variable2 === 2 }
      }

### Execution

This section summarizes the execution algorithm of a specification based on its fragments:

 1. all the fragments are divided into groups delimited by `Steps`
 2. if the `sequential` argument is present, each fragment goes to its own group
 3. groups are executed sequentially and all the fragments of a given group are executed concurrently
 4. if the `isolated` argument is present, each example is executed in its own version of the Specification
 5. if the `isolated` argument is present, all the `Steps` preceding an example are executed before that example
 6. if the Specification inherits from the `AllExpectations` trait, then it is executed as an `isolated` Specification unless it is already set as `sequential`
 7. if the `stopOnFail` argument is present, all the examples in the next group of fragments will be skipped if there is a failure in one of the previous groups
 8. if the `stopOnSkip` argument is present, all the examples in the next group of fragments will be skipped if there is a skipped in one of the previous groups
 9. if there is a `Step(stopOnFail = true)`, all the examples in the next group of fragments will be skipped if there is a failure in the group before the `Step`

### Layout

For an _acceptance_ specification you can tweak the layout of Texts and Examples.

##### Rules

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

##### Formatting fragments

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

###### Unit specification

Formatting fragments can be used in a unit specification as well. 2 forms are supported, either as a single declaration:

      "this is an example" >> { 1 === 1 }
      p // add a paragraph
      "this is another example" >> { 2 === 2 }

Or as a postfix operator on fragments:

      "this is some text and a paragraph".p
      "this is an example and a paragraph" >> {
        1 must_== 1
      } p

There are also 2 additional postfix operations which can be used to start new paragraphs. Instead of using `endp` to end a group of examples and starte a new one:

      "This is a first block of examples".p
      { 1 === 1 }.eg;
      { 2 === 2 }.eg.endp

      "And a second block".p
      { 3 === 3 }.eg;
      { 4 === 4 }.eg

You can use `newp` (or `newbr`) to the same effect:

      "This is a first block of examples".p
      { 1 === 1 }.eg;
      { 2 === 2 }.eg

      "And a second block".newp
      { 3 === 3 }.eg;
      { 4 === 4 }.eg

A shortcut is also available to indent a 'subexample' locally:

      "this is the first major example" >> { ok }
          "this is minor and should be indented" >> { ok } lt;
        "this is the second major example" >> { ok }
      }

This will output:

      this is a group of examples
      + this is the first major example
        + this is minor and should be indented
      + this is the second major example

### Unit specifications

Those are all the methods which you can use to create fragments in a unit specification:

 * `can`: create a group of Examples, with the preceding Text fragment appended with `can`

        "a configuration" can {
          "have a name" in { ... }
        }

 * <code class="prettyprint">></code><code class="prettyprint">></code>: create an Example or a group of Examples (with no appended text)

        "a configuration may" >> {
          "have a name" in { ... }
        }

   Note that you can use a `for` loop to create examples with <code class="prettyprint">></code><code class="prettyprint">></code>:

        "this system has 5 examples" >> {
          (1 to 5) foreach { i => "example "+i >> ok }
        }

   And you can also use a `for` loop with the `in` operator to create a block of expectations:

        "this example has 5 expectations" in {
          (1 to 5) foreach { i => i must_== i }
        }

 * `title`: give a title to the Specification

        "My spec title".title
        // file path can be used to specify a different path for the html reporting
        "My spec title".title(filePath = "com/MySpec.html")

 * `args`: create arguments for the specification

 * `.txt` or `textFragment`: create a `Text` fragment

        "this is a text fragment".txt

        textFragment("this is a text fragment")

 * `step`: create a `Step`

        step { initializeDatabase() }

 * `action`: create an `Action`

        action { justDoIt }

 * `link`: create a link to another specification

        link("how" ~ ("to do hello world", new HelloWorldSpec))

 * `see`: add a link to another specification without including its fragments for execution

        see(new HelloWorldSpec)

  * `include` to include another specification

        include(new HelloWorldSpec)

  * `p, br, t, bt, end, endp`: add a formatting fragment

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

        "This is a unit specification showing the use of different methods".txt

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
      """^
    """
### How to?

#### Declare arguments

Arguments are usually passed on the command line but you can also declare them at the beginning of the specification, to be applied only to that specification.
For example, you can turn off the concurrent execution of examples with the `args(sequential=true)` call:

      class ExamplesOneByOne extends Specification { def is =

        // there is a shortcut for this argument called 'sequential'
        args(sequential=true)              ^
        "first example"                    ! e1 ^
        "the the second one"               ! e2 ^
                                           end
      }

For the complete list of arguments and shortcut methods read the [Runners](org.specs2.guide.Runners.html) page.

#### Pass arguments

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

#### Add a title

Usually the title of a specification is derived from the specification class name. However if you want to give a more readable name to your specification report you can do the following:

      class MySpec extends Specification { def is =
         "My beautiful specifications".title                           ^
                                                                       p^
         "The rest of the spec goes here"                              ^ end
      }

The title can be defined either:

 * at the beginning of the specification
 * just after the arguments of the specification

#### Use descriptions

The description of an Example can be used to create an expectation in the example body:

      "This is a long, long, long description" ! ((s: String) => s.size must be_>(10))

#### Enhance failures

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

#### Share examples

In a given specification some examples may look similar enough that you would like to "factor" them out and share them between
different parts of your specification. The best example of this situation is a specification for a Stack of limited size:

      class StackSpec extends Specification { def is =

        "Specification for a Stack with a limited capacity".title                   ^
                                                                                    p^
        "A Stack with limited capacity can either be:"                              ^ endp^
          "1. Empty"                                                                ^ anEmptyStack^
          "2. Normal (i.e. not empty but not full)"                                 ^ aNormalStack^
          "3. Full"                                                                 ^ aFullStack^end

        def anEmptyStack =                                                          p^
          "An empty stack should"                                                   ^
            "have a size == 0"                                                      ! empty().e1^
            "throw an exception when sent #top"                                     ! empty().e2^
            "throw an exception when sent #pop"                                     ! empty().e3^endbr

        def aNormalStack =                                                          p^
          "A normal stack should"                                                   ^
            "behave like a non-empty stack"                                         ^ nonEmptyStack(newNormalStack)^
            "add to the top when sent #push"                                        ! nonFullStack().e1^endbr

        def aFullStack =                                                            p^
          "A full stack should"                                                     ^
            "behave like a non-empty stack"                                         ^ nonEmptyStack(newFullStack)^
            "throw an exception when sent #push"                                    ! fullStack().e1

        def nonEmptyStack(stack: =>SizedStack) = {                                  t^
          "have a size > 0"                                                         ! nonEmpty(stack).size^
          "return the top item when sent #top"                                      ! nonEmpty(stack).top1^
          "not remove the top item when sent #top"                                  ! nonEmpty(stack).top2^
          "return the top item when sent #pop"                                      ! nonEmpty(stack).pop1^
          "remove the top item when sent #pop"                                      ! nonEmpty(stack).pop2^bt
        }

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

#### Create an index

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

#### Tag examples

Tags can be used in a Specification to include or exclude some examples or a complete section of fragments from the execution. Let's have a look at one example:

      /**
       * use the org.specs2.specification.Tags trait to define tags and sections
       */
      class TaggedSpecification extends Specification with Tags { def is =
        "this is some introductory text"                          ^
        "and the first group of examples"                         ^
          "example 1"                                             ! success ^ tag("feature1", "unit")^
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

 * `args(include="feature1")` will only include `example 1`
 * `args(exclude="integration")` will include everything except `example 2`
 * `args(include="checkin,unit")` will include anything having either `checkin` OR `unit`: i.e. `example 1` and the second group of examples (`example 3` and `example 4`)
 * `args(include="feature1 && unit")` will include anything having `feature1` AND `unit`: i.e. `example 1`
 * `args(include="feature1 && unit, checkin")` will include anything having `feature1` AND `unit`, OR having `checkin`: i.e. `example 1`, `example 3`, `example4`


##### In a unit specification

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

      class EmailSpecification extends mutable.Specification {
        skipAllIf(serverIsOffLine)
        "test email" >> { sendEmail must beOk }
      }


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
 * `org.specs2.specification.NoAutoExamples`: deactivate the conversions from `Boolean/Result/MatchResult/DataTable` to `Fragment` or `Example`. Specific versions of this trait can be selectively used, on either `Boolean` or `Result` or `MatchResult` or `DataTable`. For example: `org.specs2.specification.NoBooleanAutoExamples` can be used to avoid the `^` method being used on booleans
 * `org.specs2.specification.NoFragmentsBuilder`: deactivate the implicit conversions from `String` to `Fragment`s
 * `org.specs2.specification.mutable.NoFragmentsBuilder`: deactivate the implicit conversions from to remove `in`, <code class="prettyprint">></code><code class="prettyprint">></code>, `should` and `can` methods from `String`s

  - - -
    """^
                                                                                                                        br^
  include(xonly, new GivenWhenThenSpec)                                                                                 ^
  include(xonly, exampleTextIndentation)                                                                                ^
  include(xonly, resetTextIndentation)                                                                                  ^
  include(xonly, pTextIndentation)                                                                                      ^
  include(xonly, databaseSpec)                                                                                          ^
  include(xonly, factoryMethodsSpec)                                                                                    ^
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

  val factoryMethodsSpec = new mutable.Specification with RM {
    def discount(n: Int, p: Int) = (n - n / p)

    "all methods should compile and work as expected" >> {

      var number1: Given[Int] = (s: String) => s.toInt
      number1.extract("pay ${100} now") === 100

      number1 = readAs(".*?(\\d+).*") and { (s: String) => s.toInt }
      number1.extract("pay 100 now") === 100

      number1 = groupAs("\\d+") and { (s: String) => s.toInt }
      number1.extract("pay 100 now") === 100

      var number2: When[Int, (Int, Int)] = (n1: Int) => (s: String) => (n1, s.toInt)
      number2.extract(100, "with a discount of ${10}%") === (100, 10)

      number2 = readAs(".*?(\\d+).*") and { (n1: Int) => (s: String) => (n1, s.toInt) }
      number2.extract(100, "with a discount of 10%") === (100, 10)

      number2 = groupAs("\\d+") and { (n1: Int) => (s: String) => (n1, s.toInt) }
      number2.extract(100, "with a discount of 10%") === (100, 10)

      var number3: Then[(Int, Int)] = (n: (Int, Int)) => (s: String) => discount(n._1, n._2) must_== s.toInt
      number3.extract((100, 10), "the result is ${90}") must beSuccessful

      number3 = readAs(".*?(\\d+).*") then { (n: (Int, Int)) => (s: String) => discount(n._1, n._2) must_== s.toInt }
      number3.extract((100, 10), "the result is 90") must beSuccessful

      number3 = groupAs("\\d+") then { (n: (Int, Int)) => (s: String) => discount(n._1, n._2) must_== s.toInt }
      number3.extract((100, 10), "the result is 90") must beSuccessful

    }
  }
}
