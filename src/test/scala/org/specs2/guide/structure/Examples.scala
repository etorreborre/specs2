package org.specs2
package guide
package structure

import specification.Groups

object Examples extends UserGuidePage {  def is = ""

  def section = s2"""
### Declare examples

#### Styles

The [Quick Start](org.specs2.guide.QuickStart.html) guide describes 2 styles of specifications, the _unit_ style and the _acceptance_ style. Both styles actually build a specification as a list of *fragments*.

##### _Acceptance_ specification

In an _acceptance_ specification you build a list of _Fragments_ which are interpolated from a **`s2`** string: ${snippet{
// 8<--
class s extends Specification {
// 8<--
def is =                     s2"""

  this is my specification
    and example 1            $e1
    and example 2            $e2
                             """

def e1 = success
def e2 = success
// 8<--
}
}}

This specification builds 1 piece of `Text` and 2 `Examples` which are `Fragment` objects. Another way to define an Example, outside the interpolated specification would be to write: ${snippet{

"Example description" ! { /* example body */ ok } /**/;1/**/

}}

Please read the ${(new FragmentsApi).markdownLink("Fragments API").fromTop} page you want to know more about the low-level operators to create and chain Specification Fragments.

##### _Unit_ specification

A _unit_ specification uses `should/in` blocks which build Fragments by adding them to a mutable protected variable: ${snippet{
// 8<--
class s extends mutable.Specification {
// 8<--
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
 // 8<--
}
 }}

In that specification the following methods are used:

 * `in` to create an `Example` containing a `Result`
 * `should` to create a group of Examples, where `should` is appended to the preceding Text fragment

It is completely equivalent to writing this in an `org.specs2.Specification`: ${snippet{
// 8<--
class s extends Specification {
// 8<--
def is = s2"""

 The 'Hello world' string should
   contain 11 characters ${
     "Hello world" must have size(11)
   }
   start with 'Hello' ${
     "Hello world" must startWith("Hello")
   }
   end with 'world' ${
     "Hello world" must endWith("world")
   }
"""
// 8<--
}
}}

The [Unit specifications](#Unit+specifications) section shows all the methods which can be used to build unit specifications fragments.

#### Results

An `Example` is a piece of text followed by anything which can be converted to an `org.specs2.execute.Result` (via the `org.specs2.execute.AsResult` typeclass):

* a standard result (success, failure, pending,...)
* a Matcher result
* a boolean value
* a ScalaCheck property

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

Usually the body of an example is made of *expectations* using matchers: ${snippet{

def e1 = 1 must_== 1
}}

You can refer to the [Matchers](org.specs2.guide.Matchers.html) guide to learn all about matchers and how to create expectations.

#### Expectations

##### Functional

The default `Specification` trait in ***specs2*** is functional: the `Result` of an example is always given by the last statement of its body. For instance, this example will never fail because the first expectation is "lost": ${snippet{

"my example on strings" ! e1           // will never fail!

def e1 = {
  "hello" must have size(10000)        // because this expectation will not be returned,...
  "hello" must startWith("hell")
}
}}

So the correct way of writing the example is: ${snippet{

"my example on strings" ! e1           // will fail

def e1 = "hello" must have size(10000) and
                           startWith("hell")

}}

##### Thrown

The above functionality encourages a specification style where every expectation is carefully specified and is considered good practice by some. However you might see it as an annoying restriction. You can avoid it by mixing-in the `${fullName[matcher.ThrownExpectations]}` trait. With that trait, any failing expectation will throw a `FailureException` and the rest of the example will not be executed.

There is also an additional method `failure(message)` to throw a `FailureException` at will.

Note that the `ThrownExpectations` traits is mixed in the `mutable.Specification` trait used for _unit_ specifications and, if you wish, you revert back to *not* throwing exceptions on failed expectations by mixing-in the `org.specs2.matcher.NoThrownExpectations` trait.

##### All

The `org.specs2.specification.AllExpectations` trait goes further and gives you the possibility to report all the failures of an Example without stopping at the first one. This enables a type of specification where it is possible to define lots of expectations inside the body of an example and get a maximum of information on what fails and what passes: ${snippet{

import org.specs2.specification.AllExpectations
import org.specs2.mutable.Specification

class AllExpectationsSpec extends Specification with AllExpectations {
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
}}

The second example above hints at a restriction for this kind of Specification. The failures are accumulated for each example by mutating a shared variable. "Mutable" means that the concurrent execution of examples will be an issue if done blindly. To avoid this, the `AllExpectations` trait overrides the Specification arguments so that the Specification becomes [isolated](#Isolated+variables) unless it is already `isolated` or `sequential`.

###### Short-circuit

Ultimately, you may want to stop the execution of an example if one expectation is not verified. This is possible with `orThrow`: ${snippet{
// 8<--
class s extends mutable.Specification {
// 8<--
  "In this example all the expectations are evaluated" >> {
    1 === 1            // this is ok
    (1 === 3).orThrow  // this fails but is never executed
    1 === 4
  }
// 8<--
}
// 8<--
}}

Alternatively, `orSkip` will skip the rest of the example in case of a failure.

#### Auto-Examples

If your specification is about showing the use of a DSL or of an API, you can elide a description for the Example. This functionality is used in ***specs2*** to specify matchers: ${snippet{
// 8<--
s2""" // 8<--
beNone checks if an element is None
${ None must beNone }
${ Some(1) must not be none }
// 8<--
"""
}}

In that case, the text of the example will be extracted from the source file and the output will be:

  beNone checks if an element is None
  + None must beNone
  + Some(1) must not be none

#### G / W /T

The Given/When/Then style for writing specifications is described ${(new GivenWhenThenPage).markdownLink("here").fromTop}.

#### DataTables

[DataTables](org.specs2.guide.Matchers.html#DataTables) are generally used to pack lots of expectations inside one example. A DataTable which is used as a `Result` in the body of an Example will only be displayed when failing. If, on the other hand you want to display the table even when successful, to document your examples, you can omit the example description and inline the DataTable directly in the specification: ${snippet{

class DataTableSpec extends Specification with matcher.DataTables { def is =

  "adding integers should just work in scala"  ^ {
    "a"   | "b" | "c" |
     2    !  2  !  4  |
     1    !  1  !  2  |>
     { (a, b, c) =>  a + b must_== c }
  }
}
}}
This specification will be rendered as:

  adding integers should just work in scala
  +  a | b | c |
     2 | 2 | 4 |
     1 | 1 | 2 |

#### Example groups

When you create acceptance specifications, you have to find names to reference your examples, which can sometimes be a bit tedious. You can then get some support from the `${fullName[specification.Grouped]}` trait. This trait provides group traits, named `g1` to `g22` to define groups of examples. Each group trait defines 22 variables named `e1` to `e22`, to define examples bodies. The specification below shows how to use the `Grouped` trait: ${snippet{

class MySpecification extends Specification with Examples { def is =  s2"""
  first example in first group                                        ${g1.e1}
  second example in first group                                       ${g1.e2}

  first example in second group                                       ${g2.e1}
  second example in second group                                      ${g2.e2}
  third example in second group, not yet implemented                  ${g2.e3}
  """
}

trait Examples extends specification.Grouped with matcher.Matchers {
  // group of examples with no description
  new g1 {
    e1 := ok
    e2 := ok
  }
  // group of examples with a description for the group
  "second group of examples" - new g2 {
    e1 := ok
    e2 := ok
  }
}
}}

Note that, if you use groups, you can use the example names right away, like `g2.e3`, without providing an implementation, the example will be marked as `Pending`.

##### Isolation

You can define additional variables in your group traits: ${snippet{
// 8<--
class MySpecification extends Groups { def is = ""
// 8<--
trait Local {
  def service: Service = new LocalService
}
"a group of examples" - new g1 with Local {
  e1 := ok
  e2 := ok
}
"another group of examples" - new g2 with Local {
  e1 := ok
  e2 := ok
}
// 8<--
}
}}

However, the `service` variable will be shared by all the examples of each group, which can be potentially troublesome if that variable is mutated. If you want to provide complete isolation for each example, you should instead use the `${fullName[specification.Groups]}` trait and call each group as a function: ${snippet{

class MySpecification extends Specification with Examples { def is = s2"""

  first example in first group                     ${g1().e1}
  second example in first group                    ${g1().e2}
                                                   """
}

trait Examples extends Groups with matcher.Matchers {
  trait Local {
    def service: Service = new LocalService
  }
  "a group of examples" - new g1 with Local {
    // each example will have its own instance of Service
    e1 := ok
    e2 := ok
  }
}
}}

#### Tags

Tags can be used for 2 purposes: example selection and formatting.

##### Selection

You can tag specific examples or entire sections of a specification and execute them selectively from the command line. See [HowTo](org.specs2.guide.HowTo.html#Tags) on how to use tags.

##### Formatting

You can use the `formatSection` or `formatTag` methods to specify the formatting of `Texts` and `Examples` fragments with the following parameters:

 - `flow`: the fragment (`Text` or `Example`) shouldn't be reported with automatic indenting (default = `false`, set automatically to `true` when using `s2` interpolated strings)
 - `markdown`: the fragment is using Markdown (default = `true`)
 - `verbatim`: indented text with more than 4 spaces must be rendered as a code block (default = `true`)


"""
  trait Service
  class LocalService extends Service
}