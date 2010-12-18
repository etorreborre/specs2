package org.specs2
package guide

class SpecStructure extends Specification { def is =
                                                                                                                        """
### Presentation

In this chapter you will learn how to:

 * declare examples
 * format the layout of your specification
 * include / link to other specifications
 * define contexts and actions to execute before/after examples
 * use Forms

### Declare examples

As seen in the [Quick Start](org.specs2.guide.QuickStart.html), a specification is a list of *fragments* separated by `^`:

    "this is my specification"                          ^
      "and example 1"                                   ! e1^
      "and example 2"                                   ! e2^

###### Standard results

So the first way to create an Example is to follow a piece of text with `!` and provide anything of type `org.specs2.execute.Result`.
The simplest results values are provided by the `StandardResults` trait, and match the 5 types of results provided by ***specs2***:

  * success: the example is ok
  * failure: there is a non-met expectation
  * anError: a non-expected exception occurred
  * skipped: the example is skipped possibly at runtime because some conditions are not met
  * pending: usually means "not implemented yet"

Two additional results are also available to track the progress of features:

  * done: a Success with the message "DONE"
  * todo: a Pending with the message "TODO"

###### Matcher results

Usually the body of an example is made of *expectations* using matchers:

     def e1 = 1 must_== 1

You can refer to the [Matchers](org.specs2.guide.Matchers.html) guide to learn all about matchers and how to create
expectations. There is however one important point to note here. Because of the functional nature of ***specs2*** the
result of an example is always the last statement of its body. This example will never fail because the first expectation
is "lost":

      "my example on strings" ! e1                // will never fail!
      def e1 = {
        "hello" must have size(10000)             // because this expectation will not be returned,...
        "hello" must startWith("hell")
      }

So the correct way of writing the example is:

      "my example on strings" ! e1               // will fail
      def e1 = "hello" must have size(10000) and
                            startWith("hell")

This can be seen as a restriction but this actually encourages a specification style where every expectation is carefully
specified.

###### Auto-Examples

There is a handy functionality when your specification is about showing the use of a DSL or of an API. If your expectation
fits on one line, you can use it directly, as if it was an example. This is used in ***specs2*** to specify matchers:

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

 * the extraction of the source code is very rudimentary and will just extract one line of code.

###### Using the text of the Example

It is possible to use the text of an example to extract meaningful values, use them in the example body and avoid
repeating oneself:

    "Bob should pay 12"   ! e1

    val toPay = Map("Bob"->"12", "Bill"->"10")           // a "database" of expected values

    def e1 = (s: String) => {
      val ShouldPay = "(.*) should pay (\\d+)".r      // a regular expression for extracting the name and price
      val ShouldPay(name, price) = s                     // extracting the values
      toPay(name) must_== price                          // using them for the expectation
    }

In that case the argument passed to the `!` method is a function taking a String and returning a Result.

### Layout


 - - -

<br/>
                                                                                                                        """^
  include(xonly, exampleText)                                                                                           ^
                                                                                                                        end

  val exampleText = new Specification { def is =
    "Bob should pay 12"   ! e1

    val toPay = Map("Bob"->"12", "Bill"->"10")           // a "database" of expected values

    def e1 = (s: String) => {
      val ShouldPay = "(.*) should pay (\\d+)".r      // a regular expression for extracting the name and price
      val ShouldPay(name, price) = s                     // extracting the values
      toPay(name) must_== price                          // using them for the expectation
    }
  }
}