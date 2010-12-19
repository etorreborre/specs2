package org.specs2
package guide
import examples._

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
    val ShouldPay = "(.*) should pay (\\d+)".r           // a regular expression for extracting the name and price

    def e1 = (s: String) => {
      val ShouldPay(name, price) = s                     // extracting the values
      toPay(name) must_== price                          // using them for the expectation
    }

In that case the argument passed to the `!` method is a function taking a String and returning a Result.

###### Given / When / Then

In the same fashion, the Given/When/Then style of writing specifications is supported, albeit using a mutable object to
collect the successive states of the system:

      "Given that the customer buys 3 books at 10 dollars each"                                        ! c1.buyBook^
      "Given that the customer buys 1 book at 20 dollars"                                              ! c1.buyBook^
      "When he checks out"                                                                             ! c1.checkout^
      "Then the total price must be 50 dollars"                                                        ! c1.total^
                                                                                                   end

      case object c1 {
        val BuyBooks = ".* buys (\\d+) book.? at (\\d+) .*".r     // a regular expression for extracting the quantity and price
        val TotalBooks = ".* must be (\\d+) .*".r                 // a regular expression for extracting the total price
        val books: scala.collection.mutable.Map[Int, Int] = new scala.collection.mutable.HashMap[Int, Int]()

        def buyBook = (s: String) => {
          val BuyBooks(qty, price) = s
          books += qty.toInt -> price.toInt
          success
        }
        def checkout = books.pp must not be empty
        def total = (s: String) => {
          val TotalBooks(total) = s
          books.foldLeft(0)((res, cur) => res + cur._1 * cur._2) must_== total.toInt
        }
      }


### Layout

The layout of text in ***specs2*** is mostly done automatically so that the text in the source code should look like the
displayed text after execution. You can turn off that automatic layout by adding the `noindent` arguments at the beginning
of your specification

      class MySpecWithNoIndent extends Specification {
        def is = noindent ^ ....
      }

##### The rules

By default the layout of a specification will be computed automatically based on a few rules:

  * when a text follows some text, it is indented
  * when a text follows an example, the indentation stays at the same level
  * when an example follows a text, it is indented
  * when an example follows an example, it is not indented

Let's see a standard example of this. The following fragments:

    "this is some presentation text"      ^
      "and the first example"             ! success^
      "and the second example"            ! success

will be executed and displayed as:

    this is some presentation text
      + and the first example
      + and the second example

##### The formatting fragments

Given the rules above, you might need to use some *formatting fragments* to adjust the display

###### Reset the levels

Following the rules, if you add some text after an example, this means that you want to describe a "nested" context for
your specification:

    "There are several options for displaying the text"      ^
      "xonly displays nothing but failures"                  ! success^
      "there is also a color option"                         ^              // this text will be indented
        "rgb=value uses that value to color the text"        ! rgb^         // and the following examples as well
        "nocolor dont color anything"                        ! nocolor^
    "There are different ways of hiding the text"            ^              // this text follows an example, it will
        "by tagging the text"                                ! hideTag      // be indented :-(

However in the example above the intention is to start a new group of examples at level 0 for `"There are different ways
of hiding the text"`.

In order to do that you can use the `end` fragment:

    "There are several options for displaying the text"      ^
      "xonly displays nothing but failures"                  ! success^
      "there is also a color option"                         ^              // this text will be indented
        "rgb=value uses that value to color the text"        ! rgb^         // and the following examples as well
        "nocolor dont color anything"                        ! nocolor^ end^
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


###### Adding blank lines

A better way to separate blocks of examples though is to add blank lines in between. One possibility is to use `p` (as in
"paragraph") to both add a newline and unindent a text block:

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

That looks remarkably similar to the specification code, doesn't it?

There are other possibilities for adding blank lines:

 * use the `br` element: simply adds a blank line, not changing the indentation level
 * combine `end` and `br` in one fragment: `endbr` (`endp` is also available)

###### Changing the indentation level

If, for whatever reason, you wish to have more or less indentation, you can use the `t` and `bt` fragments:

    "this text"                                     ^ bt
    "doesn't actually have an indented example"     ! success

    "this text"                                     ^ t
        "has a very indented example"               ! success

 The number of indentation levels (characterized as 2 spaces on screen) can also be specified by using `t(n)` or `bt(n)`.

 - - -

<br/>
                                                                                                                        """^
  include(xonly, exampleTextExtraction)                                                                                 ^
  include(xonly, new GivenWhenThenSpec)                                                                                 ^
  include(xonly, exampleTextIndentation)                                                                                ^
  include(xonly, resetTextIndentation)                                                                                  ^
  include(xonly, pTextIndentation)                                                                                      ^
                                                                                                                        end

  val exampleTextExtraction = new Specification { def is =
    "Bob should pay 12"   ! e1

    val toPay = Map("Bob"->"12", "Bill"->"10")           // a "database" of expected values
    val ShouldPay = "(.*) should pay (\\d+)".r           // a regular expression for extracting the name and price

    def e1 = (s: String) => {
      val ShouldPay(name, price) = s                     // extracting the values
      toPay(name) must_== price                          // using them for the expectation
    }
  }

  val exampleTextIndentation = new Specification { def is =
    "this is some presentation text"      ^
      "and the first example"             ! success^
      "and the second example"            ! success
  }

  val resetTextIndentation = new Specification { def is =
    "There are several options for displaying the text"      ^
      "xonly displays nothing but failures"                  ! success^
      "there is also a color option"                         ^              // this text will be indented
        "rgb=value uses that value to color the text"        ! rgb^         // and the following examples as well
        "nocolor dont color anything"                        ! nocolor^ end^
    "There are different ways of hiding the text"            ^              // this text will be properly indented now
        "by tagging the text"                                ! hideTag^
                                                             end
    def rgb = success
    def nocolor = success
    def hideTag = success
  }

  val pTextIndentation = new Specification { def is =
    "this is some presentation text"      ^
      "and the first example"             ! success^
      "and the second example"            ! success^
                                          p^
    "And another block of examples"       ^
      "with this example"                 ! success^
      "and that example"                  ! success^
                                          end
  }
}
