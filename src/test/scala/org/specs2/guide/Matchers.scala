package org.specs2
package guide

class Matchers extends Specification { def is =  "Matchers guide".title ^                                               """

There are many ways to define expectations in ***specs2***. You can define expectations with anything that returns
a `Result`:

  * Boolean
  * Standard result
  * Matcher result
  * Scalacheck property
  * Mock expectation
  * DataTable

### Boolean results

This is the simplest kind of result you can define for an expectation but also the least expressive!

Here's an example:

    "This is hopefully true"         ! (1 != 2)

This can be useful for simple expectations but a failure will give few information on what went wrong:

    "This is hopefully true"         ! (2 != 2) // fails with 'the value is false',...

### Standard results

Some standard results can be used when you need specific result meanings:

  * success: the example is ok
  * failure: there is a non-met expectation
  * anError: a non-expected exception occurred
  * skipped: the example is skipped possibly at runtime because some conditions are not met. A more specific message can
    be created with `Skipped("my message")`
  * pending: usually means "not implemented yet", but a specific message can be created with `Pending("my message")`

Two additional results are also available to track the progress of features:

  * done: a Success with the message "DONE"
  * todo: a Pending with the message "TODO"

### Match results

This is by far the largest category of Results in ***specs2***. They cover many data types, can composed and adapted to
create new ones or be created from scratch by the user. Let's have a look at some of them and refer the reader to the
API for the complete list:

 * Matchers for Any
 * Option / Either matchers
 * String matchers
 * Numeric matchers
 * Exception matchers
 * Iterable matchers
 * Map matchers
 * Xml matchers
 * File matchers
 * Scalaz matchers

#### Matchers for Any

The most common type of matcher is `beEqualTo` to test for equality. There are different ways to use this matcher:

       1 must beEqualTo(1)
       1 must_== 1                // my favorite!
       1 should_== 1              // for should lovers
       1 === 1                    // the ultimate shortcut
       1 must be equalTo(1)       // with a literate style
       1 must not be equalTo(2)   // with a negation

You can see on the examples above several things which are applicable to all matchers:

 * the general form for using a matcher is `a must matcher`
 * you can use `should` instead of `must` if you prefer
 * there are only 2 shortcuts provided because the equality matcher is so ubiquitous `must_==` and `===`
 * for most of the matchers you can use a form where the `be` word (or the `have` word) is detached
 * you can as well negate a matcher by adding not before (but also after as a method on the matcher)

An non-exhaustive list of those matchers:

 * `be` for checking if `a eq b`
 * `beTrue, beFalse`
 * `beLike { case exp => ok }`: to check if an object is like a given pattern
 * `beLike { case exp => exp must beXXX }`: to check if an object is like a given pattern, and verifies a condition
 * `beNull`
 * `beAsNullAs`: when 2 objects must be null at the same time if one of them is null
 * `beOneOf(a, b, c)`: to check if an object is one of a given list
 * `haveClass`: to check the class of an object
 * `beAssignableFrom`: to check if a class is assignable from another

##### With a better description



#### Matchers for Option / Either

                                                                                                                        """ ^
  include(xonly, examples)
                                                                                                                        end

  val examples = new Specification { def is = "Examples".title ^
    "This is hopefully true"         ! (1 != 2)     ^
    { 1 must beEqualTo(1)      }                    ^
    { 1 must_== 1              }                    ^ // my favorite!
    { 1 should_== 1            }                    ^ // for should lovers
    { 1 === 1                  }                    ^ // the ultimate shortcut
    { 1 must be equalTo(1)     }                    ^ // with a literate style
    { 1 must not be equalTo(2) }                    ^ // with a negation
                                                    end
  }

}