package org.specs2
package guide

import io._
import matcher._
import org.specs2.matcher.Scope
import org.specs2.mutable
import org.specs2.specification._
import specification.core.{Fragments}
import execute._

object Contexts extends UserGuidePage with FileMatchers with FileSystem { def is = s2"""

In a specification some examples are very straightforward. They just check that a function is returning expected values when given some inputs. However other examples can be more complex and require to execute in a specific context:

 * with some state being setup before the example executes
 * with some state being cleaned up after the example is executed
 * inside a database context, with or without the possibility to access the transaction context
 * with state being setup before *all* examples
 * with state being cleaned up after *all* the examples

For all those situations, there is a $specs2 trait which you can mix in your specification.

### BeforeEach / AfterEach

The `org.specs2.specification.BeforeEach` trait defines an action that will be executed before each example:${snippet {
class BeforeSpecification extends mutable.Specification with BeforeEach {
  // you need to define the "before" action
  def before = println("before")
  "example 1" >> {
    println("example1"); ok
  }
  "example 2" >> {
    println("example2"); ok
  }
}
}}

If you execute this specification you may see something like:
```console
[info] before
[info] before
[info] example2
[info] example1
```

As you can guess, defining a behaviour "after" is very similar:${snippet {
class AfterSpecification extends mutable.Specification with AfterEach {
  // you need to define the "after" action
  def after = println("after")

  "example 1" >> {
    println("example1"); ok
  }
  "example 2" >> {
    println("example2"); ok
  }
}
}}

You might also want to mix the two:${snippet {
class BeforeAfterSpecification extends mutable.Specification with BeforeAfterEach {

  def before = println("before")
  def after  = println("after")

  "example 1" >> {
    println("example1"); ok
  }
  "example 2" >> {
    println("example2"); ok
  }
}
}}

### AroundEach

Another very common situation is when you need to execute in the context of a database transaction or a web request. In this case you can use the `AroundEach` trait to execute each example in the proper context:${snippet {
trait DatabaseContext extends AroundEach {
  // you need to define the "around" method
  def around[R: AsResult](r: => R): Result = {
    openDatabaseTransaction
    try AsResult(r)
    finally closeDatabaseTransaction
  }
  // do what you need to do with the database
  def openDatabaseTransaction = ???
  def closeDatabaseTransaction = ???
}

class AroundSpecification extends mutable.Specification with DatabaseContext {
  "example 1" >> {
    println("using the database"); ok
  }
  "example 2" >> {
    println("using the database too"); ok
  }
}
}}

The specification above shows a trait `DatabaseContext` extending `AroundEach` (so that trait can be reused for other specifications). It defines a method named `around` taking the body of the example, anything with an ${"AsResult" ~/ AsResultTypeclass} typeclass, and returns a result. Because `r` is a byname parameter, you are free to do whatever you want before or after evaluating it, like opening and closing a database transaction.

The `AroundEach` trait can be used for lots of different purposes:

 - to re-execute examples a number of times
 - to time them out if they run for too long
 - to run them in different contexts, with different parameters

There is however one thing you cannot do with `AroundExample`. You can't pass the context to the example if it needs it. The `ForEach` trait solves this problem.

### ForEach

Sometimes you need to create a specific context for each example but you also want to make it accessible to each example. Here is a specification having examples using an active database transaction:${snippet {
// a transaction with the database
trait Transaction

trait DatabaseContext extends ForEach[Transaction] {
  // you need to define the "foreach" method
  def foreach[R: AsResult](f: Transaction => R): Result = {
    val transaction = openDatabaseTransaction
    try AsResult(f(transaction))
    finally closeDatabaseTransaction(transaction)
  }

  // create and close a transaction
  def openDatabaseTransaction: Transaction = ???

  def closeDatabaseTransaction(t: Transaction) = ???
}

class FixtureSpecification extends mutable.Specification with DatabaseContext {
  "example 1" >> { t: Transaction =>
    println("use the transaction")
    ok
  }
  "example 2" >> { t: Transaction =>
    println("use it here as well")
    ok
  }
}
}}

### BeforeAll / AfterAll

Some setups are very expensive and can be shared across all examples. For example you might want to start an application server just at the beginning of the specification and then close it at the end. You can use 3 traits to do this:

 * `BeforeAll` inserts a `Step` before all the examples
 * `AfterAll` inserts a `Step` after all the examples
 * `BeforeAfterAll` inserts one `Step` before all the examples and one `Step` after all of them

$AndIfYouWantToKnowMore

 * read about $specs2 ${"execution model" ~/ Execution} to understand how `Examples` and `Steps` are being executed
 * use ${"`Context` objects" ~ ContextObjects} to create contexts for just a few examples in the specification
 * use ${"traits and`Scopes`" ~ Scopes} to create contexts in unit specifications where you can access state directly on variables

$vid
"""
}


