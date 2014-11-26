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

In a specification some examples are very straightforward. They just check that a function is returning expected values when given some inputs. However other examples can be more complex and require to execute in specific context:

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

  def after = println("after")

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

Sometimes you need to create a specific context for each example but also make it accessible to each example. Here is a specification having examples using an active database transaction:${snippet {
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

 * use ${"`Context` objects" ~ ContextObjects} to create contexts for just a few examples in the specification
 * use ${"traits and`Scopes`" ~ Scopes} to create contexts in unit specifications where you can access state directly on variables

$vid
"""
}

object ContextObjects extends UserGuidePage { def is = s2"""

In the ${see(Contexts)} section we have seen how to create contexts for each example in a specification. Whilst you generally want to group all the examples having the same kind of setup in the same specification, this is not always the case. So if you have a situation where you just need to create a "local" context for just a few examples here is what you can do.

### Use case classes

In an acceptance specification you can simply use case classes to get a "fresh" context on some examples: ${snippet{
class ContextSpec extends Specification { def is = s2"""
  this is the first example                          ${trees().e1}
  this is the second example                         ${trees().e2}
  """

  case class trees() {
    val tree = createATreeWith4Nodes

    // each example has access to a brand new tree object
    def e1 = tree.removeNodes(2, 3) must have size(2)
    def e2 = tree.removeNodes(2, 3, 4) must have size(1)
  }
}
}}

If you also want to include some setup/cleanup behavior you can use the `Before` or `After` traits (or `BeforeAfter` or `Around`): ${snippet{
class ContextSpec extends Specification { def is = s2"""
  this is the first example                          ${trees().e1}
  this is the second example                         ${trees().e2}
"""

  case class trees() extends specification.After {
    lazy val tree = getATreeWith4NodesFromTheDatabase

    // you need to define the "after" method
    def after = cleanupDB()

    // this is equivalent to: def e1 = this.apply { ... }
    def e1 = this { tree.removeNodes(2, 3) must have size(2) }
    def e2 = this { tree.removeNodes(2, 3, 4) must have size(1) }
  }
}
}}

As you can see the `Before`, `After`,... traits are very similar to their `BeforeEach`, `AfterEach`,... counterparts. One good thing about this technique is that each example has access to the current state being set (a bit like when using the `ForEach` trait).

### Context object

A slightly different technique consists in creating objects extending `Before`, `After`, `BeforeAfter` or `Around` so that you can reuse them independently of examples:${snippet{
object http extends Around {
  def around[T : AsResult](t: =>T) = openHttpSession("test") {
     AsResult(t)  // execute t inside a http session
  }
}

s2"""
  this is a first example where the code executes inside a http session" ! http(e1)
  and another one                                                        ! http(e2)
"""

  def e1 = ok // do something
  def e2 = ok // here too

}}

This works because each "context" object has an `apply` method taking `R : AsResult` and returning `Result`.

Finally a last kind of "context" object, a `Fixture` can be used to inject some state:${snippet{
val evenNumbers = new Fixture[Int] {
  def apply[R : AsResult](f: Int => R) = {
    // test f with 1, 2, 3
    Seq(1, 2, 3).foldLeft(Success(): Result) { (res, i) =>
      res and AsResult(f(i))
    }
  }
}

s2"even numbers can be divided by 2  $e1"

def e1 = evenNumbers { i: Int => i % 2 === 0 }
}}

"""

  case class Tree[T](ts: T*) {
    def removeNodes(n: Int*) = Seq[Int]()
  }
  def cleanupDB() = ()
  def createATreeWith4Nodes = new Tree()
  def getATreeWith4NodesFromTheDatabase = new Tree()
  def openHttpSession[R : AsResult](name: String)(r: =>Result) = r

}

object Scopes extends UserGuidePage { def is = s2"""

### Scope

The techniques described in ${"`Context` objects" ~/ ContextObjects} are not always applicable to unit specifications where we want examples to be a "block" of code described by some text. Instead of creating a case class we can instantiate a trait which will hold a "fresh" state:${snippet{
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
}}

Each example of that specification gets a new instance of the `trees` trait. So it will have a brand new `tree` variable and even if this data is mutated by an example, other examples will be isolated from these changes.

Now you might wonder why the `trees` trait is extending the `org.specs2.specification.Scope` trait? The reason is that the body of an Example only accepts objects which are convertible to a `Result`. By extending `Scope` we can take advantage of an implicit conversion provided by the `Specification` trait to convert our context object to a `Result`.

### Before / After

It is also possible to extend Scopes with `Before` and `After` traits but they need to be `org.specs2.mutable.Before` and `org.specs2.mutable.After` traits. This is necessary because those traits extend the Scala `DelayedInit` trait allowing to insert code around the execution of the body of an object.

$AndIfYouWantToKnowMore

 - print ${"execution data" ~/ PrintExecutionData}

$vid
"""

  case class Tree[T](ts: T*) {
    def removeNodes(n: Int*) = Seq[Int]()
  }

}




