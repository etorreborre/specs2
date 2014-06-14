package org.specs2
package guide

import io._
import matcher._
import org.specs2.matcher.Scope
import org.specs2.mutable
import org.specs2.specification._
import specification.core.{Fragments}
import execute._

object Contexts extends UserGuidePage with FileMatchers with FileSystem {
  def is = ""

  val section = s2"""

## Contexts

In a specification some examples are very straightforward. They just check that a function is returning expected when given some inputs. However other examples can be more complex and require to execute in specific context:

 * with some state being setup before the example executes
 * with some clean up after the example is executed
 * inside a database context, with or without the possibility to access the transaction context
 * with state being setup before *all* examples
 * with clean up being done after *all* the examples

For all those situations, there is a ***specs2*** trait which you can mix in your specification.

### BeforeEach / AfterEach

The `org.specs2.specification.BeforeEach` trait defines an action that will be executed before each example:${
    snippet {
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
    }
  }

If you execute this specification you may see something like:
```console
[info] before
[info] before
[info] example2
[info] example1
```

As you guess, defining a behaviour "after" is very similar:${
    snippet {
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
    }
  }

You might also want to mix the 2:${
    snippet {
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
    }
  }

### AroundEach

Another very common situation is when you need to execute in the context of a database transaction or a web request. In this case you can use the `AroundEach` trait to execute each example in the proper context:${
    snippet {

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
    }
  }

The specification above shows a trait `DatabaseContext` extending `AroundEach` (so that trait can be reused for other specifications). It define a method named `around` taking the body of the example, anything with an ${"AsResult" ~ AsResultTypeclass} typeclass and returns a result. Because `r` is a byname parameter, you are free to do whatever you want before or after evaluating it, like opening and closing a database transaction.

The `AroundEach` trait can be used for lots of different purposes:

 - to re-execute examples a number of times
 - to time them out if they run for too long
 - to run them in different contexts, with different parameters

There is however one thing you cannot do with `AroundExample`. You can't pass the context to the example if it needs it. The `FixtureExample` trait solves this problem.

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

## If you want to know more

 * use ${"`Context` objects" ~ ContextObjects} to create contexts for just a few examples in the specification
 * use ${"traits and`Scopes`" ~ Scopes} to create contexts in unit specifications where you can access state directly on variables

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

The techniques described in ${"`Context` objects" ~ ContextObjects} are not always applicable to unit specifications where we want examples to be a "block" of code described by some text. Instead of creating a case class we can instantiate a trait which will hold the "fresh" state:${snippet{
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

It is also possible to extends Scopes with `Before` and `After` traits but they need to be `org.specs2.mutable.Before` and `org.specs2.mutable.After` traits. This is necessary because those traits extend the Scala `DelayedInit` trait allowing to insert code around the execution of the body of an object.

"""

  case class Tree[T](ts: T*) {
    def removeNodes(n: Int*) = Seq[Int]()
  }

}
/*
However, sometimes, we wish to go for a more concise way of getting fresh variables, without having to create a specific trait to encapsulate them. That's what the `isolated` argument is for.

#### Isolation

***specs2*** solves this issue in 2 ways:

 * simply by relying on Scala features, by creating a new trait or a case class to open a new `Scope` with fresh variables
 * by cloning the specification on each example execution when the `isolated` argument is provided

##### Isolated variables

The `isolated` argument changes the execution method so that each example is executed in a brand new instance of the Specification: ${snippet{

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
}}

Since there is a new Specification for each example, then all the variables accessible to the example will be seen as new.

_Note_: this technique will not work if the Specification is defined with a constructor having parameters because it won't be possible to create a new instance.


##### Contexts inheritance

One very cool property of using traits to define context variables is that we can use inheritance to describe more and more specific contexts: ${snippet{
// 8<--
def logInUser = User()
def pendingOrder = Order()
// 8<--
trait LoggedIn extends Scope {
  val user = logInUser
  // do something with the user
}

trait HasAPendingOrder extends LoggedIn {
  val order = createPendingOrder
  // the user is logged in
  // now do something with the user and his order
}
}}

##### In a mutable specification

You make your context trait extend the `${fullName[org.specs2.mutable.After]}` trait: ${snippet{
class ContextSpec extends org.specs2.mutable.Specification {
  "this is the first example" in new trees {
    tree.removeNodes(2, 3) must have size(2)
  }
  "this is the first example" in new trees {
    tree.removeNodes(2, 3, 4) must have size(1)
  }
}

trait trees extends org.specs2.mutable.After {
  lazy val tree = getATreeWith4NodesFromTheDatabase
  def after = cleanupDB()
}
}}

In this case, the clean-up code defined in the `after` method will be executed after each example. This is possible because the `mutable.After` trait extends the Scala `DelayedInit` trait allowing to insert code around the execution of the body of an object.

**Note**: the `org.specs2.mutable.{ Before, After, BeforeAfter }` traits only work for scala > 2.9.0 because previous Scala versions don't provide the `DelayedInit` trait.

Now we have both variable isolation and non-duplication of set-up code!

But there is more to it. The next paragraphs will show how to:

1. execute the body of each example inside a specific context: `Around`
1. set-up a context object (say a http query) and pass it to each example: `Outside`
1. declare a `before` method for all the examples of a Specification without even having to create a context object
1. create a new context object by combining existing ones

#### Around

Some examples need to be executed in a given context. For example you're testing a web application and your specification code needs to have your example executed inside an Http session.

In that case you can extend the `Around` trait and specify the `around` method: ${snippet{
// 8<--
lazy val (e1, e2) = (ok, ok)
// 8<--
object http extends Around {
  def around[T : AsResult](t: =>T) =
  ("test") {
    AsResult(t)  // execute t inside a http session
  }
}

"this is a first example where the code executes inside a http session" ! http(e1)
"and another one"                                                       ! http(e2)
}}

Note that the context here is an object instead of a trait or case class instance because in this specification we don't need any variable isolation. We also take the advantage that objects extending `Context` traits (like `Before` / `After` / `Around`,...) have an `apply` method so we can directly write `http(e1)` meaning `http.apply(e1)`.




#### Composition

##### Combinations

***specs2*** contexts can be combined in several ways. When you want to define both `Before` and `After` behavior, you can do it by extending the `${simpleName[BeforeAfter]}` trait: ${snippet{

case class withFile() extends BeforeAfter {
  def before = createFile("test")
  def after  = deleteFile("test")
}
}}

Similarly you can use `BeforeAfterAround` instead of `Before with After with Around`.

##### Composition

Contexts can be also be _composed_ but only if they are of the same type, `Before` with `Before`, `After` with `After`,... ${snippet{

case class withFile() extends Before {
  def before = createFile("test")
}
case class withDatabase() extends Before {
  def before = openDatabase("test")
}
val init = withFile() compose withDatabase()

"Do something on the full system" ! init(success)
}}

#### Console output

Some specifications might be using libraries which tend to be immoderately logging their activity. If you want to temporarily switch off any console display during the execution of the examples you can add the `org.specs2.reporter.NoStdOutAroundExample` trait. This trait creates an `Around` context for example and re-directs the output to an empty stream until the example is finished. It then resets the output to the default `System.out` output. Of course since `System.out` and `Console.out` are shared resources this might not work so well when specifications or examples are executed concurrently. In this case you might consider using `fork in Test := true` within sbt or `sequential` within specs2 to get a proper display.

#### Steps/Actions

##### Steps

Some set-up actions are very time-consuming and should be executed only once for the whole specification. This can be achieved by inserting some silent `Step`s in between fragments: ${snippet{

class DatabaseSpec extends Specification { def is = s2"""

This specification opens a database and execute some tests       ${ step(openDatabase("test")) }
  example 1                                                      $success
  example 2                                                      $success
                                                                 ${ step(closeDatabase("test")) }
  """
}
}}

The examples are (by default) executed concurrently between the 2 steps and the "result" of those steps will never be reported unless if there is a failure.

##### Actions

`Step`s are very useful because they will really be executed sequentially, before anything else, but if you need to execute some actions which are completely independent of the rest of the specification, there is an equivalent to `Step` adequately called `Action`: ${snippet{
class DatabaseSpec extends Specification { def is = s2"""

  This specification opens a database and execute some tests"     ${ step(openDatabase("test")) }
    example 1                                                     $success
    add 1 to the number of specification executions"              ${ action(db.executionsNb += 1) }
    example 2                                                     $success
                                                                  ${ step(closeDatabase("test")) }
                                                                  """
}
}}

Of course, `Step`s and `Action`s are not the privilege of acceptance specifications: ${snippet{

class DatabaseSpec extends org.specs2.mutable.Specification {

  text("This specification opens a database and execute some tests")
  step(openDatabase("test"))

  "example 1" in success

  text("add 1 to the number of specification executions")
  action(db.executionsNb += 1)

  "example 2" in success
  step(closeDatabase("test"))
}
}}

#### Template

There may still be some duplication of code if you have to use the same kind of set-up procedure for several specifications.

If that's the case you can define your own `Specification` trait doing the job: ${snippet{

trait DatabaseSpec extends Specification {
  /** the map method allows to "post-process" the fragments after their creation */
  override def map(fs: =>Fragments) = step(startDb) ^ fs ^ step(cleanDb)
}
}}

The `DatabaseSpec` above will insert, in each inherited specification, one `Step` executed before all the fragments, and one executed after all of them.

#### Global setup/teardown

The next similar thing you might want to do is to do some setup, like create a database schema, once and for all before *any* specification runs. The easiest way to do that is to use an object and a lazy variable: ${snippet{

object databaseSetup  {
  lazy val createDb = { println("creating the database") }
}

// use the createDB lazy val to create the database once for every specification inheriting from
// the DatabaseSpec trait
trait DatabaseSpec extends Specification {
  lazy val dbSetup = databaseSetup
  override def map(fs: =>Fragments) = step(dbSetup.createDb) ^ step(startDb) ^ fs ^ step(cleanDb)
}
}}

Note also that global setup and cleanup can be [done with sbt](http://www.scala-sbt.org/release/docs/Detailed-Topics/Testing#setup-and-cleanup).

#### For fragments

When using a Unit Specification, it can be useful to use variables which are only used for a given set of examples. This can be easily done by declaring local variables, but this might lead to duplication. One way to avoid that is to use the `org.specs2.mutable.NameSpace` trait: ${snippet{
// 8<--
class s extends org.specs2.mutable.Specification {
// 8<--
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
// 8<--
}
// 8<--
}}
"""
*/



