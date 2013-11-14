package org.specs2
package guide
package structure

import specification.{Step, Action, Around, Before, After, Outside, AroundOutside, BeforeExample, AfterExample, BeforeAfter, Fragments, Scope, Fixture, FixtureExample}
import mutable.NameSpace
import execute._
import matcher.FileMatchers
import io._

object Contexts extends UserGuidePage with FileMatchers with FileSystem { def is = ""
  val section = s2"""

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

Let's see an example of using a `Scope` with a mutable specification: ${snippet{

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
}}

Each example of that specification gets a new instance of the `trees` trait. So it will have a brand new `tree` variable and even if this data is mutated by an example, other examples will be isolated from these changes.

Now you might wonder why the `trees` trait is extending the `org.specs2.specification.Scope` trait? The reason is that the body of an Example only accepts objects which are convertible to a `Result`. By extending `Scope` we can take advantage of an implicit conversion provided by the `Specification` trait to convert our context object to a `Result`.

Scopes are a way to create a "fresh" object and associated variables for each example being executed. The advantages are that:

 - those classes can be reused and extended
 - the execution behavior only relies on language constructs

However, sometimes, we wish to go for a more concise way of getting fresh variables, without having to create a specific trait to encapsulate them. That's what the `isolated` argument is for.

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

##### Case classes

The same kind of variable isolation can be achieved in acceptance specifications by using case classes: ${snippet{
class ContextSpec extends Specification { def is = s2"""
  this is the first example                          ${trees().e1}
  this is the second example                         ${trees().e2}
  """
}

case class trees() {
  val tree = createATreeWith4Nodes

  def e1 = tree.removeNodes(2, 3) must have size(2)
  def e2 = tree.removeNodes(2, 3, 4) must have size(1)
}
}}

In this case we don't need to extend the `Scope` trait because the examples `e1` and `e2` already return `Result`s.

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

#### Before/After

If you want to run some code before or after each example, the `Before` and `After` traits are there to help you (they both extend the `Scope` trait). In the following examples we'll only show the use of `After` because `Before` most of the time unnecessary: ${snippet{
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
}}

Indeed when you have setup code you can do anything you want in the body of your context trait and this will be executed before the example body. However this wouldn't work with teardown code, so let's see how to use the `After` trait.

##### In a mutable specification

You make your context trait extend the `${fullName[mutable.After]}` trait: ${snippet{
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
  def after = cleanupDB()
}
}}

In this case, the clean-up code defined in the `after` method will be executed after each example. This is possible because the `mutable.After` trait extends the Scala `DelayedInit` trait allowing to insert code around the execution of the body of an object.

**Note**: the `org.specs2.mutable.{ Before, After, BeforeAfter }` traits only work for scala > 2.9.0 because previous Scala versions don't provide the `DelayedInit` trait.

##### In an acceptance specification

In that case you would extend the `specification.After` trait and use the `apply` method: ${snippet{

class ContextSpec extends Specification { def is = s2"""
  this is the first example                          ${trees().e1}
  this is the second example                         ${trees().e2}
  """
  case class trees() extends specification.After {
    lazy val tree = getATreeWith4NodesFromTheDatabase
    def after = cleanupDB()

    // this is equivalent to: def e1 = this.apply { ... }
    def e1 = this { tree.removeNodes(2, 3) must have size(2) }
    def e2 = this { tree.removeNodes(2, 3, 4) must have size(1) }
  }
}
}}

Now we have both variable isolation and non-duplication of set-up code!

But there is more to it. The next paragraphs will show how to:

1. execute the body of each example inside a specific context: `Around`
1. set-up a context object (say a http query) and pass it to each example: `Outside`
1. declare a `before` method for all the examples of a Specification without even having to create a context object
1. use an implicit context to avoid duplication
1. create a new context object by combining existing ones

#### Around

Some examples need to be executed in a given context. For example you're testing a web application and your specification code needs to have your example executed inside an Http session.

In that case you can extend the `Around` trait and specify the `around` method: ${snippet{
// 8<--
lazy val (e1, e2) = (ok, ok)
// 8<--
object http extends Around {
  def around[T : AsResult](t: =>T) = openHttpSession("test") {
    AsResult(t)  // execute t inside a http session
  }
}

"this is a first example where the code executes inside a http session" ! http(e1)
"and another one"                                                       ! http(e2)
}}

Note that the context here is an object instead of a trait or case class instance because in this specification we don't need any variable isolation. We also take the advantage that objects extending `Context` traits (like `Before` / `After` / `Around`,...) have an `apply` method so we can directly write `http(e1)` meaning `http.apply(e1)`.

#### Outside

`Outside` is bit like `Around` except that you can get access to the application state that you're setting in your Context object. Let's see that with an example (with a mutable Specification for a change): ${snippet{
// 8<--
class s extends mutable.Specification {
// 8<--
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
// 8<--
}
// 8<--
}}

##### AroundOutside

We can also combine both the `Around` and the `Outside` behaviors with the `AroundOutside` trait: ${snippet{

object http extends AroundOutside[HttpReq] {
  // create a context
  def around[T : AsResult](t: =>T) = {
    createNewDatabase()
    // execute the code inside a databaseSession
    inDatabaseSession { AsResult(t) }
  }
  // prepare a valid HttpRequest
  def outside: HttpReq = createRequest
}

"this is a first example where the code executes uses a http request" ! http((request: HttpReq) => success)
"and another one"                                                     ! http((request: HttpReq) => success)
}}

#### Fixture

Finally, the way to get the most control on the data that is passed to each example and how it is executed is to use a `Fixture`: ${snippet{

val evenNumbers = new Fixture[Int] {
  def apply[R : AsResult](f: Int => R) = {
    // test f with 1, 2, 3
    Seq(1, 2, 3).foldLeft(Success(): Result) { (res, i) =>
      res and AsResult(f(i))
    }
  }
}

"even numbers can be divided by 2" ! evenNumbers { i: Int => i % 2 === 0 }
}}

The example above tests repeatedly the same code with different values (you could add before or after actions if you wanted to). As you can see, the fixture can be applied explicitly to the example body but you can as well declare it as an implicit context or use the `FixtureExample` trait to pass the fixture implicitly: ${snippet{

// passing the fixture implicitly
class MySpec1 extends Specification { def is = s2"""
  even numbers can be divided by 2 ${ i: Int => i % 2 === 0 }
                                                                          """
  implicit val evenNumbers: Fixture[Int] = ???
}

// using the FixtureExample trait
class MySpec2 extends Specification with FixtureExample[Int] { def is = s2"""
  "even numbers can be divided by 2" ${ i: Int => i % 2 === 0 }
  """

  def fixture[R : AsResult](f: Int => R): Result = ???
}
}}

#### BeforeExample

When you just need to have set-up code executed before each example and if you don't need to have variable isolation, you can simply use the `${fullName[BeforeExample]}` trait.

The `${fullName[BeforeExample]}` trait allows you to define a `before` method exactly like the one you define in the `Before` trait and apply it to all the examples of the specification: ${snippet{

class MySpecification extends mutable.Specification with BeforeExample {
  def before = cleanDatabase()

  "This is a specification where the database is cleaned up before each example" >> {
    "first example" in { success }
    "second example" in { success }
  }
}
}}

As you can guess, the `AfterExample`, `AroundExample`,... traits work similarly by requiring the corresponding `after`, `around`,... methods to be defined.

#### Implicit context

The `BeforeExample` trait is a nice shortcut to avoid the creation of a context object, but there is another possibility to avoid the repetition of the context name for each example. If your specification is: ${snippet{

class ContextSpec extends mutable.Specification {
  val myContext = new Before { def before = cleanUp }

  "This is a specification where the database is cleaned up before each example" >> {
    "first example" in myContext { 1 must_== 1 }
    "second example" in myContext { 1 must_== 1 }
  }
}
}}

You can simply mark your context object as `implicit` and it will be automatically passed to each example: ${snippet{

class ContextSpec extends mutable.Specification {
  implicit val myContext = new Before { def before = cleanUp }

  "This is a specification where the database is cleaned up before each example" >> {
    "first example"  in { 1 must_== 1 }
    "second example" in { 1 must_== 1 }
  }
}
}}

There is just one gotcha that you need to be aware of. If your implicit context is an `Outside[String]` context this will not work: ${snippet{

class ContextSpec extends mutable.Specification {
  implicit val myContext = new Outside[String] { def outside = "hello" }

  "This is a specification uses a new String in each example" >> {
    "first example"  in { (s: String) => s must_== s }
    "second example" in { (s: String) => s must_== s }
  }
}
}}

Indeed in both examples above the `s` string that will be passed is the Example description as specified [here](#Use+descriptions).

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

#### Steps/Actions

##### Steps

Some set-up actions are very time-consuming and should be executed only once for the whole specification. This can be achieved by inserting some silent `Step`s in between fragments: ${snippet{

class DatabaseSpec extends Specification { def is = s2"""

This specification opens a database and execute some tests       ${ Step(openDatabase("test")) }
  example 1                                                      $success
  example 2                                                      $success
                                                                 ${ Step(closeDatabase("test")) }
  """
}
}}

The examples are (by default) executed concurrently between the 2 steps and the "result" of those steps will never be reported unless if there is a failure.

##### Actions

`Step`s are very useful because they will really be executed sequentially, before anything else, but if you need to execute some actions which are completely independent of the rest of the specification, there is an equivalent to `Step` adequately called `Action`: ${snippet{
class DatabaseSpec extends Specification { def is = s2"""

  This specification opens a database and execute some tests"     ${ Step(openDatabase("test")) }
    example 1                                                     $success
    add 1 to the number of specification executions"              ${ Action(db.executionsNb += 1) }
    example 2                                                     $success
                                                                  ${ Step(closeDatabase("test")) }
                                                                  """
}
}}

Of course, `Step`s and `Action`s are not the privilege of acceptance specifications: ${snippet{

class DatabaseSpec extends mutable.Specification {

  textFragment("This specification opens a database and execute some tests")
  step(openDatabase("test"))

  "example 1" in success

  textFragment("add 1 to the number of specification executions")
  action(db.executionsNb += 1)

  "example 2" in success
  step(closeDatabase("test"))
}
}}

#### Template

There may still be some duplication of code if you have to use the same kind of set-up procedure for several specifications.

If that's the case you can define your own `Specification` trait doing the job: ${snippet{

import org.specs2._
import specification._

trait DatabaseSpec extends Specification {
  /** the map method allows to "post-process" the fragments after their creation */
  override def map(fs: =>Fragments) = Step(startDb) ^ fs ^ Step(cleanDb)
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
  override def map(fs: =>Fragments) = Step(dbSetup.createDb) ^ Step(startDb) ^ fs ^ Step(cleanDb)
}
}}

Note also that global setup and cleanup can be [done with sbt](http://www.scala-sbt.org/release/docs/Detailed-Topics/Testing#setup-and-cleanup).

#### For fragments

When using a Unit Specification, it can be useful to use variables which are only used for a given set of examples. This can be easily done by declaring local variables, but this might lead to duplication. One way to avoid that is to use the `${fullName[NameSpace]}` trait: ${snippet{
// 8<--
class s extends mutable.Specification {
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
  case class User()
  case class Order()
  def createPendingOrder = Order()

  case class Tree[T](ts: T*) {
    def removeNodes(n: Int*) = Seq[Int]()
  }
  def cleanupDB() = ()
  def createATreeWith4Nodes = new Tree()
  def getATreeWith4NodesFromTheDatabase = new Tree()
  def setupDB() = ()

  def openHttpSession[T](name: String)(r: Result) = r
  case class HttpReq()
  def createRequest = HttpReq()

  def createNewDatabase() = ()
  def openDatabase(name: String) = ()
  def closeDatabase(name: String) = ()
  case class DB(var executionsNb: Int = 0)
  lazy val db = DB()
  def cleanDatabase() = ()
  def cleanUp() = ()
  def startDb() = ()
  def cleanDb() = ()
  def deleteFile(name: String) = ()
  def inDatabaseSession[T](r: Result) = r
}

