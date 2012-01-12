package org.specs2
package guide

import text.Markdown
import form.{TextCell, Tab, Form, Tabs}
import specification.Forms._
import Form._

trait MatcherCards extends Cards {
  def title = "Matchers"
  def cards = Seq(AnyMatchers,
                  OptionEitherMatchers,
                  StringMatchers,
                  NumericMatchers,
                  ExceptionMatchers,
                  IterableMatchers,
                  MapMatchers
                  )
}
trait Cards {
  def title: String
  def cards: Seq[Card]
  def toTabs = Form(title).tabs(cards)((card: Card) => Tabs(Seq(card.toTab)))
}

trait Card {
  def title: String
  def text: String
  def toTab: Tab = Tab(title, Form.tr(TextCell(text).bkWhite))
}
object AnyMatchers extends Card {
  def title = "Matchers for Any"
  def text =  """
  The most common type of matcher is `beEqualTo` to test for equality. There are different ways to use this matcher:

 Matcher                    |  Comment
 -------------------------- | --------------------------
 `1 must beEqualTo(1)      `| the normal way
 `1 must be_==(1)          `| with a shorter matcher
 `1 must_== 1              `| my favorite!
 `1 mustEqual 1            `| if you dislike underscores
 `1 should_== 1            `| for should lovers
 `1 === 1                  `| the ultimate shortcut
 `1 must be equalTo(1)     `| with a literate style

   *with a negation*        |
 -------------------------- |
 `1 must not be equalTo(2) `|
 `1 must_!= 2              `|
 `1 mustNotEqual 2         `|
 `1 must be_!=(2)          `|
 `1 !== 2                  `|

The `beEqualTo` matcher is using the regular `==` Scala equality. However in the case of `Arrays`, Scala `==` is just using reference equality, `eq`, for `Arrays`. So the `beEqualTo` matcher has been adapted to transform `Arrays` to `Seqs` before checking for equality, so that `Array(1, 2, 3) === Array(1, 2, 3)` (despite the fact that `Array(1, 2, 3) != Array(1, 2, 3)`).

You can see on the examples above several things which are applicable to all matchers:

 * the general form for using a matcher is `a must matcher`
 * you can use `should` instead of `must` if you prefer
 * there are only 2 shortcuts provided because the equality matcher is so ubiquitous `must_==` and `===`
 * for most of the matchers you can use a form where the ` be` word (or the `have` word) is detached
 * you can as well negate a matcher by adding not before it (or after it, as a method call)

An non-exhaustive list of those matchers:

 * `be_===` for checking if `a == b` where a and b are expected to have the same type by the compiler
 * `be_==~` for checking if `(a:A) == (b:A)` when there is an implicit conversion from B (the type of b) to A (the type of a)
 * `beTheSameAs` for checking if `a eq b` (`a must be(b)` also works)
 * `beTrue, beFalse`
 * `beLike { case exp => ok }`: to check if an object is like a given pattern (`ok` is a predefined value, `ko` is the opposite)
 * `beLike { case exp => exp must beXXX }`: to check if an object is like a given pattern, and verifies a condition
 * `beNull`
 * `beAsNullAs`: when 2 objects must be null at the same time if one of them is null
 * `beOneOf(a, b, c)`: to check if an object is one of a given list
 * `haveClass`: to check the class of an object
 * `haveSuperclass`: to check if the class of an object as another class as one of its ancestors
 * `haveInterface`: to check if an object is implementing a given interface
 * `beAssignableFrom`: to check if a class is assignable from another
 * `beAnInstanceOf[T]`: to check if an object is an instance of type `T`

  """
}

object OptionEitherMatchers extends Card {
  def title = "Option/Either Matchers"
  def text =  """
  There are several matchers to check Option and Either instances:

 * `beSome` checks if an element is Some(_)
 * `beSome.which(function)` checks if an element is Some(_) and satisfies a function returning a boolean
 * `beSome.like(partial function)` checks if an element is Some(_) and satisfies a partial function returning a `MatchResult`
 * `beNone` checks if an element is None
 * `beAsNoneAs` checks if 2 values are equal to None at the same time
 * `beRight` checks if an element is Right(_)
 * `beRight.like(partial function)` checks if an element is Right(_) and satisfies a partial function returning a `MatchResult`
 * `beLeft` checks if an element is Left(_)
 * `beLeft.like(partial function)` checks if an element is Left(_) and satisfies a partial function returning a `MatchResult`
  """
}

object StringMatchers extends Card {
  def title = "String Matchers"
  def text =  """
Matching on strings is very common. Here are the matchers which can help you:

* `beMatching` (or ` be matching`) checks if a string matches a regular expression
* `=~(s)` is a shortcut for `beMatching(".*"+s+".*")`
* `find(exp).withGroups(a, b, c)` checks if some groups are found in a string
* `have length` checks the length of a string
* `have size` checks the size of a string (seen as an `Iterable[Char]`)
* `be empty` checks if a string is empty
* `beEqualTo(b).ignoreCase` checks if 2 strings are equal regardless of casing
* `beEqualTo(b).ignoreSpace` checks if 2 strings are equal when trimmed
* `beEqualTo(b).ignoreSpace.ignoreCase` you can compose them
* `contain(b)` checks if a string contains another one
* `startWith(b)` checks if a string starts with another one
* `endWith(b)` checks if a string ends with another one
"""
}

object NumericMatchers extends Card {
  def title = "Numeric Matchers"
  def text =  """
Less often you need to do comparisons on Numerical values:

* `beLessThanOrEqualTo` compares any Ordered type with `<=`
`1 must be_<=(2)`
`1 must beLessThanOrEqualTo(2)`

* `beLessThan` compares any Ordered type with `<`
`1 must be_<(2)`
`1 must beLessThan(2)`

* `beGreaterThanOrEqualTo` compares any Ordered type with `>=`
`2 must be_>=(1)`
`2 must beGreaterThanOrEqualTo(1)`

* `beGreaterThan` compares any Ordered type with `>`
`2 must be_<(1)`
`2 must beGreaterThan(1)`

* `beCloseTo` checks if 2 Numerics are close to each other
`1.0 must beCloseTo(1, 0.5)`
`4 must be ~(5 +/- 2)`
"""
}

object ExceptionMatchers extends Card {
  def title = "Exception Matchers"
  def text =  """
***specs2*** offers very compact ways of checking that some exceptions are thrown:

* `throwA[ExceptionType]` checks if a block of code throws an exception of the given type
* `throwA[ExceptionType](message = "boom")` additionally checks if the exception message is as expected
* `throwA(exception)` or `throwAn(exception)` checks if a block of code throws an exception of the same type, with the
same message
* `throwA[ExceptionType].like { case e => e must matchSomething }` or
`throwA(exception).like { case e => e must matchSomething }` allow to verify that the thrown exception satisfies a property
* `throwA[ExceptionType](me.like { case e => e must matchSomething }` or
`throwA(exception).like { case e => e must matchSomething }` allow to verify that the thrown exception satisfies a property

For all the above matchers you can use `throwAn` instead of `throwA` if the exception name starts with a vowel for better
readability.
"""
}

object IterableMatchers extends Card {
  def title = "Iterable Matchers"
  def text =  """
Iterables can be checked with several matchers:

* to check if the iterable is empty
`Nil must be empty`
`List(1, 2, 3) must not be empty`

* to check if some elements are contained in the iterable
`List(1, 2, 3) must contain(3, 2)`

* to check if some elements are contained in the iterable in the same order
`List(1, 2, 3, 4) must contain(2, 4).inOrder`

* to check if only some elements are contained in the iterable
`List(4, 2) must contain(2, 4).only`

* to check if only some elements are contained in the iterable and in the same order
`List(2, 4) must contain(2, 4).only.inOrder`

* to check if a sequence contains another one
`List(2, 4) must containAllOf(List(4, 2))`
`List(2, 4) must containAllOf(List(2, 4)).inOrder`

* to check if a sequence contains any element of another one
`List(2, 4) must containAnyOf(List(4, 2))`

* to check the size of an iterable
`List(1, 2) must have size(2)`
`List(1, 2) must have length(2)` // equivalent to size

* to check if an `Iterable[String]` contains matching strings
`List("Hello", "World") must containMatch("ll")        // matches with .*ll.*`
`List("Hello", "World") must containPattern(".*llo")   // matches with .*llo`

* to check if an `Iterable[String]` contains matching strings, but only once
`List("Hello", "World") must containMatch("ll").onlyOnce`

* to check if one of the elements has a given property
`List("Hello", "World") must have(_.size >= 5)`

* to check if an iterable has the same elements as another one, regardless of the order
`List("Hello", "World") must haveTheSameElementsAs(List("World", "Hello"))`

* to check if a sequence is sorted (works with any type `T` which has an `Ordering`)
`Seq(1, 2, 3) must beSorted`
"""
}

object MapMatchers extends Card {
  def title = "Map Matchers"
  def text =  """
Maps have their own matchers as well, to check keys and values:

* `haveKey` checks if a Map has a given key
`Map(1 -> "1") must haveKey(1)`

* `haveValue` checks if a Map has a given value
`Map(1 -> "1") must haveValue("1")`

* `havePair` checks if a Map has a given pair of values
`Map(1 -> "1") must havePair(1 -> "1")`

* `havePairs` checks if a Map has some pairs of values
`Map(1->"1", 2->"2", 3->"3") must havePairs(1->"1", 2->"2")`

But Maps are also Partial Functions, so:

* `beDefinedAt` checks if a PartialFunction is defined for a given value
`partial must beDefinedAt(1)`

* `beDefinedBy` checks if a PartialFunction is defined for a given value
and returns another one
`partial` must beDefinedBy(1 -> true)
"""
}

object XmlMatchers extends Card {
  def title = "Xml Matchers"
  def text =  """
It is very useful to have literal Xml in Scala, it is even more useful to have matchers for it!

* `beEqualToIgnoringSpace` compares 2 Nodes, without considering spaces
`<a><b/></a> must ==/(<a> <b/></a>)`
`<a><b/></a> must beEqualToIgnoringSpace(<a> <b/></a>)`

* `beEqualToIgnoringSpace` can also do an ordered comparison
`<a><c/> <b/></a> must ==/(<a> <c/><b/></a>).ordered`

* `\` is an XPath-like matcher matching if a node is a direct child of another
`<a><b/></a> must \("b")`

* You can also check attribute names
`<a><b name="value"></b></a> must \("b", "name")`

* And attribute names and values as well
`<a><b n="v" n2="v2" n3="v3"></b></a> must \("b", "n"->"v", "n2"->"v2")`

* The equivalent of `\` for a "deep" match is simply `\\`
`<a><s><c></c></s></a> must \\("c")`
"""
}

object JsonMatchers extends Card {
  def title = "Json Matchers"
  def text =  """
[Json](www.json.org) is a simple data format essentially modeling recursive key-values. There are 2 matchers which can be used to verify the presence of appropriate values in Strings representing Json documents:

* `/(value)` checks if a value is present at the root of the document. This can only be the case if that document is
an Array

* `/(key -> value)` checks if a pair is present at the root of the document. This can only be the case if that document is
a Map

* `*/(value)` checks if a value is present anywhere in the document, either as an entry in an Array, or as the value
for a key in a Map

* `*/(key -> value)` checks if a pair is present anywhere in a Map of thedocument

Now the interesting part comes from the fact that those matchers can be chained to search specific paths in the Json document. For example, for the following document:

      // taken from an example in the Lift project
      val person = {
      "person": {
      "name": "Joe",
      "age": 35,
      "spouse": {
      "person": {
      "name": "Marilyn",
      "age": 33
      }
      }
      }
      }

You can use these combinations:

person must /("person") */("person") /("age" -> 33.0) // by default numbers are parsed as Doubles
"""
}

object FileMatchers extends Card {
  def title = "File Matchers"
  def text =  """
The Java api for files is more or less mimicked as matchers which can operate on strings denoting paths or on Files:

* `beEqualToIgnoringSep` checks if 2 paths are the same regardless of their separators
`"c:\\temp\\hello" must beEqualToIgnoringSep("c:/temp/hello")`
* `beAnExistingPath` checks if a path exists
* `beAReadablePath` checks if a path is readable
* `beAWritablePath` checks if a path is writable
* `beAnAbsolutePath` checks if a path is absolute
* `beAHiddenPath` checks if a path is hidden
* `beAFilePath` checks if a path is a file
* `beADirectoryPath` checks if a path is a directory
* `havePathName` checks if a path has a given name
* `haveAsAbsolutePath` checks if a path has a given absolute path
* `haveAsCanonicalPath` checks if a path has a given canonical path
* `haveParentPath` checks if a path has a given parent path
* `listPaths` checks if a path has a given list of children
* `exist` checks if a file existsy

* `beReadable` checks if a file is readable
* `beWritable` checks if a file is writable
* `beAbsolute` checks if a file is absolute
* `beHidden` checks if a file is hidden
* `beAFile` checks if a file is a file
* `beADirectory` checks if a file is a directory
* `haveName` checks if a file has a given name
* `haveAbsolutePath` checks if a file has a given absolute path
* `haveCanonicalPath` checks if afile has a given canonical path
* `haveParent` checks if a file has a given parent path
* `haveList` checks if a file has a given list of children
"""
}

object TerminationMatchers extends Card {
  def title = "Termination Matchers"
  def text =  """

Sometimes you just want to specify that a block of code is going to terminate. The `TerminationMatchers` trait is here to help. If you mix in that trait, you can write:

Thread.sleep(100) must terminate

// the default is retries=0, sleep=100.millis
Thread.sleep(100) must terminate(retries=1, sleep=60.millis)

Note that the behaviour of this matcher is a bit different from the `eventually` operator. In this case, we let the current Thread sleep during the given `sleep` time and then we check if the computation is finished, then, we retry for the given number of `retries`.

In a further scenario, we might want to check that triggering another action is able to unblock the first one:

action1 must terminate.when(action2)
action1 must terminate.when("starting the second action", action2)
action1 must terminate(retries=3, sleep=100.millis).when(action2)

When a second action is specified like that, `action1` will be started and `action2` will be started on the first retry. Otherwise, if you want to specify that `action1` can *only* terminate when `action2` is started, you write:

action1 must terminate.onlyWhen(action2)
"""
}

object ScalazMatchers extends Card {
  def title = "Scalaz Matchers"
  def text =  """

It was useful to check some Scalaz properties during the development of ***specs2*** so they are available as matchers:

* `semigroup.isAssociative` checks if a `Semigroup` respect the associativity law
* `monoid.hasNeutralElement` checks if a `Monoid` zero value is really a neutral element
* `monoid.isMonoid` checks if a `Monoid` has a neutral element and respects the associativity rule

However specs2 is not using a publicly available version of Scalaz but an "internal" one. This decision was taken to avoid potential clashes with people using a different version  of Scalaz, including the Scalaz project itself! So, you can have a look at the implementation of those matchers in the specs2 source code, to reuse them for your own specifications.
"""
}

object ResultMatchers extends Card {
  def title = "Result Matchers"
  def text =  """
That's only if you want to match the result of other matchers!

// you need to extend the ResultMatchers trait
class MatchersSpec extends Specification with ResultMatchers { def is =
  "beMatching is using a regexp" ! {
    ("Hello" must beMatching("h.*")) must beSuccessful
  }
}
"""
}

object InterpreterMatchers extends Card {
  def title = "Scala Interpreter Matchers"
  def text =  """
This trait is not included in the default specification so you'll have to add it in the rare case where you want to use the Scala interpreter and execute a script:

class ScalaInterpreterMatchersSpec extends Specification with ScalaInterpreterMatchers {
  def interpret(s: String): String = // you have to provide your own Scala interpreter here

    "A script" can {
      "be interpreted" in {
        "1 + 1" >| "2"
      }
    }
}
"""
}

object ParserMatchers extends Card {
  def title = "Parser Matchers"
  def text =  """
Scala provides a parsing library using [parser combinators](http://www.scala-lang.org/api/current/scala/util/parsing/combinator/Parsers.html).

You can specify your own parsers by:

* extending the `ParserMatchers` trait
* defining the `val parsers` variable with your parsers definition
* using the `beASuccess`, `beAFailure`, `succeedOn`, `failOn`, `errorOn` matchers to specify the results of parsing input
strings. `beAPartialSuccess`, `be aPartialSuccess`, `succeedOn.partially` will allow a successful match only on part of the input
* using `haveSuccessResult` and `haveFailureMsg` to specify what happens *only* on success or failure. Those matchers accept
a String or a matcher so that
. `haveSuccessResult("r") <==> haveSuccessResult(beMatching(".*r.*") ^^ ((_:Any).toString)`
. `haveFailingMsg("m") <==> haveFailingMsg(beMatching(".*r.*"))`

For example, specifying a Parser for numbers could look like this:

import util.parsing.combinator.RegexParsers
import NumberParsers.{number, error}

class ParserSpec extends Specification with matcher.ParserMatchers {  def is =
  "Parsers for numbers"                                                                   ^
    p^
    "beASuccess and succeedOn check if the parse succeeds"                                  ^
    { number("1") must beASuccess }                                                         ^
    { number("1i") must beAPartialSuccess }                                                 ^
    { number must succeedOn("12") }                                                         ^
    { number must succeedOn("12ab").partially }                                             ^
    { number must succeedOn("12").withResult(12) }                                          ^
    { number must succeedOn("12").withResult(equalTo(12)) }                                 ^
    { number("1") must haveSuccessResult("1") }                                             ^
    p^
    "beAFailure and failOn check if the parse fails"                                        ^
    { number must failOn("abc") }                                                           ^
    { number must failOn("abc").withMsg("string matching regex.*expected") }                ^
    { number must failOn("abc").withMsg(matching(".*string matching regex.*expected.*")) }  ^
    { number("i") must beAFailure }                                                         ^
    { number("i") must haveFailureMsg("i' found") }                                         ^
    p^
    "beAnError and errorOn check if the parser errors out completely"                       ^
    { error must errorOn("") }                                                              ^
    { error("") must beAnError }                                                            ^
    end

  val parsers = NumberParsers
}
object NumberParsers extends RegexParsers {
  /** parse a number with any number of digits */
  val number: Parser[Int] = "\\d+".r ^^ {_.toInt}
  /** this parser returns an error */
  val error: Parser[String] = err("Error")
}
"""
}

object DependencyMatchers extends Card {
  def title = "Dependency Matchers"
  def text =  """
It is highly desirable to have acyclic dependencies between the packages of a project. This often leads to describing the packages structure as "layered": each package on a layer can only depend on a package on a lower layer. ***specs2*** helps you enforce this design property with specific matchers.

##### Layers definition

First you need to define the packages and their expected dependencies. Mix-in the `org.specs2.specification.Analysis` trait and define, (taking ***specs2*** as an example):

layers (
"runner",
"reporter",
"specification mutable",
"mock      form",
"matcher",
"execute",
"reflect    xml  time html",
"collection control io text main data").withPrefix("org.specs2")

The above expression defines layers as an ordered list of `String`s containing space-separated package names. It is supplemented by a `withPrefix` declaration to factor out the common package prefix between all these packages.

By default, the packages are supposed to correspond to directories in the `src/target/scala-<version>/classes` directory. If your project has a different layout you can declare another target directory:

layers(...).inTargetDir("out/classes")

###### Inclusion/Exclusion

Every rule has exceptions :-). In some rare cases, it might be desirable to exclude a class from being checked on a given layer. To do this, you can use the `include/exclude` methods on the `Layer` class:

layers (
"runner",
"reporter",
"specification mutable".exclude("mutable.SpecificationWithJUnit"),
"mock      form",
"matcher",
"execute",
"reflect  xml  time html",
"collection control io text main data").withPrefix("org.specs2")

The `include/exclude` methods accept a list of regular expressions to:

- exclude fully qualified class names (generally, only `exclude` will be necessary)
- re-include fully qualified class names if the exclusion list is to big

##### Verification

Now you've defined layers, you can use the `beRespected` matcher to check if all the dependencies are verified:

val design = layers(...)
design must beRespected

This works by invoking the Scala `BuildManager` which will recompile the files contained in the source directory (this operation is potentially time-consuming). Once the the `BuildManager` knows all the dependencies between classes, if some of them contradict what you specified, you'll get a failure message showing all the broken dependencies:

those dependencies are not satisfied:
org.specs2.main x-> org.specs2.io because org.specs2.io.FileSystem -> org.specs2.main.Arguments
org.specs2.main x-> org.specs2.io because org.specs2.io.FileSystem -> org.specs2.main.ArgumentsArgs

Note: this functionality relies on the scala compiler library, so you need to add it to your build file:

// use sbt's scalaVersion Setting to define the scala-compiler library version
libraryDependencies <<= scalaVersion { scala_version => Seq(
"org.specs2" %% "specs2" % "1.7" % "test",
"org.scala-lang" % "scala-compiler" % scala_version % "test")
}


##### Layers as an `Example`

The `Analysis` trait allows to directly embed the layers definition in a `Specification` and turn it into an `Example`:

class DependenciesSpec extends Specification with Analysis { def is =
  "this is the application design" ^
    layers(
      "gui commandline",
      "controller",
      "backend"
    )
}

##### Alternate implementation

Another implementation of the same functionality is available through the `org.specs2.analysis.CompilerDependencyFinder` trait. This implementation uses the compiler dependency analysis functionality but needs more time, since it recompiles the sources.

The source files are taken from the `src/main/scala` directory by default but you can change this value by using the `Layers.inSourceDir` method.

While this implementation is slower than the Classycle one, it might retrieve more dependencies, for example when constants are inlined in class files.
"""
}
