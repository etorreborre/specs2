package org.specs2
package guide
import form._

object MatcherCards extends Cards {
  def title = "Specification Matchers"
  def cards = Seq(
    EqualityMatchers,
    AnyMatchers,
    OptionEitherMatchers,
    StringMatchers,
    NumericMatchers,
    ExceptionMatchers,
    TraversableMatchers,
    MapMatchers,
    XmlMatchers,
    JsonMatchers,
    FileMatchers,
    ContentMatchers)
}

object OptionalMatcherCards extends Cards {
  def title = "Optional Matchers"
  def cards = Seq(
    ResultMatchers,
    InterpreterMatchers,
    ParserMatchers,
    TerminationMatchers,
    DependencyMatchers)
}

object AnyMatchers extends Card {
  def title = "Any"
  def text =  """
These matchers can be used with `Any` objects:

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
object EqualityMatchers extends Card {
  def title = "Equality"
  def text =  """
The most common type of matcher is ***`beEqualTo`*** to test for equality. There are different ways to use this matcher:

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

For some other types of equality:

 Matcher                    |  Comment
 -------------------------- | --------------------------
 `be_===                   `| same as `be_==` but can be used with some combinators like `^^^` or `toSeq` because the parameter type is kept
 `be_==~                   `| checks if `(a:A) == (b:A)` when there is an implicit conversion from B (the type of b) to A (the type of a)
 `beTheSameAs              `| checks if `a eq b` (`a must be(b)` also works)
 `beTrue, beFalse          `| shortcuts for Boolean equality
 `a ==== b                 `| similar to `a === b` but will not typecheck if `a` and `b` don't have the same type


Note: the `beEqualTo` matcher is using the regular `==` Scala equality. However in the case of `Arrays`, Scala `==` is just using reference equality, `eq`, for `Arrays`. So the `beEqualTo` matcher has been adapted to transform `Arrays` to `Seqs` before checking for equality, so that `Array(1, 2, 3) === Array(1, 2, 3)` (despite the fact that `Array(1, 2, 3) != Array(1, 2, 3)`).
  """
}

object OptionEitherMatchers extends Card {
  def title = "Option/Either"
  def text =  """
  There are several matchers to check Option and Either instances:

 * `beSome` checks if an element is Some(_)
 * `beSome(exp)` checks if an element is Some(exp)
 * `beSome.which(function)` checks if an element is Some(_) and satisfies a function returning a boolean
 * `beSome.like(partial function)` checks if an element is Some(_) and satisfies a partial function returning a `MatchResult`
 * `beNone` checks if an element is None
 * `beAsNoneAs` checks if 2 values are equal to None at the same time
 * `beRight` checks if an element is Right(_)
 * `beRight(exp)` checks if an element is Right(exp)
 * `beRight.like(partial function)` checks if an element is Right(_) and satisfies a partial function returning a `MatchResult`
 * `beLeft` checks if an element is Left(_)
 * `beLeft(exp)` checks if an element is Left(exp)
 * `beLeft.like(partial function)` checks if an element is Left(_) and satisfies a partial function returning a `MatchResult`
  """
}

object StringMatchers extends Card {
  def title = "String"
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
  def title = "Numeric"
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
   `2 must be_>(1)`
   `2 must beGreaterThan(1)`

 * `beCloseTo` checks if 2 Numerics are close to each other
   `1.0 must beCloseTo(1, 0.5)`
   `4 must be ~(5 +/- 2)`

 * `beBetween` checks if a value is between 2 others
   `5 must beBetween(3, 6)`
   `5 must beBetween(3, 6).excludingEnd`
   `5 must beBetween(4, 6).excludingStart`
   `5 must beBetween(4, 6).excludingBounds`
   `// with brackets notation`
   `5 must (`be[`(4, 7)`]`) `
"""
}

object ExceptionMatchers extends Card {
  def title = "Exception"
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

object TraversableMatchers extends Card {
  def title = "Traversable"
  def text =  """
Traversables can be checked with several matchers:

 * to check if a traversable is empty
 `Nil must be empty`
 `List(1, 2, 3) must not be empty`

 * to check if some elements are contained in the traversable
 `List(1, 2, 3) must contain(3, 2)`

 * to check if some elements are contained in the traversable in the same order
 `List(1, 2, 3, 4) must contain(2, 4).inOrder`

 * to check if only some elements are contained in the traversable
 `List(4, 2) must contain(2, 4).only`

 * to check if only some elements are contained in the traversable and in the same order
 `List(2, 4) must contain(2, 4).only.inOrder`

 * to check if a sequence contains another one
 `List(2, 4) must containAllOf(List(4, 2))`
 `List(2, 4) must containAllOf(List(2, 4)).inOrder`

 * to check if 2 sequences are contained in each other (like equality but with no order)
 `List(2, 4, 1) must containTheSameElementsAs(List(1, 4, 2))`

 * to check if a sequence contains any element of another one
 `List(2, 4) must containAnyOf(List(4, 2))`

 * to check the size of an iterable
 `List(1, 2) must have size(2)`
 `List(1, 2) must have length(2)` // equivalent to size

 * to check if a `Traversable[String]` contains matching strings
 `List("Hello", "World") must containMatch("ll")        // matches with .*ll.*`
 `List("Hello", "World") must containPattern(".*llo")   // matches with .*llo`

 * to check if a `Traversable[String]` contains matching strings, but only once
 `List("Hello", "World") must containMatch("ll").onlyOnce`

 * to check if one of the elements has a given property
 `List("Hello", "World") must have(_.size >= 5)`

 * to check if one of the elements matching a partial function is ok
 `List(1, 2, 3, 4) must haveOneElementLike { case i if i > 2 => (i % 2) must_== 0 }`

 * to check if all elements matching a partial function are ok
 `List(1, 2, 3, 4) must haveAllElementsLike { case i if i > 2 => i must be_<(10) }`

 * to check if a traversable has the same elements as another one, regardless of the order, recursively (
 `List("Hello", "World") must haveTheSameElementsAs(List("World", "Hello"))`
 `List("Hello", "World") must haveTheSameElementsAs(List("World", "Hello"), equalArrays)` // with your own equality method

 * to check if a sequence is sorted (works with any type `T` which has an `Ordering`)
 `Seq(1, 2, 3) must beSorted`

***Adapting Traversable matchers***

The `contain` and `haveTheSameElementsAs` matchers can be "adapted" to use a different notion of equality than `==` when checking for the existence of elements in a traversable.

`^^ (f: (T, T) => Boolean)` can be used instead of `==`. For example:

         Seq(1, 2, 3) must contain(4, 3, 2) ^^ ((i: Int, j: Int) => i-j <= 1)

will check if each value in the first list is contained in the second list with possibly an error margin of 1.

`^^ (f: T => Matcher[T])` can be used to compare values with a matcher. For example:

         val equalIgnoreCase = (s: String) => be_==(s.toLowerCase)
         Seq("Eric", "Bob") must contain("bob", "eric") ^^ equalIgnoreCase

`^^^ (f: T => S)` can be used to compare values with a function. For example:

         val usersFromDb = Seq(User(id=1, name="eric"), User(id=2, name="Bob"))
         usersFromDb must contain(User(id=0, name="eric"), User(id=0, name="Bob") ^^^ ((_:User).copy(id=0))

_Note_: the last operator used here is slightly different. It is `^^^` instead of simply `^^` because the same function is used to "adapt" both values at the same time. On the other hand the first 2 operators are more or less using a function taking 2 parameters.

"""
}

object MapMatchers extends Card {
  def title = "Map"
  def text =  """
Maps have their own matchers as well, to check keys and values:

 * `haveKey` checks if a Map has a given key
   `Map(1 -> "1") must haveKey(1)`

 * `haveKeys` checks if a Map has several keys
   `Map(1 -> "1", 2 -> "2") must haveKeys(1, 2)`

 * `haveValue` checks if a Map has a given value
   `Map(1 -> "1") must haveValue("1")`

 * `haveValues` checks if a Map has several values
   `Map(1 -> "1", 2 -> "2") must haveValue("1", "2")`

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
  def title = "Xml"
  def text =  """
It is very useful to have literal Xml in Scala, it is even more useful to have matchers for it!

 * `beEqualToIgnoringSpace` compares 2 Nodes, without considering spaces
 `<a><b/></a> must ==/(<a> <b/></a>)`
 `<a><b/></a> must beEqualToIgnoringSpace(<a> <b/></a>)`

 * `beEqualToIgnoringSpace` can also do an ordered comparison
 <code class="prettyprint"><a><c/> <b/></a> must ==/(<a> <c/><b/></a>).ordered</code>

 * on the other hand `beEqualToIgnoringSpace` will not check attributes order
 <code class="prettyprint"><n a="1" b="2"/> must ==/(<n b="2" a="1"/>)</code>

 * <code class="prettyprint">\</code> is an XPath-like matcher matching if a node is a direct child of another
 <code class="prettyprint"><a><b/></a> must \("b")</code>

 * You can also check attribute names
 <code class="prettyprint"><a><b name="value"></b></a> must \("b", "name")</code>

 * And attribute names and values as well (values are checked using a regular expression, use the <a href="http://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html#quote(java.lang.String)">quote method</a>  if you want an exact match)
 <code class="prettyprint"><a><b n="v" n2="v2" n3="v3"></b></a> must \("b", "n"->"v", "n2"->"v\\d")</code>

 * Or the content of a `Text` node
 <code class="prettyprint"><a>hello</a> must \("a") \> "hello"</code> (alias `textIs`)
 <code class="prettyprint"><a>hello</a> must \("a") \>~ "h.*"</code>  (alias `textMatches`)

 * The equivalent of `\` for a "deep" match is simply <code class="prettyprint">\\</code>
 <code class="prettyprint"><a><s><c></c></s></a> must \\("c")</code>
"""
}

object JsonMatchers extends Card {
  def title = "Json"
  def text =  """
 [Json](http://www.json.org) is a simple data format essentially modeling recursive key-values. There are 2 matchers which can be used to verify the presence of appropriate values in Strings representing Json documents:

 * `/(value)` checks if a value is present at the root of the document. This can only be the case if that document is an Array

 * `/(regex)` checks if a value matching the regex is present at the root of the document. This can only be the case if that document is an Array

 * `/(key -> value)` checks if a pair is present at the root of the document. This can only be the case if that document is a Map

 * `*/(value)` checks if a value is present anywhere in the document, either as an entry in an Array, or as the value for a key in a Map

 * `*/(key -> value)` checks if a pair is present anywhere in a Map of the document

 * `/#(i)` selects the ith element in a 0-based indexed Array or a Map and allow further checks on that element

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

You can as well use regular expressions or String matchers instead of values to verify the presence of keys or elements. For example:

   `person must /("p.*".r) */(".*on".r) /("age" -> "\d+\.\d".r)`
   `person must /("p.*".r) */(".*on".r) /("age" -> startWith("3"))`
   `person must /("p.*".r) */(".*on".r) /("age" -> (be_>(30) ^^ ((_:String).toInt)))`

Finally you can access some records by their index:

   `person must /("person") /#(2) /("person")`

              """
}

object FileMatchers extends Card {
  def title = "File"
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

object ContentMatchers extends Card {
  def title = "Content"
  def text =  """
A few matchers can help us check the contents of files or actually anything containing lines of Strings. We can check that 2 files have the same lines:

 * `(file1, file2) must haveSameLines`
 * `file1 must haveSameLinesAs(file2)`

We can check that the content of one file is contained in another one:

 * `file1 must containLines(file2)`

***LinesContent***

Files are not the only possible source of lines and it is useful to be able to check the content of a `File` with a `Seq[String]`:

 * `file1 must haveSameLinesAs(Seq(line1, line2, line3))`

This is because those 2 types implement the `org.specs2.text.LinesContent` trait, defining:

 * a name for the overall content
 * a method for returning the lines
 * a default method for computing the differences of 2 sequences of lines (in case you need to override this logic)

So if you have a specific type `T` which you can represent as a `Seq[String]`, you can create an implicit `LinesContent` and then you'll be able to use the `ContentMatchers`:

      implicit val linesforMyType: LinesContent[T] = new LinesContent[T] {
        def name(t: T) = "My list of lines"
        def lines(t: T): Seq[String] = ... // your implementation goes here
      }

***Order***

It is possible to relax the constraint by requiring the equality or containment to be true regardless of the order of lines:

 * `(file1, file2) must haveSameLines.unordered`
 * `file1 must haveSameLinesAs(file2).unordered`
 * `file1 must containLines(file2).unordered`

***Missing only***

By default, `(file1, file2) must haveSameLines` will report misplaced lines if any, that is, lines of `f1` which appear in `f2` but not at the right position. However if `file2` is big, this search might degrade the performances. In that case you can turn it off with `missingOnly`:

 `(file1, file2) must haveSameLines.missingOnly`

***Show less differences***

If there are too many differences, you can specify that you only want the first 10:

 * `(file1, file2) must haveSameLines.showOnly(10.differences).unordered`

In the code above `10.differences` builds a `DifferenceFilter` which is merely a filtering function: `(lines1: Seq[String], lines2: Seq[String]) => (Seq[String], Seq[String])`. The parameter `lines1` is the sequence of lines not found in the second content while `lines2` is the sequence of lines not found in the first content.

"""
}

object TerminationMatchers extends Card {
  def title = "Termination"
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

object ResultMatchers extends Card {
  def title = "Result"
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
  def title = "Scala Interpreter"
  def text =  """
In the rare case where you want to use the Scala interpreter and execute a script:

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
  def title = "Parser"
  def text =  """
Scala provides a parsing library using [parser combinators](http://www.scala-lang.org/api/current/scala/util/parsing/combinator/Parsers.html).

You can specify your own parsers by:

 * extending the `ParserMatchers` trait
 * associating the `val parsers` variable with your parsers definition
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
        "Parsers for numbers"                                                                     ^
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

***Layers definition***

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

**Inclusion/Exclusion**

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

***Verification***

Now you've defined layers, you can use the `beRespected` matcher to check if all the dependencies are verified:

     val design = layers(...)
     design must beRespected

If some dependencies are not respected:

      those dependencies are not satisfied:
      org.specs2.main x-> org.specs2.io because org.specs2.io.FileSystem -> org.specs2.main.Arguments
      org.specs2.main x-> org.specs2.io because org.specs2.io.FileSystem -> org.specs2.main.ArgumentsArgs

***Layers as an `Example`***

The `Analysis` trait allows to directly embed the layers definition in a `Specification` and turn it into an `Example`:

      class DependenciesSpec extends Specification with Analysis { def is =
        "this is the application design" ^
          layers(
            "gui commandline",
            "controller",
            "backend"
          )
      }

***Alternative implementation***

Another implementation of the same functionality is available through the `org.specs2.analysis.CompilerDependencyFinder` trait. This implementation uses the compiler dependency analysis functionality but needs more time, since it recompiles the sources.

The source files are taken from the `src/main/scala` directory by default but you can change this value by using the `Layers.inSourceDir` method.

While this implementation is slower than the Classycle one, it might retrieve more dependencies, for example when constants are inlined in class files.

Note: since this functionality relies on the scala compiler library, so you need to add it to your build file:

      // use sbt's scalaVersion Setting to define the scala-compiler library version
      libraryDependencies <<= scalaVersion { scala_version => Seq(
        "org.specs2" %% "specs2" % "1.10" % "test",
        "org.scala-lang" % "scala-compiler" % scala_version % "test")
      }
"""
}
