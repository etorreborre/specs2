package org.specs2
package guide

import java.io.File

import org.scalacheck.util.Pretty
import org.scalacheck._
import scalacheck._
import scalaz._, Scalaz._
import execute.ResultImplicits

object UseScalaCheck extends UserGuidePage with ScalaCheck with ResultImplicits { def is = "ScalaCheck".title ^ s2"""

A clever way of creating expectations in $specs2 is to use the [ScalaCheck](https://github.com/rickynils/scalacheck) library.

To declare ScalaCheck properties you first need to extend the `org.specs2.ScalaCheck` trait. Then you can pass functions returning any kind of `Result` (`Boolean`, `Result`, `MatchResult` or a ScalaCheck `Prop`) to the `prop` method and use the resulting `Prop` as your example body: ${snippet{
s2"addition and multiplication are related ${ prop { (a: Int) => a + a == 2 * a } }"
}}

The function that is checked can either return: ${snippet{
// a Boolean
s2"addition and multiplication are related ${ prop { (a: Int) => a + a == 2 * a } }"

// a MatchResult
s2"addition and multiplication are related ${ prop { (a: Int) => a + a must_== 2 * a } }"

// a Prop
s2"addition and multiplication are related ${ prop { (a: Int) => (a > 0) ==> (a + a must_== 2 * a) } }"
}}

Note that if you pass functions using `MatchResult`s you will get better failure messages than just using boolean expressions.

By default the properties created with `prop` will be shrinking counter-examples. But as you will see below there lots of different ways to parameterize ScalaCheck properties in specs2, including declaring if shrinking must be done.

### Prop and Properties

You can also directly use the property types defined by ScalaCheck: `Prop` and `Properties` (a `Properties` object is a just a collection of named `Prop`s)${snippet{
val p1: Prop = Prop.forAll { (a: Int) => a + a == 2 * a }

s2"addition and multiplication are related $p1"

val p2: Properties = new Properties("addition/multiplication") {
  property("addition1") = Prop.forAll { (a: Int) => a + a == 2 * a }
  property("addition2") = Prop.forAll { (a: Int) => a + a + a == 3 * a }
}

s2"addition and multiplication are related $p2"
}}

When using `Properties` only one example is created. This example will run each included property in turn and label the result with the property name if there is a failure. If, however, you want to create one example per included property you need to use the `properties` method: ${snippet{

val p2: Properties = new Properties("addition/multiplication") {
  property("addition1") = Prop.forAll { (a: Int) => a + a == 2 * a }
  property("addition2") = Prop.forAll { (a: Int) => a + a + a == 3 * a }
}

s2"addition and multiplication are related ${properties(p2)}"
}}

*Note*: in a mutable specification the `properties` block of examples need to be added with `addFragments`:
```
"addition and multiplication are related" >> addFragments(properties(p2))
```

If you don't do that there will be no examples executed at all (the beauty of side-effects!).

### Arbitrary instances

ScalaCheck requires an implicit `Arbitrary[T]` instance for each parameter of type `T` used in a property. If you rather want to pick up a specific `Arbitrary[T]` for a given property argument you can modify the `prop` with to use another `Arbitrary` instance: ${snippet{
s2"""
  a simple property       $ex1
  a more complex property $ex2
"""

def abStringGen = (Gen.oneOf("a", "b") |@| Gen.oneOf("a", "b"))(_+_)

implicit def abStrings: Arbitrary[String] =
  Arbitrary(abStringGen)

def ex1 = prop((s: String) => s must contain("a") or contain("b")).setArbitrary(abStrings)

// use the setArbitrary<n> method for the nth argument
def ex2 = prop((s1: String, s2: String) => (s1+s2) must contain("a") or contain("b")).
            setArbitrary1(abStrings).setArbitrary2(abStrings)
}}

It is also possible to pass a `Gen[T]` instance instead of an `Arbitrary[T]`: ${snippet{
val abStringGen = (Gen.oneOf("a", "b") |@| Gen.oneOf("a", "b"))(_+_)

def ex1 = prop((s: String) => s must contain("a") or contain("b")).setGen(abStringGen)
}}

### With Shrink / Pretty

Specific Shrink and Pretty instances can also be specified at the property level: ${snippet{
val shrinkString: Shrink[String] = ???

// set a specific shrink instance on the second parameter
prop((s1: String, s2: String) => s1.nonEmpty or s2.nonEmpty).setShrink2(shrinkString)

// set a specific pretty instance
prop((s: String) => s must contain("a") or contain("b")).setPretty((s: String) =>
  Pretty((prms: Pretty.Params) => if (prms.verbosity >= 1) s.toUpperCase else s))

// or simply if you don't use the Pretty parameters
prop((s: String) => s must contain("a") or contain("b")).pretty((_: String).toUpperCase)
}}

### Contexts

ScalaCheck properties are sometimes used to test stateful applications rather than pure functions. For example you want to test that a function is writing files somewhere and you would like those files to be deleted after each property execution: ${snippet{
// 8<---
implicit val arbitraryFile: Arbitrary[File] = ???
// 8<---
def createFile(f: File): Unit = ???
def deleteTmpDir(): Unit = ???

prop { f: File =>
  createFile(f)
  f.exists
}.after(deleteTmpDir) // before and beforeAfter can also be used there

}}

You can also "prepare" the property to be tested based on the generated arguments: ${snippet {
  // 8<---
  implicit val arbitraryFile: Arbitrary[File] = ???
  // 8<---

def createFile(directory: File, f: File): Unit = ???
// this method will keep the arguments intact but can
// have a side-effect to prepare the system
def setupDirectoryAndFile = (directory: File, file: File) => (directory, file)

prop { (directory: File, f: File) =>
  createFile(directory, f)
  f.exists
}.prepare(setupDirectoryAndFile)

}}

Note that there is a way to [model stateful systems](https://github.com/rickynils/scalacheck/wiki/User-Guide#stateful-testing) with ScalaCheck which goes beyond the simple setup/teardown testing done here.

### Test properties

#### Default values

ScalaCheck test generation can be tuned with a few properties. If you want to change the default settings, you have to use implicit values: ${snippet{
  implicit val params = Parameters(minTestsOk = 20) // add ".verbose" to get additional console printing
}}

The parameters you can modify are:

 Parameter         | Default                | Description
 ----------------- | ---------------------- | ------------
 `minTestsOk`      | `100`                  | minimum of tests which must be ok before the property is ok
 `maxDiscardRatio` | `5.0f`                 | if the data generation discards too many values, then the property can't be proven
 `minSize`         | `0`                    | minimum size for the "sized" data generators, like list generators
 `maxSize`         | `100`                  | maximum size for the "sized" data generators
 `workers`         | `1`                    | number of threads checking the property
 `rng`             | `new java.util.Random` | the random number generator
 `callback`        |                        | a ScalaCheck TestCallback (see the [ScalaCheck documentation](http://www.scalacheck.org))
 `loader`          |                        | a custom classloader (see the [ScalaCheck documentation](http://www.scalacheck.org))
 `prettyParams`    |                        | a `Pretty.Params` instance to set the verbosity level when displaying `Pretty` instances

#### Property level

It is also possible to specifically set the execution parameters on a given property: ${snippet{
class ScalaCheckSpec extends mutable.Specification with ScalaCheck {
  "this is a specific property" >> prop { (a: Int, b: Int) =>
    (a + b) must_== (b + a)
  }.set(minTestsOk = 200, workers = 3) // use "display" instead of "set" for additional console printing
}
}}

You can also set the random generator that is used in all the ScalaCheck generators: ${snippet{
case class MyRandomGenerator() extends java.util.Random {
  // implement a deterministic generator for example
}

"this is a specific property" ! prop { (a: Int, b: Int) =>
  (a + b) must_== (b + a)
}.set(rng = MyRandomGenerator(), minTestsOk = 200, workers = 3)
}}

#### Command-line

Some properties can be overridden from the command line

 Parameter         | Command line
 ----------------- | ------------
 `minTestsOk`      | `scalacheck.mintestsok`
 `maxDiscardRatio` | `scalacheck.maxdiscardratio`
 `minSize`         | `scalacheck.minsize`
 `maxSize`         | `scalacheck.maxsize`
 `workers`         | `scalacheck.workers`
 `verbose`         | `scalacheck.verbose`

#### Expectations

By default, a successful example using a `Prop` will be reported as 1 success and 100 (or `minTestsOk`) expectations. If you don't want the number of expectations to appear in the specification statistics just mix-in your specification the `org.specs2.scalacheck.OneExpectationPerProp` trait.

### Collect values

It is important to validate that generated values are meaningful. In order to do this you can use `collect` to collect values: ${snippet{
// for a property with just one argument
prop((i: Int) => i % 2 == 0).collect
// for a property with just 2 arguments
// collect the second value only
prop((i: Int, j: Int) => i > 0 && j % 2 == 0).collect2
// collect the second value but map it to something else
prop((i: Int, j: Int) => i > 0 && j % 2 == 0).collectArg2((n: Int) => "the value "+n)
// collect all values and display
prop((i: Int, j: Int) => i > 0 && j % 2 == 0).collectAll.verbose
}}

Note that, by default, nothing will be printed on screen unless you set the reporting to `verbose` by either:

 - changing the default `Parameters`
 - setting `.verbose` at the property level
 - passing `scalacheck.verbose` on the command-line

### Equivalence

The `==>` operator in ScalaCheck helps you specify under which conditions a given property is applicable. However it only works one way, you cannot declare that a property must be true "if and only if" some conditions are respected.

With $specs2 and the `org.specs2.execute.ResultImplicits` trait you can use the `<==>` operator to declare the equivalence of 2 `Results`, whether they are properties or booleans or `MatchResults`. So you can write: ${snippet{
// 8<---
  def isYoung(i: Int): Boolean = true
// 8<---
// replace 55 with whatever you think "old" is...
prop((i: Int) => (i >= 18 && i <= 55) <==> isYoung(i))
}}

"""
}
