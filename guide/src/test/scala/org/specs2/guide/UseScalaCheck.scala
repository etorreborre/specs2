package org.specs2
package guide

import org.scalacheck.{Prop, Gen, Arbitrary}
import org.specs2.scalacheck.Parameters
import scalaz._, Scalaz._

object UseScalaCheck extends UserGuidePage with ScalaCheck { def is = "ScalaCheck".title ^ s2"""

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

By default the properties created with `prop` will be shrinking counter-examples. But as you will see below there lots of different ways to parameterize ScalaCheck properties in specs2, including if shrinking must be done.

### Arbitrary instances

ScalaCheck requires an implicit `Arbitrary[T]` instance for each parameter of type `T` used in a property. If you rather want to pick up a specific `Arbitrary[T]` for a given property argument you can modify the `prop` with  another `Arbitrary` instance: ${snippet{
s2"""
  a simple property       $ex1
  a more complex property $ex2
"""
  val abStringGen = (Gen.oneOf("a", "b") |@| Gen.oneOf("a", "b"))(_+_)

  implicit def abStrings: Arbitrary =
    Arbitrary(abStringGen)

  def ex1 = prop((s: String) => s must contain("a") or contain("b")).setArbitrary(abStrings)

  // use the setArbitrary<n> method for the nth argument
  def ex2 = prop((s1: String, s2: String) => (s1+s2) must contain("a") or contain("b")).
              setArbitrary1(abStrings).setArbitrary2(abStrings)
}}

### With Generators

What you can do with arbitraries can also be done with generators
ScalaCheck also allows to create `Prop`s directly with the `Prop.forAll` method accepting `Gen` instances: ${snippet{
s2"""
  a simple property       $ex1
  a more complex property $ex2
"""
def abStrings = for { a <- Gen.oneOf("a", "b"); b <- Gen.oneOf("a", "b") } yield a+b

def ex1 = Prop.forAll(abStrings) { s: String =>
  s must contain("a") or contain("b")
}

def ex2 = Prop.forAll(abStrings, abStrings) { (s1: String, s2: String) =>
  (s1+s2) must contain("a") or contain("b")
}
}}

### Test properties

ScalaCheck test generation can be tuned with a few properties. If you want to change the default settings, you have to use implicit values: ${snippet{
  implicit val params = Parameters(minTestsOk = 20) // add "verbose = true" to get additional console printing
}}

It is also possible to specifically set the execution parameters on a given property: ${snippet{
class ScalaCheckSpec extends mutable.Specification with ScalaCheck {
  "this is a specific property" >> prop { (a: Int, b: Int) =>
    (a + b) must_== (b + a)
  }.set(minTestsOk = 200, workers = 3) // use "display" instead of "set" for additional console printing
}
}}

The parameters you can modify are:

 Parameter         | Description
 ----------------- | ------------
 `minTestsOk`      | minimum of tests which must be ok before the property is ok (default = 100)
 `maxDiscardRatio` | if the data generation discards too many values, then the property can't be proven (default = 5)
 `minSize`         | minimum size for the "sized" data generators, like list generators (default = 0)
 `maxSize`         | maximum size for the "sized" data generators (default = 100)
 `workers`         | number of threads checking the property (default = 1)
 `rng`             | the random number generator (default = `new java.util.Random`)
 `callback`        | a ScalaCheck TestCallback (see the [ScalaCheck documentation](http://www.scalacheck.org))
 `loader`          | a custom classloader (see the [ScalaCheck documentation](http://www.scalacheck.org))

You can also set the random generator that is used in all the ScalaCheck generators: ${snippet{
case class MyRandomGenerator() extends java.util.Random {
  // implement a deterministic generator for example
}

"this is a specific property" ! prop { (a: Int, b: Int) =>
  (a + b) must_== (b + a)
}.set(rng = MyRandomGenerator(), minTestsOk = 200, workers = 3)
}}

#### Expectations

By default, a successful example using a `Prop` will be reported as 1 success and 100 (or `minTestsOk`) expectations. If you don't want the number of expectations to appear in the specification statistics just mix-in your specification the `org.specs2.scalacheck.OneExpectationPerProp` trait.

"""
}
