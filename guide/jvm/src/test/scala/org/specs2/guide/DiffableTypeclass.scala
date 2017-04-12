package org.specs2.guide

import org.specs2.execute.Snippet
import org.specs2.matcher._, describe._

object DiffableTypeclass extends UserGuidePage { def is = s2"""
#### Better failures messages

The matchers based on typed equality like `be_===[T]` all take an implicit typeclass instance from the `Diffable` typeclass:
```
trait Diffable[-T] {

  def diff(actual: T, expected: T): ComparisonResult

}

trait ComparisonResult {
  def identical: Boolean
  def render: String
  def render(indent: String): String = render
}
```

The `Diffable` typeclass produces a `ComparisonResult` between 2 instances so that:
```
Diffable[T].diff(t1, t2).identical <==> t1 == t2
```

The `ComparisonResult.render` method is then used to produce a user-readable difference between 2 objects of the same type. This
typeclass is implemented for the most common types:

**Primitives**
${snippet {
(1 ==== 2).message
}.eval}

**Lists**
${snippet {
(List(1, 2, 3) ==== List(1, 5, 3)).message
}.eval}
${snippet {
(List(1, 2, 3) ==== List(1, 2, 3, 4, 5)).message
}.eval}

**Maps**
${snippet {
  (Map(1 -> "one", 2 -> "two") ==== Map(2 -> "Two", 3 -> "three")).message
}.eval}

We can also compare case classes with by adding the `specs2-shapeless` dependency and importing a special `Diffable`
instance for case classes with `CaseClassDiffs`:
```
case class Address(number: Int, street: String)
case class Person(age: Int, name: String, address: Address)
```
${snippet {
val p1 = Person(44, "me", Address(14, "Best avenue"))
val p2 = Person(27, "you", Address(14, "First street"))

import org.specs2.matcher.CaseClassDiffs._

(p1 ==== p2).message
}.eval}

#### Other instances

Since `Diffable` is a typeclass you are free to provide other instances in scope to display failures differently:
${snippet{
val p1 = Person(44, "me", Address(14, "Best avenue"))
val p2 = Person(27, "you", Address(14, "First street"))

import org.specs2.matcher.CaseClassDiffs._

// display string differences using the edit distance
implicit def stringDiffable: Diffable[String] = new Diffable[String] {

  def diff(s1: String, s2: String): ComparisonResult = {
    if (s1 == s2) PrimitiveIdentical(s1)
    else
      new ComparisonResult {
        def identical = false

        def render = {
          val (s1Diff, s2Diff) = org.specs2.text.StringEditDistance.showDistance(s1, s2)
          s"$s1Diff != $s2Diff"
        }
      }
  }
}

(p1 ==== p2).message
}.eval}

You can also reuse the `identical` method on `ComparisonResult` to provide a different notion of equality:${snippet{

import org.specs2.matcher.describe.Diffables._

implicit def approximateInt: Diffable[Int] =
  PrimitiveDiffable.primitive[Int].
    compareWith((i1: Int, i2: Int) => math.abs(i1 - i2) <= 5)

(1 ==== 3).message
}.eval}



"""

  override implicit def defaultSnippetParameters[T] = Snippet.defaultParams[T].copy(prompt = identity)

  case class Address(number: Int, street: String)
  case class Person(age: Int, name: String, address: Address)

}
