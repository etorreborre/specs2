package org.specs2
package guide
package matchers

import matcher.MatcherMacros

object CaseClassMatchers extends UserGuideCard with MatcherMacros {
  def title = "Case class"

  def text = s2"""
There is a special support for matching case classes:

 1. using shapeless to "project" a case class to another one containing less fields to match
 2. using a matcher macro

Both approaches are not incompatible, you can restrict the number of fields to match the remaining fields with more
 precise criteria.

### Case class projection

You need to add the `specs2-shapeless` module to your project dependencies and add the `org.specs2.shapeless.Projection._`
import to your file.

Then you can "project" a type `A` on a "smaller" type `B`:${snippet{
import org.specs2.shapeless.Projection._
    
case class User(id: Int, name: String, age: Int)
case class ExpectedUser(name: String, age: Int)


val u = User(123, "Martin", 58)

u.projectOn[ExpectedUser] must_== ExpectedUser("Martin", 58)
}}

### Matcher macro

You to add the `specs2-matcher-extra` module to your project dependencies and add the `org.specs2.matcher.MatcherMacros` trait to your specification.

Then, with the `matchA` matcher you can check the values of case class attributes:${snippet{
// case class for a Cat
case class Cat(name: String = "", age: Int = 0, kitten: Seq[Cat] = Seq())
// a given cat
val cat = Cat(name = "Kitty", age = 6, kitten = Seq(Cat("Oreo", 1), Cat("Ella", 2)))

// this cat must be a Cat
cat must matchA[Cat]

// check the value of "name"
cat must matchA[Cat].name("Kitty")

// check the value of "age" using a matcher
def is[A](a: A) = be_==(a)
cat must matchA[Cat].age(is(6))

// check the value of "kitten" using a function returning a Result
cat must matchA[Cat].kitten((_:Seq[Cat]) must haveSize(2))

// matchers can be chained
cat must matchA[Cat]
  .name("Kitty")
  .age(is(6))
  .kitten((_:Seq[Cat]) must haveSize(2))

}}

$AndIfYouWantToKnowMore

 - learn about all available ${"specs2 modules" ~/ Installation}
 - show better failure messages for case classes with the  ${"Diffable typeclass" ~/ DiffableTypeclass}

$vid

"""
}
