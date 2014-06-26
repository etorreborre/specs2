package org.specs2
package guide
package matchers

import form.Card
import org.specs2.matcher.MatcherMacros

object CaseClassMatchers extends Card with MatcherMacros {
  def title = "Case class"

  def text = s2"""
There is a special support for matching case classes, using a matcher macro. To use it you need to add the `specs2-matcher-extra` jar to your project and add the `org.specs2.matcher.MatcherMacros` trait to your specification. Then, with the `matchA` matcher you can check the values of case class attributes:${snippet{
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
"""
}
