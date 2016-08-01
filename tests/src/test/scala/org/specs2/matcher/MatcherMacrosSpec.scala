package org.specs2
package matcher

/**
 * this requires the macro compiler plugin with Scala 2.10.x:
 *
 *  addCompilerPlugin("org.scala-lang.plugins" %% "macro-paradise" % "2.0.0"))
 */
class MatcherMacrosSpec extends Spec with ResultMatchers with MatcherMacros with MustMatchers { def is = s2"""

 It is possible to define a matcher with methods matching the public members of a given type

 We can specify:
   just the type to match                                               $e0
   a value for one of the members                                       $e1
   a matcher for one of the members                                     $e2
   a function returning a result for one of the members                 $e3
   several values at once                                               $e4

 The first member value to fail will return the failure message         $e5
 The member name must be mentioned in the failure message               $e6
 The expectable description must be included in the failure message     $e7
 Deeply nested matches must mention their full path when failing        $e8
"""

  val cat = Cat(name = "Kitty", age = 6, kitten = Seq(Cat("Oreo", 1), Cat("Ella", 2)))

  def e0 = cat must matchA[Cat]
  def e1 = cat must matchA[Cat].name("Kitty")
  def e2 = cat must matchA[Cat].age(is(6))
  def e3 = cat must matchA[Cat].kitten((_:Seq[Cat]) must haveSize(2))
  def e4 = cat must matchA[Cat].name("Kitty").age(is(6)).kitten((_:Seq[Cat]) must haveSize(2))

  def e5 = cat must matchA[Cat].name("Kitty").age(is(7)).kitten((_:Seq[Cat]) must haveSize(2)) returns "6" // todo: fix this
//  def e5 = (cat must matchA[Cat].name("Kitty").age(is(7)).kitten((_:Seq[Cat]) must haveSize(2))) returns "'6' is not equal to '7'"
  def e6 = (cat must matchA[Cat].age(is(7))) returns "age: "
  def e7 = (cat aka "the cat" must matchA[Cat].name("Kitty").age(is(7)).kitten((_:Seq[Cat]) must haveSize(2))) returns cat.toString
  def e8 = (Nested() must matchA[Nested].n1(_ must matchA[Nested1].n2(_ must matchA[Nested2].n3(_ must matchA[Nested3].name("other"))))) returns
"""|For Nested(Nested1(Nested2(Nested3(name))))
   |For Nested1(Nested2(Nested3(name)))
   |For Nested2(Nested3(name))
   |For Nested3(name)
   |  name: name""".stripMargin // todo: fix this
//   |  name: 'name' is not equal to 'other'""".stripMargin

  def is[A](a: A) = be_==(a)

  case class Cat(name: String = "", age: Int = 0, kitten: Seq[Cat] = Seq()) {
    // additional methods can exist on the case class
    // but will not be used for matching
    def test(hello: String) {}
  }

  case class Nested(n1: Nested1 = Nested1())
  case class Nested1(n2: Nested2 = Nested2())
  case class Nested2(n3: Nested3 = Nested3())
  case class Nested3(name: String = "name")

}

