package org.specs2
package form
import io._
import specification._

class PropertySpec extends SpecificationWithJUnit {
  val content =
                                                                                          """
  A Property is used to store values which can be executed lazily.
  It has an Option-like structure, supporting the same kind of operations and 
  can be empty like an Option
                                                                                          """                                                                  ^
"  A property"                                                                            ^
"    can be created from any value"                                                       ! creation1^
"    can be empty "                                                                       ! creation2^
"    can be updated with another value"                                                   ! creation3^
"    can be updated with an option"                                                       ! creation4^
"    has a toString method returning the option value toString"                           ! creation5^
                                                                                          p^
"  A property can be executed"                                                            ^
"    and return a value"                                                                  ! exec().e1^
"    it is only executed once"                                                            ! exec().e2^
                                                                                          p^
"  A property behaves like an Option"                                                     ^
"    with map"                                                                            ! option().e1^
"    with flatMap"                                                                        ! option().e2^
"    with filter"                                                                         ! option().e3^
"    with foreach"                                                                        ! option().e4^
"    with getOrElse"                                                                      ! option().e5^
"    with isDefined"                                                                      ! option().e6^
"    with isEmpty"                                                                        ! option().e7^
"    with orElse"                                                                         ! option().e8^
"    with toLeft"                                                                         ! option().e9^
"    with toRight"                                                                        ! option().e10^
                                                                                          end

  def creation1 = Property(1).get must_== 1
  def creation2 = Property().isEmpty must beTrue
  def creation3 = Property(1).update(2).optionalValue must_== Some(2)
  def creation4 = Property(1).updateValue(Some(2)).optionalValue must_== Some(2)
  def creation5 = Property(1).toString must_== "Some(1)"

  case class exec() extends Before with MockOutput { 
    def before = clear()
    def e1 = Property(1).execute.get must_== 1
    def e2 = {  
      val p = Property({print("one"); 1}).execute
      p.get
      messages.size must_== 1
    }
  }

  case class option() extends Before with MockOutput {
    def before = clear()
    val p = Property(1)
    def e1 = p.map(_.toString).get must_== "1"
    def e2 = p.flatMap(i => Some(i.toString)).get must_== "1"
    def e3 = p.filter(_ >= 0).get must_== 1
    def e4 = { p.foreach(i => print("1")); messages.size must_== 1 }
    def e5 = p.getOrElse(0) must_== 1
    def e6 = p.isDefined must beTrue
    def e7 = p.isEmpty must beFalse
    def e8 = p.orElse(Property(2)) must_== Property(1)
    def e9 = p.toLeft(2) must_== Left(1)
    def e10 = p.toRight(2) must_== Right(1)
  }
}