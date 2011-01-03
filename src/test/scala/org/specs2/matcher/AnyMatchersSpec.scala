package org.specs2
package matcher
import java.io._

class AnyMatchersSpec extends SpecificationWithJUnit { def is =
  
  "beTheSameAs checks if a value is eq to another one"                                    ^
  { aValue must beTheSameAs(aValue) }                                                     ^
  { "a" must not beTheSameAs("b") }                                                       ^
                                                                                          p^
  "beTrue matches true values"                                                            ^
  { true must beTrue }                                                                    ^
  { (false must beTrue).message must_==  "the value is false" }                           ^
                                                                                          p^
  "beFalse matches false values"                                                          ^
  { false must beFalse }                                                                  ^
  { (true must beFalse).message must_== "the value is true" }                             ^
                                                                                          p^
  "beLike matches objects against a pattern"                                              ^
  { List(1, 2) must beLike { case List(a, b) => ok } }                                    ^
  { List(1, 2) must beLike { case List(a, b) => (a + b) must_== 3 } }                     ^
  "if the match succeeds but the condition after match fails, \n"                         +
  "a precise failure message can be returned"                                             ! e1^
                                                                                          p^
  "toSeq allows to transform a single matcher to a matcher checking a Seq"                ^
  { List(1, 2, 3) must ((be_==(_:Int)).toSeq)(Seq(1, 2, 3)) }                             ^
                                                                                          p^
  "toSet allows to transform a single matcher to a matcher checking a Set"                ^
  { Set(1, 2, 3) must ((be_==(_:Int)).toSet)(Set(1, 2, 3)) }                              ^ 
                                                                                          p^
  "beNull matches null values"                                                            ^
  { (null:String) must beNull }                                                           ^
  { "" must not beNull }                                                                  ^
                                                                                          p^
  "beAsNullAs checks if two values are null at the same time"                             ^
  { (null:String) must beAsNullAs(null) }                                                 ^
  { 1 must not be asNullAs(null) }                                                        ^
  { (null:String) must not be asNullAs(1) }                                               ^
  { 1 must be asNullAs(1) }                                                               ^
                                                                                          p^
  "beOneOf matches a value is amongs others"                                              ^
  { 1 must beOneOf(1, 2, 3) }                                                             ^
  { 4 must not be oneOf(1, 2, 3) }                                                        ^
                                                                                          p^
  "haveClass checks if a value has a given class as its type"                             ^
  { 1 must haveClass[java.lang.Integer] }                                                 ^
  { 1 must not have klass[String] }                                                       ^
                                                                                          p^
  "haveSuperclass checks if a value has a given class as one of its ancestors"            ^
  { new BufferedInputStream(null) must haveSuperclass[InputStream] }                      ^
  { 1 must not have superClass[String] }                                                  ^
                                                                                          p^
  "beAssignableFrom checks if a class is assignable from another"                         ^
  { classOf[OutputStream] must beAssignableFrom[FileOutputStream] }                       ^
                                                                                          end
                                                                                          
  def e1 = (List(1, 2) must beLike { case List(a, b) => (a + b) must_== 2 }) returns 
           "'3' is not equal to '2'"
  val aValue: String = "a value"
}