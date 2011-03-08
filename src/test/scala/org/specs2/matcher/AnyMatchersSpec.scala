package org.specs2
package matcher
import java.io._

class AnyMatchersSpec extends SpecificationWithJUnit { def is =

  "be_== checks the equality of 2 objects"                                                                              ^
  { "a" must_== "a" }                                                                                                   ^
  { "a" must_!= "b" }                                                                                                   ^
  { "a" should_== "a" }                                                                                                 ^
  { "a" should_!= "b" }                                                                                                 ^
  { "a" must be_==("a") }                                                                                               ^
  { "a" must be_!=("b") }                                                                                               ^
  { "a" === "a" }                                                                                                       ^
  { "a" !== "b" }                                                                                                       ^
                                                                                                                        p^
  "beTheSameAs checks if a value is eq to another one"                                                                  ^
  { aValue must beTheSameAs(aValue) }                                                                                   ^
  { "a" must not beTheSameAs("b") }                                                                                     ^
                                                                                                                        p^
  "be is an alias for beTheSameAs"                                                                                      ^
  { aValue must be(aValue) }                                                                                            ^
  { "a" must not be("b") }                                                                                              ^
                                                                                                                        p^
  "beTrue matches true values"                                                                                          ^
  { true must beTrue }                                                                                                  ^
  { (false must beTrue).message must_==  "the value is false" }                                                         ^
                                                                                                                        p^
  "beFalse matches false values"                                                                                        ^
  { false must beFalse }                                                                                                ^
  { (true must beFalse).message must_== "the value is true" }                                                           ^
                                                                                                                        p^
  "beLike matches objects against a pattern"                                                                            ^
  { List(1, 2) must beLike { case List(a, b) => ok } }                                                                  ^
  { List(1, 2) must beLike { case List(a, b) => (a + b) must_== 3 } }                                                   ^
  "if the match succeeds but the condition after match fails, a precise failure message can be returned"                ! e1^
                                                                                                                        p^
  "toSeq allows to transform a single matcher to a matcher checking a Seq"                                              ^
  { List(1, 2, 3) must ((be_==(_:Int)).toSeq)(Seq(1, 2, 3)) }                                                           ^
                                                                                                                        p^
  "toSet allows to transform a single matcher to a matcher checking a Set"                                              ^
  { Set(1, 2, 3) must ((be_==(_:Int)).toSet)(Set(1, 2, 3)) }                                                            ^
                                                                                                                        p^
 "forall allows to transform a single matcher to a matcher checking that all elements of a Seq are matching"            ^
  { Seq(2, 3, 4) must be_>=(2).forall }                                                                                 ^
  { (Seq(2, 3, 4) must be_<=(2).forall) returns
    "In the sequence '2, 3, 4', the 2nd element is failing: 3 is greater than 2" }                                      ^
                                                                                                                        p^
  "foreach is like forall but will execute all matchers and collect the results"                                        ^
  { Seq(2, 3, 4) must be_>=(2).foreach }                                                                                ^
  { ((_:Int) must be_>=(2)).foreach(Seq(2, 3, 4)) }                                                                     ^
  { (Seq(2, 3, 4) must be_<=(2).foreach) returns "3 is greater than 2; 4 is greater than 2" }                           ^
                                                                                                                        p^
 "atLeastOnce allows to transform a single matcher to a matcher checking that one element of a Seq is matching"         ^
  { Seq(2, 3, 4) must be_>=(2).atLeastOnce }                                                                            ^
  { (Seq(2, 3, 4) must be_<=(1).atLeastOnce) returns "No element of '2, 3, 4' is matching ok" }                         ^
                                                                                                                        p^
  "beNull matches null values"                                                                                          ^
  { (null:String) must beNull }                                                                                         ^
  { (null:String) must be(null) }                                                                                       ^
  { "" must not beNull }                                                                                                ^
  { "" must not be null }                                                                                               ^
                                                                                                                        p^
  "beAsNullAs checks if two values are null at the same time"                                                           ^
  { (null:String) must beAsNullAs(null) }                                                                               ^
  { 1 must not be asNullAs(null) }                                                                                      ^
  { (null:String) must not be asNullAs(1) }                                                                             ^
  { 1 must be asNullAs(1) }                                                                                             ^
                                                                                                                        p^
  "beOneOf matches a value is amongs others"                                                                            ^
  { 1 must beOneOf(1, 2, 3) }                                                                                           ^
  { 4 must not be oneOf(1, 2, 3) }                                                                                      ^
                                                                                                                        p^
  "haveClass checks if a value has a given class as its type"                                                           ^
  { 1 must haveClass[java.lang.Integer] }                                                                               ^
  { 1 must not have klass[String] }                                                                                     ^
                                                                                                                        p^
  "haveSuperclass checks if a value has a given class as one of its ancestors"                                          ^
  { new BufferedInputStream(null) must haveSuperclass[InputStream] }                                                    ^
  { 1 must not have superClass[String] }                                                                                ^
                                                                                                                        p^
  "beAssignableFrom checks if a class is assignable from another"                                                       ^
  { classOf[OutputStream] must beAssignableFrom[FileOutputStream] }                                                     ^
                                                                                                                        end
                                                                                          
  def e1 = (List(1, 2) must beLike { case List(a, b) => (a + b) must_== 2 }) returns 
           "'3' is not equal to '2'"
  val aValue: String = "a value"

}
