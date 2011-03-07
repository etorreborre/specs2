package org.specs2
package matcher
import java.util.Arrays._
import scala.collection.JavaConversions.{ collectionAsScalaIterable }

class IterableMatchersSpec extends SpecificationWithJUnit { def is =

  "we can check if one or several elements are present in an iterable"                                                  ^
    { List(1, 2) must contain(1) }                                                                                      ^
    { List(1, 2, 3) must contain(3, 2) }                                                                                ^
    { (List(1, 2) must contain(0)) returns "'List(1, 2)' doesn't contain '0'" }                                         ^
    "with a subclass"                                                                                                   ! subclass().e1^
                                                                                                                        p^
  "we can check if at least one or several elements are present in an iterable"                                         ^
    { List(1, 2) must containAnyOf(1, 4) }                                                                              ^
                                                                                                                        p^
  "we can check the iterable contains another element exactly once"                                                     ^
    { List(1, 2) must contain(1).exactlyOnce }                                                                          ^
    { List(1, 1) must contain(1).exactlyOnce.not }                                                                      ^
                                                                                                                        p^
  "we can check if an iterable contains other elements in the same order"                                               ^
    { List(1, 2, 3, 4) must contain(2, 4).inOrder }                                                                     ^
    "and fails when one element is missing"                                                                             ! order().fail1^
    "or if the order is wrong"                                                                                          ! order().fail2^
                                                                                                                        p^
  "we can check if an iterable has the same elements in the same order, and no more"                                    ^
    { List("Hello", "World") must contain("Hello", "World").only.inOrder }                                              ^
    { List("Hello", "World") must contain("Hello", "World").inOrder.only }                                              ^
    { (List("Hello", "World") must not contain("World", "Hello")).only.inOrder }                                        ^
    "and show appropriate failure messages if one element doesn't match"                                                ! sameSeq().e1 ^
                                                                                                                        p^
  "we can check the size of an iterable"                                                                                ^
    { Nil must beEmpty }                                                                                                ^
    { Nil must be empty }                                                                                               ^
    { List(1, 2) must haveSize(2) }                                                                                     ^
    { List(1, 2) must have size(2) }                                                                                    ^
                                                                                                                        p^
  "we can check if an iterable contains elements following a given pattern"                                             ^
    { List("Hello", "World") must containMatch("ll") }                                                                  ^
    { List("Hello", "World") must containPattern(".*llo") }                                                             ^
                                                                                                                        p^
  "we can check if an iterable contains elements with a pattern only once"                                              ^
    { List("Hello", "World") must containMatch("ll").onlyOnce }                                                         ^
    { List("Hello", "World") must containPattern(".*llo").onlyOnce }                                                    ^
    "and fails when there's no match"                                                                                   ! patternMatch().fail1^
    "or if there's more than one"                                                                                       ! patternMatch().fail2^
                                                                                                                        p^
  "we can check if an iterable contains an element with a given property"                                               ^
    { List("Hello", "World") must have(_.size >= 5) }                                                                   ^
                                                                                                                        p^
  "we can check if an iterable has the same elements as another one"                                                    ^
    { List("Hello", "World") must haveTheSameElementsAs(List("Hello", "World")) }                                       ^
    "regardless of the order"                                                                                           ^
    { List("Hello", "World") must haveTheSameElementsAs(List("World", "Hello")) }                                       ^
    "recursively"                                                                                                       ! sameElems().e1 ^
                                                                                                                        p^
  "Java collections can also be used with Iterable matchers"                                                            ^bt^
  "But generally require explicit conversion"                                                                           ^
    { asList("Hello", "World") must haveSize(2) }                                                                       ^
    { collectionAsScalaIterable(asList("Hello", "World")) must containMatch("ll") }                                     ^
                                                                                                                        end

  case class subclass() {
    class Food
    case class Pizza() extends Food
    case class Fruit() extends Food
    val diner = List(Pizza(), new Fruit())
    def e1 = diner  must contain(Pizza())
  }
  case class order() {
    def fail1 = (List(1, 2, 3, 4) must contain(2, 5).inOrder) returns 
                "'List(1, 2, 3, 4)' doesn't contain in order '2, 5'"
    def fail2 = (List(1, 2, 3, 4) must contain(4, 2).inOrder) returns  
                 "'List(1, 2, 3, 4)' doesn't contain in order '4, 2'"
  }

  case class patternMatch() {
    def fail1 = (List("Hey", "World") must containMatch("llo").onlyOnce) returns
                 "'List(Hey, World)' doesn't contain match '.*llo.*'"
    def fail2 = (List("Hello", "Bella") must containMatch("ll").onlyOnce) returns
                 "'List(Hello, Bella)' contains match '.*ll.*' 2 times"
  }
  
  case class sameElems() {
    def e1 = List("Hello", List("Dear", "World"), "!") must 
             haveTheSameElementsAs(List("!", "Hello", List("World", "Dear")))
  }

  case class sameSeq() {
    def e1 = (List("Hello", "World") must contain("Hello2", "World2").inOrder.only) returns
             "'List(Hello, World)' doesn't contain in order 'Hello2, World2'"
  } 
}                                                                                          