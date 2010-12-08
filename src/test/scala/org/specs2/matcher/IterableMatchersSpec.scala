package org.specs2
package matcher
import java.util.Arrays._
import scala.collection.JavaConversions.{ asScalaIterable }

class IterableMatchersSpec extends SpecificationWithJUnit { def is = 
  
  "we can check if one or several elements are present in an iterable"                    ^
    { List(1, 2) must contain(1) }                                                        ^
    { List(1, 2, 3) must contain(3, 2) }                                                  ^
    { (List(1, 2) must contain(0)) returns "'List(1, 2)' doesn't contain '0'" }           ^
                                                                                          endp^
  "we can check if an iterable contains other elements in the same order"                 ^
    { List(1, 2, 3, 4) must contain(2, 4).inOrder }                                       ^
    "and fails when one element is missing"                                               ! order().fail1^ 
    "or if the order is wrong"                                                            ! order().fail2^
                                                                                          endp^
  "we can check the size of an iterable"                                                  ^
    { Nil must beEmpty }                                                                  ^
    { Nil must be empty }                                                                 ^
    { List(1, 2) must haveSize(2) }                                                       ^
    { List(1, 2) must have size(2) }                                                      ^
                                                                                          endp^
  "we can check if an iterable contains elements following a given pattern"               ^
    { List("Hello", "World") must containMatch("ll") }                                    ^
    { List("Hello", "World") must containPattern(".*llo") }                               ^
                                                                                          endp^
  "we can check if an iterable contains elements with a pattern only once"                ^
    { List("Hello", "World") must containMatch("ll").onlyOnce }                           ^
    { List("Hello", "World") must containPattern(".*llo").onlyOnce }                      ^
    "and fails when there's no match"                                                     ! patternMatch().fail1^ 
    "or if there's more than one"                                                         ! patternMatch().fail2^
                                                                                          endp^
  "we can check if an iterable contains an element with a given property"                 ^
    { List("Hello", "World") must have(_.size >= 5) }                                     ^
                                                                                          endp^
  "we can check if an iterable has the same elements as another one"                      ^
    { List("Hello", "World") must haveTheSameElementsAs(List("Hello", "World")) }         ^
    "regardless of the order"                                                             ^
    { List("Hello", "World") must haveTheSameElementsAs(List("World", "Hello")) }         ^
    "recursively"                                                                         ! sameElems().e1 ^
                                                                                          endp^
  "we can check if an iterable has the same elements in the same order"                   ^
    { List("Hello", "World") must beTheSameSeqAs(List("Hello", "World")) }                ^
    "and show appropriate failure messages if one element doesn't match"                  ! sameSeq().e1 ^
                                                                                          endp^
  "Java collections can also be used with Iterable matchers"                              ^bt^
  "But generally require explicit conversion"                                             ^
    { asList("Hello", "World") must haveSize(2) }                                         ^
    { asList("Hello", "World").toList must containMatch("ll") }                           ^
                                                                                          end
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
    def e1 = (List("Hello", "World") must beTheSameSeqAs(List("Hello2", "World2"))) returns
             "'Hello' is not equal to 'Hello2'; 'World' is not equal to 'World2'"
  } 
}                                                                                          