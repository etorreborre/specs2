package org.specs2
package mock
import specification._
import org.hamcrest._
import org.hamcrest.core._
import matcher._

/** Adapter class to use specs matchers as Hamcrest matchers */
case class HamcrestMatcherAdapter[T](m: org.specs2.matcher.Matcher[T]) extends org.hamcrest.TypeSafeMatcher[T] {
   var message = ""
   def matchesSafely(item: T) = {
     m.apply(new Expectable(item)) match {
       case MatchSuccess(m, _, _) => message = m; true
       case _ => false
     }
     
   }
   def describeTo(description: Description) = {
     description.appendText(message)
   }
}

