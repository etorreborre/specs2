package org.specs2
package mock

import org.hamcrest.{ Description, TypeSafeMatcher }
import matcher._

/** Adapter class to use specs2 matchers as Hamcrest matchers */
case class HamcrestMatcherAdapter[T](m: Matcher[T]) extends TypeSafeMatcher[T] {
   var message = ""
     
   def matchesSafely(item: T) = {
     m.apply(Expectable(item)) match {
       case MatchSuccess(m, _, _) => message = m; true
       case _ => false
     }
     
   }
   def describeTo(description: Description) = {
     description.appendText(message)
   }
}

