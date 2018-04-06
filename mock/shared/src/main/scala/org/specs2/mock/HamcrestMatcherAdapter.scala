package org.specs2
package mock

import org.hamcrest.{BaseMatcher, Description}
import matcher._

/** 
 * Adapter class to use specs2 matchers as Hamcrest matchers
 */
case class HamcrestMatcherAdapter[T](m: Matcher[T]) extends BaseMatcher[T] {
  /** this variable is necessary to store the result of a match */ 
  private var message = ""
    
  def matches(item: Object): Boolean = matchesSafely(item: Any, m.asInstanceOf[Matcher[Any]])

  /** this method used to be called when extending TypeSafeMatcher. However the final `matches` method was bypassing 'null' values */
  def matchesSafely(item: T): Boolean = matchesSafely(item, m)

  private def matchesSafely[A](item: A, matcher: Matcher[A]): Boolean = {
    // special case for by-name arguments
    // in that case we apply the Function0 to get the value
    val i = if (item != null && item.isInstanceOf[Function0[_]]) item.asInstanceOf[Function0[_]].apply().asInstanceOf[A] else item
    try {
      matcher.apply(Expectable(i)) match {
        case f: MatchFailure[_] => message = f.koMessage; false
        case _ => true
      }
    // a class cast exception can happen if we tried: vet.treat(dog); there must be one(vet).treat(bird) (see issue #222)
    } catch {
      case c: ClassCastException => false
      case e: Throwable =>
        // this is a stop-gap solution for #584
        // it seems that Mockito can pass null values in some cases
        if (item == null) false
        else throw e
    }
  }

  def describeTo(description: Description): Unit = {
    description.appendText(message)
    ()
  }
}



