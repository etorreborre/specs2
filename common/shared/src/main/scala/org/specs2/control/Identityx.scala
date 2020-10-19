package org.specs2
package control

import org.specs2.fp._

/**
 * This class provides the possibility to execute a function on an object if a condition is true
 * If not, the object is returned
 */
extension [T](t: =>T)
  def when(condition: Boolean)(f: T => T) =
    if condition then f(t) else t

  def unless(condition: Boolean)(using m: Monoid[T]) =
    if condition then t else m.zero
