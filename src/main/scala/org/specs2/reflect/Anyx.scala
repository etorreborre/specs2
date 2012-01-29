package org.specs2.reflect

/**
 * Utility methods which are useful when the type of the object is not known at compile time
 */
private[specs2]
trait Anyx { outer =>

  /** @return an ExtendedAny object */
  implicit def extendAny(a: Any): ExtendedAny = new ExtendedAny(a)
  class ExtendedAny(a: Any) {
    def isBoolean = outer.isBoolean(a)
  }

  /** @return true if a is a Boolean object */
  def isBoolean(a: Any) = a match {
    case b: Boolean => true;
    case _ => false
  }
}
private[specs2]
object Anyx extends Anyx