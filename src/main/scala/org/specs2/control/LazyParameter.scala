package org.specs2
package control

/**
 * This trait can be used to allow some function to be called with varargs, with
 * values being evaluated lazily:<code>
 * 
 * def method[T](values: LazyParameter[T]*) = {
 *  values.toStream // use the toStream method to consume the values lazily
 * }
 * // usage  
 * method(exp1, exp2, exp3)  
 * </code>
 * Note that the values are really evaluated once, unlike a by-name parameter.
 * @see org.specs.util.lazyParamSpec
 */ 
trait LazyParameters {
  /** transform a value to a zero-arg function returning that value */
  implicit def toLazyParameter[T](value: =>T) = new LazyParameter(() => value)
}
object LazyParameters extends LazyParameters

/** class holding a value to be evaluated lazily */
class LazyParameter[T](private val v: () => T) {
  private lazy val evaluated = v.apply()
  /**
   * @return the evaluated value. This method is private to specs2 to avoid the 
   *         implicit to leak to client specifications using the 'value' method 
   */
  private[specs2] def value = evaluated
  
  override def toString = value.toString
  override def equals(o: Any) = value == o
  override def hashCode = value.hashCode
}

