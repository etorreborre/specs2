package org.specs2
package form

/**
 * This class represent properties which can be updated and retrieved using customized getter and setter functions.
 * 
 * The held value is optional: it may not exist yet and it is lazy: it is evaluated only once on the first call to get
 */
case class Property[T](value: () => Option[T], evaluated: Boolean = false, evaluatedValue: Option[T] = None) {
  /** change the value */
  def updateValue(init: =>Option[T]) = new Property(value = () => init)
  /** change the value */
  def withValue(init: =>T) = Property(init)
  /** @return the option(value) */
  def optionalValue: Option[T] = execute.evaluatedValue
  /** alias for optionalValue */
  def toOption: Option[T] = optionalValue
  /** @return a value */
  def get: T = optionalValue.get
  /** alias for get */
  def apply(): T = get
  /** update the value */
  def update(newValue: =>T) = withValue(newValue)
  /** alial for update */
  def apply(newValue: =>T) = update(newValue)
  /** @return the value as an Option */
  def execute = {
    if (!evaluated)
      copy(value, true, evaluatedValue = value())
    else 
      this
  }
  override def toString = optionalValue.toString
  /** @return an iterator containing the value if present */
  def iterator = optionalValue.iterator
  /** return the property with the value being filtered according to a predicate */
  def filter(p: T => Boolean) = new Property(() => value().filter(p))
  /** option-like flatMap */
  def flatMap[U](f: T => Option[U]): Property[U] = new Property(() => optionalValue.flatMap(f))
  /** option-like foreach */
  def foreach(f: T => Unit): Unit = optionalValue.foreach(f)
  /** option-like getOrElse */
  def getOrElse[U >: T](other: U): U = optionalValue.getOrElse(other)
  /** option-like isDefined */
  def isDefined = optionalValue.isDefined
  /** option-like isEmpty */
  def isEmpty = optionalValue.isEmpty
  /** option-like map */
  def map[U](f: T => U): Property[U] = new Property(() => optionalValue.map(f(_)))
  /** option-like orElse */
  def orElse[U >: T](other: => Property[U]): Property[U] = new Property(() => optionalValue.orElse(other.optionalValue))
  /** option-like toLeft */
  def toLeft[R](right: R) = optionalValue.toLeft(right)
  /** option-like toRight */
  def toRight[L](left: L) = optionalValue.toRight(left)
  /** to a list */
  def toList = optionalValue.toList
  
  
  override def equals(other: Any) = {
    other match {
      case o: Property[_] => o.optionalValue == optionalValue
      case _ => false
    }
  }
  override def hashCode = optionalValue.hashCode
}
/**
 * Companion object to create properties with possibly no initial value
 */
object Property {
  def apply[T](i: =>T) = new Property(() => Some(i))
  def apply[T]() = new Property[T](() => None)
}

object Properties extends Properties
trait Properties {
    /**
   * This method is used setup a property value, in order to avoid repeting a string. For example: <pre>
   * The name of the person should be {"john" as personName in checkPersonName}
   * </pre>. 
   */
  implicit def anyToAs[T](a: T) = new AsProperty(a)
  implicit def propertyToValue[T](p: Property[T]):T = p()
  case class AsProperty[T](a: T) { 
    def as(p: Property[T]) = {p() = a; a }
    def apply(p: Property[T]) = {p() = a; a}
    def apply(f: T => Any)= {f(a); a }
    def as(f: T => Any)= {f(a); a }
  }
}
