package org.specs2
package control

import Exceptions._

/**
 * This class represents values which are evaluated lazily and which may even be missing.
 * 
 * It has Option-like function and can be also converted to an Either object
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
  /** update the value */
  def update(newValue: =>T) = withValue(newValue)
  /** alial for update */
  def apply(newValue: =>T) = update(newValue)
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
  def map[U](f: T => U): Property[U] = new Property(() => optionalValue.map(f))
  /** option-like orElse */
  def orElse[U >: T](other: => Property[U]): Property[U] = new Property(() => optionalValue.orElse(other.optionalValue))
  /** option-like toLeft */
  def toLeft[R](right: R): Either[T, R] = optionalValue.toLeft(right)
  /** option-like toRight */
  def toRight[L](left: L): Either[L, T] = optionalValue.toRight(left)
  /** to a list */
  def toList = optionalValue.toList
  
  /** @return execute the property */
  private def execute: Property[T] =
    if (!evaluated) copy(value, evaluated = true, evaluatedValue = value())
    else            this

  override def equals(other: Any) =
    tryCollect(other) { case o: Property[_] => o.optionalValue == optionalValue }

  override def hashCode =
    tryOr(optionalValue.hashCode)((_:Throwable).hashCode)

  override def toString = optionalValue.fold("")(_.toString)
}

/**
 * Companion object to create properties with possibly no initial value
 */
object Property {
  def apply[T](i: =>T) = new Property(() => Some(i))
  def apply[T]() = new Property[T](() => None)
}

trait Properties {
  implicit def aProperty[T](t: T) = Property(t)
}

object Properties extends Properties
