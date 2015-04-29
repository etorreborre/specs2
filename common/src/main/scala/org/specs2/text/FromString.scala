package org.specs2
package text

import control.Exceptions._

/**
 * This typeclass is used to describe any instance which can be decoded from a String.
 *
 * It can be used to pass string properties in SystemProperties and decode them in a specific way
 */
trait FromString[T] {
  def fromString(s: String): Option[T]
}

object FromString {

  def apply[T](implicit fs: FromString[T]): FromString[T] = fs

  implicit def StringFromString: FromString[String] = new FromString[String] {
    def fromString(s: String): Option[String] = Option(s)
  }

  implicit def IntFromString: FromString[Int] = new FromString[Int] {
    def fromString(s: String): Option[Int] = tryo(s.toInt)
  }

  implicit def BooleanFromString: FromString[Boolean] = new FromString[Boolean] {
    def fromString(s: String): Option[Boolean] =
      if ("false".equals(s))     Some(false)
      else if ("true".equals(s)) Some(true)
      else                       None
  }
}