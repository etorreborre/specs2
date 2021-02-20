package org.specs2
package text

import control.Exceptions._

/**
 * This typeclass is used to describe any instance which can be decoded from a String.
 *
 * It can be used to pass string properties in SystemProperties and decode them in a specific way
 */
trait FromString[T]:
  def fromString(s: String): Option[T]

object FromString:

  def apply[T](using fs: FromString[T]): FromString[T] = fs

  given FromString[String] with
    def fromString(s: String): Option[String] = Option(s)

  given FromString[Int] with
    def fromString(s: String): Option[Int] = tryo(s.toInt)

  given FromString[Boolean] with
    def fromString(s: String): Option[Boolean] =
      if "false".equals(s) then Some(false)
      else if "true".equals(s) then Some(true)
      else None
