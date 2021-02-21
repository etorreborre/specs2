package org.specs2
package matcher

import language.adhocExtensions
import scala.reflect.ClassTag
import matcher.describe.Diffable
import text.Quote.*
import collection.IsEmpty
import execute.*, Result.*

/**
 * This trait provides matchers which are applicable to any type of value
 */
trait AnyMatchers:

  /** matches if a == true */
  def beTrue: Matcher[Boolean] =
    new BeTrueMatcher

  /** matches if a == false */
  def beFalse: Matcher[Boolean] =
    (new BeTrueMatcher).not

  /** matches if a eq b */
  def beTheSameAs[T <: AnyRef](t: =>T): BeTheSameAs[T] =
    new BeTheSameAs(t)

  /** alias for beTheSameAs */
  def be[T <: AnyRef](t: =>T): BeTheSameAs[T] =
    beTheSameAs(t)

  /** matches if a == b */
  def be_==[T](t: =>T): EqualityMatcher[Any] =
    beEqualTo(t)

  /** matches if a != b */
  def be_!=[T](t: =>T): Matcher[Any] =
    be_==(t).not

  /** matches if a == b */
  def be_===[T : Diffable](t: =>T): EqualityMatcher[T] =
    beTypedEqualTo(t)

  /** matches if a == b */
  def ===[T : Diffable](t: =>T): EqualityMatcher[T] =
    be_===(t)

  /** matches if a != b */
  def be_!==[T : Diffable](t: =>T): Matcher[T] =
    be_===(t).not

  /** matches if a != b */
  def !==[T : Diffable](t: =>T): Matcher[T] =
    be_!==(t)

  /** matches if a == b */
  def beEqualTo[T](t: =>T): EqualityMatcher[Any] =
    new BeEqualTo(t)

  /** matches if a == b */
  def equalTo[T](t: =>T): EqualityMatcher[Any] =
    beEqualTo(t)

  /** matches if a == b */
  def beTypedEqualTo[T : Diffable](t: =>T): EqualityMatcher[T] =
    new EqualityMatcher(t)

  /** matches if a == b */
  def typedEqualTo[T](t: =>T): EqualityMatcher[T] =
    beTypedEqualTo(t)

  /** matches if a == b after an implicit conversion */
  def be_==~[T : Diffable, S](s: =>S)(using convert: S => T): Matcher[T] =
    new EqualityMatcher(convert(s)).adapt(identity, identity)

  /** matches if a == b after an implicit conversion */
  def ==~[T : Diffable, S](s: =>S)(using convert: S => T): Matcher[T] =
    be_==~(s)

  /** negate a matcher */
  def not[T](m: Matcher[T]) = m.not

  /** matches if a.isEmpty */
  def beEmpty[T : IsEmpty] =
    new Matcher[T]:
      def apply[S <: T](iterable: Expectable[S]) =
        // we need to pattern match on arrays otherwise we get a reflection exception
        iterable.value.asInstanceOf[Matchable] match
          case a: Array[?] =>
            result(a.isEmpty,
              iterable.description + " is not empty")

          case _ =>
              result(summon[IsEmpty[T]].isEmpty(iterable.value),
              iterable.description + " is not empty")

  /** matches if the value is null */
  def beNull[T] =
    new BeNull[T]

  /** matches if a is null when v is null and a is not null when v is not null */
  def beAsNullAs[T](a: =>T) =
    new Matcher[T]:
      def apply[S <: T](y: Expectable[S]) =
        val x = a
        result(x == null && y.value == null || x != null && y.value != null,
               if x == null then "the actual value " +y.description + " is not null"
               else "the expected value " + q(x) + " is not null but the actual value is null")

  /** matches if t.toSeq.exists(_ == v) */
  def beOneOf[T](t: T*): Matcher[T] =
    BeOneOf(t)

  /** alias for beOneOf */
  def beAnyOf[T](t: T*): Matcher[T] =
    BeOneOf(t)

  /** alias for beOneOf, which can be used with contain matchers */
  def anyOf[T](t: T*): Matcher[T] =
    BeOneOf(t)

  /** matches if the value returns a successful result when applied to a PartialFunction */
  def beLike[T](pattern: PartialFunction[T, Result]): Matcher[T] =
    new Matcher[T]:
      def apply[S <: T](a: Expectable[S]) =
        val r = if pattern.isDefinedAt(a.value) then pattern.apply(a.value) else Failure("", "")
        result(r.isSuccess,
               a.description + " is incorrect: " + r.message)

  /** matches if v.getClass == c */
  def haveClass[T : ClassTag]: Matcher[AnyRef] =
    new Matcher[AnyRef]:
      def apply[S <: AnyRef](x: Expectable[S]) =
        val c = implicitly[ClassTag[T]].runtimeClass
        val xClass = x.value.getClass
        result(xClass == c,
               x.description + " doesn't have class " + q(c.getName) + ". It has class " + q(xClass.getName))

  /** matches if c.isAssignableFrom(v.getClass.getSuperclass) */
  def haveSuperclass[T : ClassTag]: Matcher[AnyRef] =
    new Matcher[AnyRef]:
      def apply[S <: AnyRef](x: Expectable[S]) =
        val c = implicitly[ClassTag[T]].runtimeClass
        val xClass = x.value.getClass
        result(c.isAssignableFrom(xClass.getSuperclass),
               x.description + " doesn't have super class " + q(c.getName) + ". It has super class " + q(xClass.getSuperclass.getName))

  /** matches if x.getClass.getInterfaces.contains(T) */
  def haveInterface[T : ClassTag]: Matcher[AnyRef] =
    new Matcher[AnyRef]:
      def apply[S <: AnyRef](x: Expectable[S]) =
        val c = implicitly[ClassTag[T]].runtimeClass
        val xClass = x.value.getClass
        result(xClass.getInterfaces.contains(c),
               x.description + " doesn't have interface " + q(c.getName) + ". It has interface " + xClass.getInterfaces.mkString(", "))

  /** matches if v.isAssignableFrom(c) */
  def beAssignableFrom[T : ClassTag]: Matcher[Class[?]] =
    new Matcher[Class[?]]:
      def apply[S <: Class[?]](x: Expectable[S]) =
        val c = implicitly[ClassTag[T]].runtimeClass
        result(x.value.isAssignableFrom(c),
               x.description + " is not assignable from " + q(c.getName))

  def beAnInstanceOf[T: ClassTag]: Matcher[AnyRef] =
    new Matcher[AnyRef]:
      def apply[S <: AnyRef](x: Expectable[S]) =
        val c = implicitly[ClassTag[T]].runtimeClass
        val xClass = x.value.getClass
        val xWithClass = x.mapDescription(d => s"'$d: ${xClass.getName}'")
        result(c.isAssignableFrom(xClass),
               xWithClass.description + " is not an instance of " + q(c.getName))

object AnyMatchers extends AnyMatchers

/**
 * Matcher for a boolean value which must be true
 */
class BeTrueMatcher extends Matcher[Boolean]:
  def apply[S <: Boolean](v: Expectable[S]) =
    result(v.value, v.description + " is false")

/**
 * Equality Matcher
 */
class BeEqualTo(t: =>Any) extends EqualityMatcher(t)

/**
 * This matcher always matches any value of type T
 */

case class AlwaysMatcher[T]() extends Matcher[T]:
  def apply[S <: T](e: Expectable[S]): Result =
    result(true, "ko")

/**
 * This matcher never matches any value of type T
 */
case class NeverMatcher[T]() extends Matcher[T]:
  def apply[S <: T](e: Expectable[S]) =
    result(false, "ko")

class BeTheSameAs[T <: AnyRef](t: =>T) extends Matcher[T]:
  def apply[S <: T](a: Expectable[S]) =
    val b = t
    result(a.value eq b, a.description + " is not the same as " + q(b))

class BeNull[T] extends Matcher[T]:
  def apply[S <: T](value: Expectable[S]) =
    result(value.value == null, value.description + " is not null")

case class BeOneOf[T](t: Seq[T]) extends Matcher[T]:
  def apply[S <: T](y: Expectable[S]) =
    val x = t
    result(x.contains(y.value), s"${q(y.description)} is not contained in ${q(x.mkString(", "))}")
