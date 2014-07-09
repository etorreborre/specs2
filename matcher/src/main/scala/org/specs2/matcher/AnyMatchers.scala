package org.specs2
package matcher

import text.Trim._
import text.Quote._
import text.NotNullStrings._
import execute._
import scala.reflect.ClassTag
import org.specs2.control.DefaultStackTraceFilter

/**
 * This trait provides matchers which are applicable to any type of value
 */
trait AnyMatchers extends AnyBaseMatchers with AnyBeHaveMatchers
object AnyMatchers extends AnyMatchers

private[specs2]
trait AnyBaseMatchers {

  /** matches if a == true */
  def beTrue = new BeTrueMatcher
  /** matches if a == false */
  def beFalse = beTrue.not

  /** matches if a eq b */
  def beTheSameAs[T <: AnyRef](t: =>T) = new BeTheSameAs(t)
  /** alias for beTheSameAs */
  def be[T <: AnyRef](t: =>T) = beTheSameAs(t)

  /** matches if a == b */
  def be_==[T](t: =>T) = beEqualTo(t)
  /** matches if a != b */
  def be_!=[T](t: =>T) = be_==(t).not
  /** matches if a == b */
  def be_===[T](t: =>T) = beTypedEqualTo(t)
  /** matches if a == b */
  def ===[T](t: =>T) = be_===(t)
  /** matches if a != b */
  def be_!==[T](t: =>T) = be_===(t).not
  /** matches if a != b */
  def !==[T](t: =>T) = be_!==(t)
  /** matches if a == b */
  def beEqualTo[T](t: =>T) = new BeEqualTo(t)
  /** matches if a == b */
  def equalTo[T](t: =>T) = beEqualTo(t)
  /** matches if a == b */
  def beTypedEqualTo[T](t: =>T, equality: (T, T) => Boolean = (t1:T, t2:T) => t1 == t2) =
    new BeTypedEqualTo(t, equality)
  /** matches if a == b */
  def typedEqualTo[T](t: =>T, equality: (T, T) => Boolean = (t1:T, t2:T) => t1 == t2) =
    beTypedEqualTo(t, equality)
  /** matches if a == b after an implicit conversion */
  def be_==~[T, S](s: =>S)(implicit convert: S => T): Matcher[T] = new BeTypedEqualTo(convert(s)).
    adapt(identity, identity, identity)
  /** matches if a == b after an implicit conversion */
  def ==~[T, S](s: =>S)(implicit convert: S => T): Matcher[T] = be_==~(s)(convert)

  /** negate a matcher */
  def not[T](m: Matcher[T]) = m.not
  
  /** matches if a.isEmpty */
  def beEmpty[T <% Any { def isEmpty: Boolean }] = new Matcher[T] {
    def apply[S <: T](iterable: Expectable[S]) = {
      result(iterable.value.isEmpty,
             iterable.description + " is empty", 
             iterable.description + " is not empty", iterable)
    }
  }

  /** matches if the value is null */
  def beNull[T] = new BeNull[T]

  /** matches if a is null when v is null and a is not null when v is not null */
  def beAsNullAs[T](a: =>T) = new Matcher[T](){
    def apply[S <: T](y: Expectable[S]) = {
      val x = a
      result(x == null && y.value == null || x != null && y.value != null,
             "both values are null",
             if (x == null) y.description + " is not null" else q(x) + " is not null" + 
             y.optionalDescription.map(" but " + _ + " is null").getOrElse(""), 
             y)
    }
  }

  /** matches if t.toSeq.exists(_ == v) */
  def beOneOf[T](t: T*): Matcher[T] = BeOneOf(t)
  /** alias for beOneOf */
  def beAnyOf[T](t: T*): Matcher[T] = BeOneOf(t)

  /** matches if the value returns a successful result when applied to a PartialFunction */
  def beLike[T](pattern: PartialFunction[T, MatchResult[_]]) = new Matcher[T] {
    def apply[S <: T](a: Expectable[S]) = {
      val r = if (pattern.isDefinedAt(a.value)) pattern.apply(a.value) else MatchFailure("", "", a)
      result(r.isSuccess,
             a.description + " is correct: " + r.message,
             a.description + " is incorrect: " + r.message,
             a)
    }
  }
  /** matches if v.getClass == c */
  def haveClass[T : ClassTag] = new Matcher[Any] {
    def apply[S <: Any](x: Expectable[S]) = {
      val c = implicitly[ClassTag[T]].runtimeClass
      val xClass = x.value.asInstanceOf[java.lang.Object].getClass
      result(xClass == c,
             x.description + " has class " + q(c.getName),
             x.description + " doesn't have class " + q(c.getName) + ". It has class " + q(xClass.getName),
             x)
    }
  }
  /** matches if c.isAssignableFrom(v.getClass.getSuperclass) */
  def haveSuperclass[T : ClassTag] = new Matcher[Any] {
    def apply[S <: Any](x: Expectable[S]) = {
      val c = implicitly[ClassTag[T]].runtimeClass
      val xClass = x.value.asInstanceOf[java.lang.Object].getClass
      result(c.isAssignableFrom(xClass.getSuperclass),
             x.description + " has super class " + q(c.getName),
             x.description + " doesn't have super class " + q(c.getName) + ". It has super class " + q(xClass.getSuperclass.getName),
             x)
    }
  }

  /** matches if x.getClass.getInterfaces.contains(T) */
  def haveInterface[T : ClassTag] = new Matcher[Any] {
    def apply[S <: Any](x: Expectable[S]) = {
      val c = implicitly[ClassTag[T]].runtimeClass
      val xClass = x.value.asInstanceOf[java.lang.Object].getClass
      result(xClass.getInterfaces.contains(c),
             x.description + " has interface " + q(c.getName),
             x.description + " doesn't have interface " + q(c.getName) + ". It has interface " + xClass.getInterfaces.mkString(", "),
             x)
    }
  }

  /** matches if v.isAssignableFrom(c) */
  def beAssignableFrom[T : ClassTag] = new Matcher[Class[_]] {
    def apply[S <: Class[_]](x: Expectable[S]) = {
      val c = implicitly[ClassTag[T]].runtimeClass
      result(x.value.isAssignableFrom(c), 
             x.description + " is assignable from " + q(c.getName), 
             x.description + " is not assignable from " + q(c.getName), 
             x)
    }
  }

  def beAnInstanceOf[T: ClassTag] = new Matcher[Any] {
    def apply[S <: Any](x: Expectable[S]) = {
      val c = implicitly[ClassTag[T]].runtimeClass
      val xClass = x.value.asInstanceOf[java.lang.Object].getClass
      result(c.isAssignableFrom(xClass),
             x.description + " is an instance of " + q(c.getName),
             x.description + " is not an instance of " + q(c.getName),
             x)
    }
  }
}


/**
 * Matcher for a boolean value which must be true
 */
class BeTrueMatcher extends Matcher[Boolean] {
  def apply[S <: Boolean](v: Expectable[S]) = {
    result(v.value, v.description + " is true", v.description + " is false", v) 
  }
}
/**
 * Typed equality Matcher
 */
class BeTypedEqualTo[T](t: =>T, equality: (T, T) => Boolean = (t1:T, t2:T) => t1 == t2) extends AdaptableMatcher[T] { outer =>
  import AnyMatchers._
  protected val ok: String => String = identity
  protected val ko: String => String = identity
  
  def adapt(f: T => T, okFunction: String => String, koFunction: String => String) = {
    new BeTypedEqualTo(f(t), equality) {
      override def apply[S <: T](s: Expectable[S]): MatchResult[S] = {
        val originalValues = s"\nOriginal values\n  Expected: '$t'\n  Actual  : '${s.value}'"
        result(super.apply(s.map(f)).updateMessage(_+originalValues), s)
      }
      override protected val ok: String => String = okFunction compose outer.ok
      override protected val ko: String => String = koFunction compose outer.ko
    }
  }
  
  def apply[S <: T](b: Expectable[S]): MatchResult[S] = {
    val (actual, expected) = (b.value, t)
    def isEqual =
      (actual, expected) match {
        case (arr: Array[_], arr2: Array[_])    => arr.deep == arr2.deep
        case (arr: Array[_], t: Traversable[_]) => arr.deep == t
        case (t: Traversable[_], arr: Array[_]) => t == arr.deep
        case other                              => equality(actual, expected)
      }

    lazy val (db, qa) =
      (b.description, q(expected)) match {
        case (x, y) if !isEqual && x == y =>
          val (actualWithClass, expectedWithClass) = (actual.notNullWithClass, expected.notNullWithClass)
          if (actualWithClass == expectedWithClass) (b.describe(actual.notNullWithClass(showAll = true)), q(expected.notNullWithClass(showAll = true)))
          else                                      (b.describe(actualWithClass), q(expectedWithClass))

        case other                          => other
      }

    def print(b: String, msg: String, a: String) = Seq(b, msg, a).mkString("\n".unless(Seq(a, b).exists(_.size <= 40)))
    result(isEqual, ok(print(db, " is equal to ", qa)), ko(print(db, " is not equal to ", qa)), b, expected.notNull, actual.notNull)
  }

  def expected = t
}

/**
 * Equality Matcher
 */
class BeEqualTo(t: =>Any) extends BeTypedEqualTo(t)
/**
 * This matcher always matches any value of type T
 */
case class AlwaysMatcher[T]() extends Matcher[T] {
  def apply[S <: T](e: Expectable[S]) = result(true, "ok", "ko", e)
}
/**
 * This matcher never matches any value of type T
 */
case class NeverMatcher[T]() extends Matcher[T] {
  def apply[S <: T](e: Expectable[S]) = result(false, "ok", "ko", e)
}
/**
 * This trait allows to write expressions like
 * 
 *  `1 must be equalTo(1)`
 */
trait AnyBeHaveMatchers { outer: AnyMatchers =>
  implicit def anyBeHaveMatcher[T](result: MatchResult[T]) = new AnyBeHaveMatchers(result)
  class AnyBeHaveMatchers[T](result: MatchResult[T]) {
    def be_==(t: T) = result(outer.be_==(t))
    def be_!=(t: T) = result(outer.be_!=(t))
    def be_===(t: T) = result(outer.be_===(t))
    def be_!==(t: T) = result(outer.be_!==(t))
    def be_==~[S](s: =>S)(implicit convert: S => T) = result(outer.be_==~[T, S](s))
    def equalTo(t: T) = result(outer.be_==(t))
    def asNullAs[T](a: =>T) = result(outer.beAsNullAs(a))
    def beAnyOf(t: T*) = result(outer.beAnyOf(t:_*))
    def beOneOf(t: T*) = result(outer.beOneOf(t:_*))
    def anyOf(t: T*) = result(outer.beAnyOf(t:_*))
    def oneOf(t: T*) = result(outer.beOneOf(t:_*))
    def beNull = result(outer.beNull)
    def anInstanceOf[T : ClassTag] = result(beAnInstanceOf[T])
  }

  implicit def toAnyRefMatcherResult[T <: AnyRef](result: MatchResult[T]) = new AnyRefMatcherResult(result)
  class AnyRefMatcherResult[T <: AnyRef](result: MatchResult[T]) {
    def beTheSameAs(t: T) = result(outer.beTheSameAs(t))
  }

  implicit def toAnyMatcherResult(result: MatchResult[Any]) = new AnyMatcherResult(result)
  class AnyMatcherResult(result: MatchResult[Any]) {
    def haveClass[T : ClassTag] = result(outer.haveClass[T])
  }

  implicit def toClassMatcherResult(result: MatchResult[Class[_]]) = new ClassMatcherResult(result)
  class ClassMatcherResult(result: MatchResult[Class[_]]) {
    def assignableFrom = result(outer.beAssignableFrom)
  }
  
  implicit def anyWithEmpty[T <% Any { def isEmpty: Boolean }](result: MatchResult[T]) = 
    new AnyWithEmptyMatchers(result)

  class AnyWithEmptyMatchers[T <% Any { def isEmpty: Boolean }](result: MatchResult[T]) {
    def empty = result(outer.beEmpty[T])
    def beEmpty = result(outer.beEmpty[T])
  }
  implicit def toBeLikeResultMatcher[T](result: MatchResult[T]) = new BeLikeResultMatcher(result)
  class BeLikeResultMatcher[T](result: MatchResult[T]) {
    def like(pattern: =>PartialFunction[T, MatchResult[_]]) = result(outer.beLike(pattern))
    def likeA(pattern: =>PartialFunction[T, MatchResult[_]]) = result(outer.beLike(pattern))
  }
  def asNullAs[T](a: =>T) = beAsNullAs(a)
  def like[T](pattern: =>PartialFunction[T, MatchResult[_]]) = beLike(pattern)
  def beLikeA[T](pattern: =>PartialFunction[T, MatchResult[_]]) = beLike(pattern)
  def likeA[T](pattern: =>PartialFunction[T, MatchResult[_]]) = beLike(pattern)
  def empty[T <: Any { def isEmpty: Boolean }] = beEmpty[T]
  def oneOf[T](t: T*) = beOneOf(t:_*)
  def anyOf[T](t: T*) = beAnyOf(t:_*)
  def klass[T : ClassTag]: Matcher[Any] = outer.haveClass[T]
  def superClass[T : ClassTag]: Matcher[Any] = outer.haveSuperclass[T]
  def interface[T : ClassTag]: Matcher[Any] = outer.haveInterface[T]
  def assignableFrom[T : ClassTag] = outer.beAssignableFrom[T]
  def anInstanceOf[T : ClassTag] = outer.beAnInstanceOf[T]
}
class BeTheSameAs[T <: AnyRef](t: =>T) extends Matcher[T] {
  def apply[S <: T](a: Expectable[S]) = {
    val b = t
    result(a.value eq b, a.description + " is the same as " + q(b), a.description + " is not the same as " + q(b), a)
  }
}
class BeNull[T] extends Matcher[T] {
  def apply[S <: T](value: Expectable[S]) = {
    result(value.value == null,
           value.description + " is null",
           value.description + " is not null", value)
  }
}
case class BeOneOf[T](t: Seq[T]) extends Matcher[T] {
  def apply[S <: T](y: Expectable[S]) = {
    val x = t
    result(x.exists(_ == y.value),
      y.description + s" is contained in " + q(x.mkString(", ")),
      y.description + s" is not contained in " + q(x.mkString(", ")),
      y)
  }
}
