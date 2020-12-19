package org.specs2
package matcher

import text.Quote._
import text.Plural._
import execute.Result._

/**
 * Matchers for Maps
 */
trait MapMatchers:

  /** matches if map.contains(k) */
  def haveKey[K](k: K): Matcher[Iterable[(K, Any)]] =
    haveKeys(k)

    /** matches if map.contains(k) forall key k */
  def haveKeys[K](keys: K*): Matcher[Iterable[(K, Any)]] =
    new Matcher[Iterable[(K, Any)]]:
      def apply[S <: Iterable[(K, Any)]](map: Expectable[S]) =
        lazy val missing = keys.filterNot(map.value.map(_._1).toSeq.contains)

        result(missing.isEmpty,
               map.description+s" doesn't have the ${"key".plural(missing)} "+q(missing.mkString(", ")))

      override def not: Matcher[Iterable[(K, Any)]] =
        new Matcher[Iterable[(K, Any)]]:
          def apply[S <: Iterable[(K, Any)]](map: Expectable[S]) =
            lazy val existing = map.value.map(_._1).filter(keys.contains)

            result(existing.isEmpty,
                   map.description+s" has the ${"key".plural(existing)} "+q(existing.mkString(", ")))

  /** matches if map contains a pair (key, value) with value == v */
  def haveValue[V](v: V): Matcher[Iterable[(Any, V)]] =
    haveValues(v)

  /** matches if map contains a pair (key, value) with value == v  for all value v*/
  def haveValues[V](values: V*): Matcher[Iterable[(Any, V)]] =
    new Matcher[Iterable[(Any, V)]]:
      def apply[S <: Iterable[(Any, V)]](map: Expectable[S]) =
        lazy val missing = values.filterNot(map.value.map(_._2).toSeq.contains)

        result(missing.isEmpty,
               map.description+s" doesn't have the ${"value".plural(missing)} "+q(missing.mkString(", ")))

      override def not: Matcher[Iterable[(Any, V)]] =
        new Matcher[Iterable[(Any, V)]]:
          def apply[S <: Iterable[(Any, V)]](map: Expectable[S]) =
            lazy val existing = map.value.map(_._2).filter(values.contains)

            result(existing.isEmpty,
                   map.description+s" has the ${"value".plural(existing)} "+q(existing.mkString(", ")))

  /** matches if map contains a pair (key, value) == (k, v) */
  def havePair[K, V](p: (K, V)): Matcher[Iterable[(K, V)]] =
    havePairs(p)

  /** matches if map contains all the specified pairs */
  def havePairs[K, V](pairs: (K, V)*): Matcher[Iterable[(K, V)]] =
    new Matcher[Iterable[(K, V)]]:
      def apply[S <: Iterable[(K, V)]](map: Expectable[S]) =
        lazy val missing = pairs.filterNot(map.value.toSeq.contains)
        result(missing.isEmpty,
               map.description+s" doesn't have the ${"pair".plural(missing)} "+q(missing.mkString(", ")))

      override def not: Matcher[Iterable[(K, V)]] =
        new Matcher[Iterable[(K, V)]]:
          def apply[S <: Iterable[(K, V)]](map: Expectable[S]) =
            lazy val existing = map.value.filter(pairs.contains)
            result(existing.isEmpty,
                   map.description+s" has the ${"pair".plural(existing)} "+q(existing.mkString(", ")))


  /** matches if the partial function is defined at those values */
  def beDefinedAt[K](values: K*): Matcher[PartialFunction[K, Any]] =
    new Matcher[PartialFunction[K, Any]]:
      def apply[S <: PartialFunction[K, Any]](f: Expectable[S]) =
        val isDefined = values.map(v => (v, f.value.isDefinedAt(v)))
        val undefined = isDefined.filter(!_._2).map(_._1)
        val functionDescription = f.showValue.map(_(f.value.toString)).getOrElse("the function")
        result(isDefined.map(_._2).forall(_ == true),
               functionDescription + " is not defined for the value".plural(undefined.size) +
                 " " + q(undefined.mkString(", ")))

  /** matches if the partial function is defined at those values and return expected values */
  def beDefinedBy[K, V](values: (K, V)*): Matcher[PartialFunction[K, V]] =
    new Matcher[PartialFunction[K, V]]:
      def apply[S <: PartialFunction[K, V]](f: Expectable[S]) =
        val isDefined = values.map(v => (v, f.value.isDefinedAt(v._1) && f.value(v._1) == v._2))
        val undefined = isDefined.filter(!_._2).map(_._1)
        val functionDescription = f.showValue.map(_(f.value.toString)).getOrElse("the function")
        result(isDefined.map(_._2).forall(_ == true),
               functionDescription + " is not defined by the value".plural(undefined.size) + " " + q(undefined.mkString(", ")))

object MapMatchers extends MapMatchers
