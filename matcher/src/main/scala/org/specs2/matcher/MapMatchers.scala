package org.specs2
package matcher

import text.Quote._
import text.Plural._
import scalaz._
import Scalaz._
/**
 * Matchers for Maps
 */
trait MapMatchers extends MapBaseMatchers with MapBeHaveMatchers
object MapMatchers extends MapMatchers

private[specs2]
trait MapBaseMatchers {
  
  /** matches if map.contains(k) */   
  def haveKey[K](k: K) = haveKeys(k)

    /** matches if map.contains(k) forall key k */
  def haveKeys[K](keys: K*) = new Matcher[Iterable[(K, Any)]] {
      def apply[S <: Iterable[(K, Any)]](map: Expectable[S]) = {
        lazy val missing = keys.filterNot(map.value.map(_._1).toSeq.contains)

        result(missing.isEmpty,
               map.description+s" has the ${"key".plural(keys)} "+q(keys.mkString(", ")),
               map.description+s" doesn't have the ${"key".plural(missing)} "+q(missing.mkString(", ")),
               map)
      }
      override def not = new Matcher[Iterable[(K, Any)]] {
        def apply[S <: Iterable[(K, Any)]](map: Expectable[S]) = {
          lazy val existing = map.value.map(_._1).filter(keys.contains)

          result(existing.isEmpty,
                 map.description+s" doesn't have the ${"key".plural(keys)}"+q(keys),
                 map.description+s" has the ${"key".plural(existing)} "+q(existing.mkString(", ")),
                 map)
        }
      }
  }

  /** matches if map contains a pair (key, value) with value == v */   
  def haveValue[V](v: V) = haveValues(v)

  /** matches if map contains a pair (key, value) with value == v  for all value v*/
  def haveValues[V](values: V*) = new Matcher[Iterable[(Any, V)]] {

    def apply[S <: Iterable[(Any, V)]](map: Expectable[S]) = {
      lazy val missing = values.filterNot(map.value.map(_._2).toSeq.contains)

      result(missing.isEmpty,
             map.description+s" has the ${"value".plural(values)} "+q(values.mkString(", ")),
             map.description+s" doesn't have the ${"value".plural(missing)} "+q(missing.mkString(", ")),
             map)
    }
    override def not = new Matcher[Iterable[(Any, V)]] {
      def apply[S <: Iterable[(Any, V)]](map: Expectable[S]) = {
        lazy val existing = map.value.map(_._2).filter(values.contains)

        result(existing.isEmpty,
               map.description+s" doesn't have the ${"value".plural(values)} "+q(values.mkString(", ")),
               map.description+s" has the ${"value".plural(existing)} "+q(existing.mkString(", ")),
               map)
      }
    }
  }

  /** matches if map contains a pair (key, value) == (k, v) */   
  def havePair[K, V](p: (K, V)) = havePairs(p)
  
  /** matches if map contains all the specified pairs */   
  def havePairs[K, V](pairs: (K, V)*) = new Matcher[Iterable[(K, V)]] {
    def apply[S <: Iterable[(K, V)]](map: Expectable[S]) = {
      lazy val missing = pairs.filterNot(map.value.toSeq.contains)
      result(missing.isEmpty,
             map.description+s" has the ${"pair".plural(pairs)} "+q(pairs.mkString(", ")),
             map.description+s" doesn't have the ${"pair".plural(missing)} "+q(missing.mkString(", ")),
             map)
    }
    override def not = new Matcher[Iterable[(K, V)]] {
      def apply[S <: Iterable[(K, V)]](map: Expectable[S]) = {
        lazy val existing = map.value.filter(pairs.contains)
        result(existing.isEmpty,
               map.description+s" doesn't have the ${"pair".plural(pairs)} "+q(pairs.mkString(", ")),
               map.description+s" has the ${"pair".plural(existing)} "+q(existing.mkString(", ")),
               map)
      }
    }
  }

  /** matches if the partial function is defined at those values */   
  def beDefinedAt[K](values: K*) = new Matcher[PartialFunction[K, Any]] {
    def apply[S <: PartialFunction[K, Any]](f: Expectable[S]) = {
      val isDefined = values.map(v => (v, f.value.isDefinedAt(v)))
      val undefined = isDefined.filter(!_._2).map(_._1)
      result(isDefined.map(_._2).forall(_ == true), 
             f.optionalDescription.getOrElse("the function") + Noun(" is defined for the value").plural(values.size) +
                                             " " + q(values.mkString(", ")),
             f.optionalDescription.getOrElse("the function") +
                                             Noun(" is not defined for the value").plural(undefined.size) + " " +
                                             q(undefined.mkString(", ")), f)
    }
  }
  
  /** matches if the partial function is defined at those values and return expected values */   
  def beDefinedBy[K, V](values: (K, V)*) = new Matcher[PartialFunction[K, V]] {
    def apply[S <: PartialFunction[K, V]](f: Expectable[S]) = {
      val isDefined = values.map(v => (v, f.value.isDefinedAt(v._1) && f.value(v._1) == v._2))
      val undefined = isDefined.filter(!_._2).map(_._1)
      result(isDefined.map(_._2).forall(_ == true), 
             f.optionalDescription.getOrElse("the function") + Noun(" is defined by the value").plural(values.size) + " " + q(values.mkString(", ")),
             f.optionalDescription.getOrElse("the function") + Noun(" is not defined by the value").plural(undefined.size) + " " + q(undefined.mkString(", ")),
             f)
    }
  }

}
private[specs2]
trait MapBeHaveMatchers { outer: MapBaseMatchers =>
  implicit def toMapKeyResultMatcher[K](result: MatchResult[Iterable[(K, Any)]]) = new MapKeyResultMatcher(result)
  class MapKeyResultMatcher[K](result: MatchResult[Iterable[(K, Any)]]) {
    def key(k: K) = result(outer.haveKey(k))
    def keys(ks: K*) = result(outer.haveKeys(ks:_*))
    def haveKey(k: K) = result(outer.haveKey(k))
    def haveKeys(ks: K*) = result(outer.haveKeys(ks:_*))
  }
  implicit def toMapValueResultMatcher[V](result: MatchResult[Iterable[(Any, V)]]) = new MapValueResultMatcher(result)
  class MapValueResultMatcher[V](result: MatchResult[Iterable[(Any, V)]]) {
    def value(v: V) = result(outer.haveValue(v))
    def values(vs: V*) = result(outer.haveValues(vs:_*))
    def haveValue(v: V) = result(outer.haveValue(v))
    def haveValues(vs: V*) = result(outer.haveValues(vs:_*))
  }
  implicit def toMapResultMatcher[K, V](result: MatchResult[Iterable[(K, V)]]) = new MapResultMatcher(result)
  class MapResultMatcher[K, V](result: MatchResult[Iterable[(K, V)]]) {
    def pair(p: (K, V)) = result(outer.havePair(p))
    def pairs(pairs: (K, V)*) = result(outer.havePairs(pairs:_*))
    def havePair(p: (K, V)) = result(outer.havePair(p))
    def havePairs(pairs: (K, V)*) = result(outer.havePairs(pairs:_*))
  }
  implicit def toPartialFunctionResultMatcher[K, V](result: MatchResult[PartialFunction[K, V]]) = new PartialFunctionResultMatcher(result)
  class PartialFunctionResultMatcher[K, V](result: MatchResult[PartialFunction[K, V]]) { 
    def definedAt(values: K*) = result(outer.beDefinedAt(values:_*))
    def beDefinedAt(values: K*) = result(outer.beDefinedAt(values:_*))
    def definedBy(values: (K, V)*) = result(outer.beDefinedBy(values:_*))
    def beDefinedBy(values: (K, V)*) = result(outer.beDefinedBy(values:_*))
  }
  def key[K](k: K) = haveKey(k)
  def keys[K](ks: K*) = haveKeys(ks:_*)
  def value[V](v: V) = haveValue(v)
  def values[V](vs: V*) = haveValues(vs:_*)
  def pair[K, V](p: (K, V)) = havePair(p)
  def pairs[K, V](pairs: (K, V)*) = havePairs(pairs:_*)
  def definedAt[K](values: K*) = beDefinedAt(values:_*)
  def definedBy[K, V](values: (K, V)*) = beDefinedBy(values:_*)
}

