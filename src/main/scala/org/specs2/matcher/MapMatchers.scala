package org.specs2
package matcher
import text.Quote._
import text.Plural._

/**
 * Matchers for Maps
 */
trait MapMatchers extends MapBaseMatchers with MapBeHaveMatchers
object MapMatchers extends MapMatchers

private[specs2]
trait MapBaseMatchers {
  
  /** matches if map.contains(k) */   
  def haveKey[K](k: K) = new Matcher[Iterable[(K, Any)]] { 
    def apply[S <: Iterable[(K, Any)]](m: =>Expectable[S]) = {
      val map = m
      result(map.value.exists(_._1 == k), 
             map.description + " has the key " + q(k), 
             map.description + " doesn't have the key " + q(k), 
             map)
    }
  } 
  /** matches if map contains a pair (key, value) with value == v */   
  def haveValue[V](v: V) = new Matcher[Iterable[(Any, V)]] { 
    def apply[S <: Iterable[(Any, V)]](m: =>Expectable[S]) = {
      val map = m
      result(map.value.exists(_._2 == v), 
             map.description + " has the value " + q(v), 
             map.description + " doesn't have the value " + q(v),
             map)
    } 
  }

  /** matches if map contains a pair (key, value) == (k, v) */   
  def havePair[K, V](p: (K, V)) = new Matcher[Iterable[(K, V)]] {
    def apply[S <: Iterable[(K, V)]](m: =>Expectable[S]) = {
       val map = m
       result(map.value.exists(_ == p), 
              map.description + " has the pair " + q(p), 
              map.description + " doesn't have the pair " + q(p),
              map)
     }
  }
  /** matches if map contains all the specified pairs */   
  def havePairs[K, V](pairs: (K, V)*) = new Matcher[Iterable[(K, V)]] {
    def apply[S <: Iterable[(K, V)]](m: =>Expectable[S]) = {
       val map = m
       result(pairs.forall(pair => map.value.exists(_ == pair)), 
              map.description + " has the pairs " + q(pairs.mkString(", ")), 
              map.description + " doesn't have the pairs " + q(pairs.mkString(", ")),
              map)
     }
  }

  /** matches if the partial function is defined at those values */   
  def beDefinedAt[K](values: K*) = new Matcher[PartialFunction[K, Any]] {
    def apply[S <: PartialFunction[K, Any]](f: =>Expectable[S]) = {
      val isDefined = values.map(v => (v, f.value.isDefinedAt(v)))
      val undefined = isDefined.filter(!_._2).map(_._1)
      result(isDefined.map(_._2).forall(_ == true), 
             f.optionalDescription.getOrElse("the function") + " is defined for the value".plural(values.size) + " " + q(values.mkString(", ")), 
             f.optionalDescription.getOrElse("the function") + " is not defined for the value".plural(undefined.size) + " " + q(undefined.mkString(", ")), 
             f)
    }
  }
  
  /** matches if the partial function is defined at those values and return expected values */   
  def beDefinedBy[K, V](values: (K, V)*) = new Matcher[PartialFunction[K, V]] {
    def apply[S <: PartialFunction[K, V]](f: =>Expectable[S]) = {
      val isDefined = values.map(v => (v, f.value.isDefinedAt(v._1) && f.value(v._1) == v._2))
      val undefined = isDefined.filter(!_._2).map(_._1)
      result(isDefined.map(_._2).forall(_ == true), 
             f.optionalDescription.getOrElse("the function") + " is defined by the value".plural(values.size) + " " + q(values.mkString(", ")), 
             f.optionalDescription.getOrElse("the function") + " is not defined by the value".plural(undefined.size) + " " + q(undefined.mkString(", ")),
             f)
    }
 }

}
private[specs2]
trait MapBeHaveMatchers { outer: MapBaseMatchers =>
  implicit def toMapKeyResultMatcher[K](result: MatchResult[Iterable[(K, Any)]]) = new MapKeyResultMatcher(result)
  class MapKeyResultMatcher[K](result: MatchResult[Iterable[(K, Any)]]) {
    def key(k: K) = result(haveKey(k))   
  }
  implicit def toMapValueResultMatcher[V](result: MatchResult[Iterable[(Any, V)]]) = new MapValueResultMatcher(result)
  class MapValueResultMatcher[V](result: MatchResult[Iterable[(Any, V)]]) {
    def value(v: V) = result(haveValue(v)) 
  }
  implicit def toMapResultMatcher[K, V](result: MatchResult[Iterable[(K, V)]]) = new MapResultMatcher(result)
  class MapResultMatcher[K, V](result: MatchResult[Iterable[(K, V)]]) {
    def pair(p: (K, V)) = result(havePair(p))
    def pairs(pairs: (K, V)*) = result(havePairs(pairs:_*))
  }
  implicit def toPartialFunctionResultMatcher[K, V](result: MatchResult[PartialFunction[K, V]]) = new PartialFunctionResultMatcher(result)
  class PartialFunctionResultMatcher[K, V](result: MatchResult[PartialFunction[K, V]]) {
  def definedAt(values: K*) = beDefinedAt(values:_*)
  def definedBy(values: (K, V)*) = beDefinedBy(values:_*)
  }
  def key[K](k: K) = haveKey(k)   
  def value[V](v: V) = haveValue(v) 
  def pair[K, V](p: (K, V)) = havePair(p)
  def pairs[K, V](pairs: (K, V)*) = havePairs(pairs:_*)
  def definedAt[K](values: K*) = beDefinedAt(values:_*)
  def definedBy[K, V](values: (K, V)*) = beDefinedBy(values:_*)
}
