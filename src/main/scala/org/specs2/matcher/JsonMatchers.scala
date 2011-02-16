package org.specs2
package matcher

import scala.util.parsing.json._
import text.Quote._
import AnyMatchers._
import MapMatchers._
import IterableMatchers._

/**
 * Matchers for Json expressions (entered as strings)
 */
trait JsonMatchers extends JsonBaseMatchers with JsonBaseBeHaveMatchers

private[specs2]
trait JsonBaseMatchers {

  def /(value: String): JsonValueMatcher = new JsonValueMatcher(value)
  def */(value: String): JsonDeepValueMatcher = new JsonDeepValueMatcher(value)

  def /(pair: (String, String)): JsonPairMatcher = new JsonPairMatcher(pair._1, pair._2)
  def */(pair: (String, String)): JsonDeepPairMatcher = new JsonDeepPairMatcher(pair._1, pair._2)
}

private[specs2]
trait JsonBaseBeHaveMatchers { outer: JsonBaseMatchers =>

  implicit def toNotMatcherJson(result: NotMatcher[Any]) : NotMatcherJson = new NotMatcherJson(result)
  class NotMatcherJson(result: NotMatcher[Any]) {
    def /(pair: (String, String)): JsonPairMatcher = outer./(pair).not
    def */(pair: (String, String)): JsonDeepPairMatcher = outer.*/(pair).not
    def /(value: String): JsonValueMatcher = outer./(value).not
    def */(value: String): JsonDeepValueMatcher = outer.*/(value).not
  }
}

class JsonPairMatcher(key: String, value: String) extends Matcher[String] {
  def navigate(s: String): Option[Any] = JSON.parseRaw(s)
  def apply[S <: String](s: Expectable[S]) = {
    navigate(s.value) match {
      case Some(JSONObject(obj)) => result(havePair[Any, Any](key->value).apply(Expectable(obj)), s)
      case Some(JSONArray(list)) => result(false, "ok", list.mkString("[", ", ", "]")+" doesn't contain: "+key+" -> "+value, s)
      case None                  => result(false, "ok", "Could not parse:\n"+s.value, s)
    }
  }
  override def not = new JsonPairMatcher(key, value) {
    override def apply[S <: String](s: Expectable[S]) = super.apply(s).not
  }
}
class JsonValueMatcher(value: String) extends Matcher[String] { outer =>
  def navigate(s: String): Option[Any] = JSON.parseRaw(s)
  def apply[S <: String](s: Expectable[S]) = {
    navigate(s.value) match {
      case Some(JSONObject(obj)) => result(false, "ok", obj.map(p => p._1+": "+p._2).mkString("{ ", ", ", " }")+" doesn't contain: "+q(value), s)
      case Some(JSONArray(list)) => result(contain[Any](value).apply(Expectable(list)), s)
      case None                  => result(false, "ok", "Could not parse:\n"+s.value, s)
    }
  }
  override def not = new JsonValueMatcher(value) {
    override def apply[S <: String](s: Expectable[S]) = super.apply(s).not
  }
  /** in this case, interpret 'value' as the key and value1 as the expected value in the Array */
  def /(value1: String) = new JsonValueMatcher(value1) {
    override def navigate(s: String) = outer.navigate(s) match {
      case Some(JSONObject(obj)) => obj.get(value)
      case _                     => None
    }
  }
  /** in this case, interpret 'value' as the key and key1/value1 as the expected pair in the Map */
  def /(pair1: (String, String)) = new JsonPairMatcher(pair1._1, pair1._2) {
    override def navigate(s: String) = outer.navigate(s) match {
      case Some(JSONObject(obj)) => obj.get(value)
      case _                     => None
    }
  }
}
class JsonDeepPairMatcher(key: String, value: String) extends Matcher[String] {
  def navigate(s: String) = JSON.parseRaw(s)
  def apply[S <: String](s: Expectable[S]) = {
    navigate(s.value) match {
      case Some(json) => result(true, "", "", s)
      case None       => result(false, "ok", "Could not parse:\n"+s.value, s)
    }
  }
  override def not = new JsonDeepPairMatcher(key, value) {
    override def apply[S <: String](s: Expectable[S]) = super.apply(s).not
  }
}
class JsonDeepValueMatcher(value: String) extends Matcher[String] {
  def apply[S <: String](s: Expectable[S]) = {
    val parsed = JSON.parseFull(s.value)
    parsed match {
      case Some(json) => result(true, "", "", s)
      case None       => result(false, "ok", "Could not parse:\n"+s.value, s)
    }
  }
  override def not = new JsonDeepValueMatcher(value) {
    override def apply[S <: String](s: Expectable[S]) = super.apply(s).not
  }
}