package org.specs2
package matcher

import scala.util.parsing.json._
import text.Quote._
import text.Trim._
import text.NotNullStrings._
import TraversableMatchers._
import json.Json._

/**
 * Matchers for Json expressions (entered as strings)
 */
trait JsonMatchers extends JsonBaseMatchers with JsonBaseBeHaveMatchers

private[specs2]
trait JsonBaseMatchers extends Expectations {
  /** match if the document contains the value at the top-level */
  def /(value: Any): JsonValueMatcher = new JsonValueMatcher(value)
  /** match if the document contains the value at any level */
  def */(value: Any): JsonDeepValueMatcher = new JsonDeepValueMatcher(value)

  /** match if the document contains the pair at the top level */
  def /(pair: (String, Any)): JsonPairMatcher = new JsonPairMatcher(pair._1, pair._2)
  /** match if the document contains the pair at any level */
  def */(pair: (String, Any)): JsonDeepPairMatcher = new JsonDeepPairMatcher(pair._1, pair._2)

  class JsonPairMatcher(key: String, value: Any) extends Matcher[String] {
    def navigate(json: JSONType): Option[JSONType] = Some(json)

    def apply[S <: String](s: Expectable[S]) = {
      parse(s.value).map(navigate) match {
        case Some(Some(JSONObject(obj))) => result(MapMatchers.havePair[Any, Any](key->value).apply(createExpectable(obj)), s)
        case Some(Some(JSONArray(list))) => result(false, "ok", list.mkString("[", ", ", "]")+" doesn't contain: "+key+" -> "+value, s)
        case Some(None)                  => result(false, "ok", s.value+" is empty", s)
        case None                        => result(false, "ok", "Could not parse:\n"+s.value, s)
      }
    }
    override def not = new JsonPairMatcher(key, value) {
      override def apply[S <: String](s: Expectable[S]) = super.apply(s).not
    }
  }
  class JsonValueMatcher(value: Any) extends Matcher[Any] { outer =>
    def navigate(json: JSONType): Option[JSONType] = Some(json)

    def apply[S <: Any](s: Expectable[S]) = {
      parse(s.value.notNull).map(navigate) match {
        case Some(Some(JSONObject(obj))) => result(false, "ok", obj.map(p => p._1+": "+p._2).mkString("{ ", ", ", " }")+" doesn't contain: "+q(value), s)
        case Some(Some(JSONArray(list))) => result(contain[Any](value).apply(createExpectable(list)), s)
        case Some(None)                  => result(false, "ok", s.value+" is empty", s)
        case None                        => result(false, "ok", "Could not parse:\n"+s.value, s)
      }
    }
    override def not = new JsonValueMatcher(value) {
      override def apply[S <: Any](s: Expectable[S]) = super.apply(s).not
    }
    /** in this case, interpret 'value' as the key and value1 as the expected value in the Array */
    def /(value1: Any) = new JsonValueMatcher(value1) {
      override def navigate(json: JSONType): Option[JSONType] = outer.navigate(json).flatMap(find(value.notNull, _))
    }
    /** in this case, interpret 'value' as the key and key1/value1 as the expected pair in the Map */
    def /(pair1: (String, Any)) = new JsonPairMatcher(pair1._1, pair1._2) {
      override def navigate(json: JSONType): Option[JSONType] = outer.navigate(json).flatMap(find(value.notNull, _))
    }
    /** in this case, interpret 'value' as the key and value1 as the expected value in the Array */
    def */(value1: String) = new JsonDeepValueMatcher(value1) {
      override def navigate(json: JSONType): Option[JSONType] = outer.navigate(json).flatMap(find(value.notNull, _))
    }
    /** in this case, interpret 'value' as the key and value1 as the expected pair in the map */
    def */(pair1: (String, Any)) = new JsonDeepPairMatcher(pair1._1, pair1._2) {
      override def navigate(json: JSONType): Option[JSONType] = outer.navigate(json).flatMap(find(value.notNull, _))
    }

  }
  class JsonDeepPairMatcher(key: String, value: Any) extends Matcher[String] {
    def navigate(json: JSONType): Option[JSONType] = Some(json)

    def apply[S <: String](s: Expectable[S]) = {
      parse(s.value).map(navigate) match {
        case Some(Some(o)) => result(pairs(o).contains(key->value),
                                     s.value.remove("\"")+" contains: "+key+" -> "+value,
                                     s.value.remove("\"")+" doesn't contain: "+key+" -> "+value, s)
        case Some(None)    => result(false, "ok", s.value+" is empty", s)
        case None          => result(false, "ok", "Could not parse:\n"+s.value, s)
      }
    }

    override def not = new JsonDeepPairMatcher(key, value) {
      override def apply[S <: String](s: Expectable[S]) = super.apply(s).not
    }
  }
  class JsonDeepValueMatcher(value: Any) extends Matcher[Any] { outer =>
    def navigate(json: JSONType): Option[JSONType] = Some(json)

    def apply[S <: Any](s: Expectable[S]) = {
      parse(s.value.notNull).map(navigate) match {
        case Some(Some(o)) => result(contain[Any](value).apply(createExpectable(values(o))), s)
        case Some(None)    => result(false, "ok", s.value.notNull+" is empty", s)
        case None          => result(false, "ok", "Could not parse:\n"+s.value, s)
      }
    }

    override def not = new JsonDeepValueMatcher(value) {
      override def apply[S <: Any](s: Expectable[S]) = super.apply(s).not
    }

    /** in this case, interpret 'value' as the key and value1 as the expected value in the Array */
    def /(value1: Any) = new JsonValueMatcher(value1) {
      override def navigate(json: JSONType): Option[JSONType] = outer.navigate(json).flatMap(findDeep(value.notNull, _))
    }
    /** in this case, interpret 'value' as the key and pair1 as the expected pair in the map */
    def /(pair1: (String, Any)) = new JsonPairMatcher(pair1._1, pair1._2) {
      override def navigate(json: JSONType): Option[JSONType] = outer.navigate(json).flatMap(findDeep(value.notNull, _))
    }
    /** in this case, interpret 'value' as the key and value1 as the expected value in the Array */
    def */(value1: Any) = new JsonDeepValueMatcher(value1) {
      override def navigate(json: JSONType): Option[JSONType] = outer.navigate(json).flatMap(findDeep(value.notNull, _))
    }
    /** in this case, interpret 'value' as the key and value1 as the expected pair in the map */
    def */(pair1: (String, Any)) = new JsonDeepPairMatcher(pair1._1, pair1._2) {
      override def navigate(json: JSONType): Option[JSONType] = outer.navigate(json).flatMap(findDeep(value.notNull, _))
    }
  }
}

private[specs2]
trait JsonBaseBeHaveMatchers { outer: JsonBaseMatchers =>

  implicit def toNotMatcherJson(result: NotMatcher[Any]) : NotMatcherJson = new NotMatcherJson(result)
  class NotMatcherJson(result: NotMatcher[Any]) {
    def /(pair: (String, Any)): JsonPairMatcher = outer./(pair).not
    def */(pair: (String, Any)): JsonDeepPairMatcher = outer.*/(pair).not
    def /(value: Any): JsonValueMatcher = outer./(value).not
    def */(value: Any): JsonDeepValueMatcher = outer.*/(value).not
  }
}

