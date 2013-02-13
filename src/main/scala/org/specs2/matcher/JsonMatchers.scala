package org.specs2
package matcher

import scala.util.parsing.json._
import text.Quote._
import text.NotNullStrings._
import text.Regexes._
import text.Trim._
import json.Json._
import util.matching.Regex

/**
 * Matchers for Json expressions (entered as strings)
 */
trait JsonMatchers extends JsonBaseMatchers with JsonBaseBeHaveMatchers

private[specs2]
trait JsonBaseMatchers extends Expectations { outer =>
  /** match if the document contains the value at the top-level */
  def /(value: Any): JsonValueMatcher = new JsonValueMatcher(value)
  /** match if the document contains the value at any level */
  def */(value: Any): JsonDeepValueMatcher = new JsonDeepValueMatcher(value)

  /** match if the document contains the pair at the top level */
  def /(pair: (Any, Any)): JsonPairMatcher = new JsonPairMatcher(pair._1, pair._2)
  /** match if the document contains the pair at any level */
  def */(pair: (Any, Any)): JsonDeepPairMatcher = new JsonDeepPairMatcher(pair._1, pair._2)

  /** allow to match on the i-th element of an Array or a Map (0-based) */
  def /#(i: Int) = new JsonSelector(i)
  class JsonSelector(i: Int) { parentSelector =>
    protected def select(json: JSONType): Option[JSONType] = json match {
      case JSONObject(map) if i >= 0 && i < map.size  => Some(JSONObject(Map(map.iterator.toSeq(i))))
      case JSONArray(list) if i >= 0 && i < list.size => list(i) match {
        case o: JSONType => Some(o)
        case other       => Some(JSONArray(List(other)))
      }
      case other                                      => None
    }
    def /#(j: Int) = new JsonSelector(j) {
      override def select(json: JSONType): Option[JSONType] = parentSelector.select(json).flatMap(super.select)
    }
    def /(value: Any): JsonValueMatcher = new JsonValueMatcher(value) {
      override def navigate(json: JSONType): Option[JSONType] = select(json)
    }
    def */(value: Any): JsonDeepValueMatcher = new JsonDeepValueMatcher(value) {
      override def navigate(json: JSONType): Option[JSONType] = select(json)
    }
    def /(pair: (Any, Any)): JsonPairMatcher = new JsonPairMatcher(pair._1, pair._2) {
      override def navigate(json: JSONType): Option[JSONType] = select(json)
    }
    def */(pair: (Any, Any)): JsonDeepPairMatcher = new JsonDeepPairMatcher(pair._1, pair._2) {
      override def navigate(json: JSONType): Option[JSONType] = select(json)
    }
  }

  class JsonPairMatcher(key: Any, value: Any) extends Matcher[String] {
    def navigate(json: JSONType): Option[JSONType] = Some(json)

    def apply[S <: String](s: Expectable[S]) = {
      parse(s.value).map(navigate) match {
        case Some(Some(JSONObject(obj))) => result(havePair(obj, obj.toSeq, key, value), s)
        case Some(Some(JSONArray(list))) => result(false, "ok", list.mkString("[", ", ", "]")+" doesn't contain "+stringOrRegex(key, value), s)
        case Some(None)                  => result(false, "ok", s.value+" is empty", s)
        case None                        => result(false, "ok", "Could not parse:\n"+s.value, s)
      }
    }
    override def not = new JsonPairMatcher(key, value) {
      override def apply[S <: String](s: Expectable[S]) = super.apply(s).negate
    }
  }
  class JsonValueMatcher(value: Any) extends Matcher[Any] { parent =>
    def navigate(json: JSONType): Option[JSONType] = Some(json)

    def apply[S <: Any](s: Expectable[S]) = {
      parse(s.value.notNull).map(navigate) match {
        case Some(Some(JSONObject(obj))) => result(false, "ok", obj.map(p => p._1+" : "+p._2).mkString("{", ", ", "}")+" doesn't contain "+stringOrRegex(value), s)
        case Some(Some(JSONArray(list))) => result(containValue(list, value), s)
        case Some(None)                  => result(false, "ok", s.value+" is empty", s)
        case None                        => result(false, "ok", "Could not parse:\n"+s.value, s)
      }
    }
    override def not = new JsonValueMatcher(value) {
      override def apply[S <: Any](s: Expectable[S]) = super.apply(s).negate
    }

    /** select the i'th element after navigation */
    def /#(i: Int) = new JsonSelector(i) {
      override def select(json: JSONType): Option[JSONType] = parent.navigate(json) match {
        case Some(JSONObject(map)) => map.find { case (k, v) => regexOrEqualMatch(k, value) }.flatMap {
          case (_, o: JSONType) => super.select(o)
          case _                => None
        }
        case other                 => None
      }
    }
    /** in this case, interpret 'value' as the key and value1 as the expected value in the Array */
    def /(value1: Any) = new JsonValueMatcher(value1) {
      override def navigate(json: JSONType): Option[JSONType] = parent.navigate(json).flatMap(find(value, _))
    }
    /** in this case, interpret 'value' as the key and key1/value1 as the expected pair in the Map */
    def /(pair1: (Any, Any)) = new JsonPairMatcher(pair1._1, pair1._2) {
      override def navigate(json: JSONType): Option[JSONType] = parent.navigate(json).flatMap(find(value, _))
    }
    /** in this case, interpret 'value' as the key and value1 as the expected value in the Array */
    def */(value1: Any) = new JsonDeepValueMatcher(value1) {
      override def navigate(json: JSONType): Option[JSONType] = parent.navigate(json).flatMap(find(value, _))
    }
    /** in this case, interpret 'value' as the key and value1 as the expected pair in the map */
    def */(pair1: (Any, Any)) = new JsonDeepPairMatcher(pair1._1, pair1._2) {
      override def navigate(json: JSONType): Option[JSONType] = parent.navigate(json).flatMap(find(value, _))
    }

  }
  class JsonDeepPairMatcher(key: Any, value: Any) extends Matcher[String] {
    def navigate(json: JSONType): Option[JSONType] = Some(json)

    def apply[S <: String](s: Expectable[S]) = {
      parse(s.value).map(navigate) match {
        case Some(Some(JSONObject(o: Map[_,_]))) if havePair(o.toSeq, key, value) => result(true,
                                                                                            havePairOkMessage(o, o.toSeq, key, value), havePairKoMessage(o, o.toSeq, key, value), s)
        case Some(Some(o))                                                        => result(havePair(o, terminalPairs(o), key, value), s)
        case Some(None)                                                           => result(false, "ok", s.value+" is empty", s)
        case None                                                                 => result(false, "ok", "Could not parse:\n"+s.value, s)
      }
    }

    override def not = new JsonDeepPairMatcher(key, value) {
      override def apply[S <: String](s: Expectable[S]) = super.apply(s).negate
    }
  }
  class JsonDeepValueMatcher(value: Any) extends Matcher[Any] { parent =>
    def navigate(json: JSONType): Option[JSONType] = Some(json)

    def apply[S <: Any](s: Expectable[S]) = {
      parse(s.value.notNull).map(navigate) match {
        case Some(Some(o)) => result(containValue(terminalValues(o), value), s)
        case Some(None)    => result(false, "ok", s.value.notNull+" is empty", s)
        case None          => result(false, "ok", "Could not parse:\n"+s.value, s)
      }
    }

    override def not = new JsonDeepValueMatcher(value) {
      override def apply[S <: Any](s: Expectable[S]) = super.apply(s).negate
    }
    /** select the i'th element after navigation */
    def /#(i: Int) = new JsonSelector(i) {
      override def select(json: JSONType): Option[JSONType] =
        parent.navigate(json).flatMap(j => findDeep(value, j)).flatMap(super.select)
    }
    /** in this case, interpret 'value' as the key and value1 as the expected value in the Array */
    def /(value1: Any) = new JsonValueMatcher(value1) {
      override def navigate(json: JSONType): Option[JSONType] = parent.navigate(json).flatMap(findDeep(value, _))
    }
    /** in this case, interpret 'value' as the key and pair1 as the expected pair in the map */
    def /(pair1: (Any, Any)) = new JsonPairMatcher(pair1._1, pair1._2) {
      override def navigate(json: JSONType): Option[JSONType] = parent.navigate(json).flatMap(findDeep(value, _))
    }
    /** in this case, interpret 'value' as the key and value1 as the expected value in the Array */
    def */(value1: Any) = new JsonDeepValueMatcher(value1) {
      override def navigate(json: JSONType): Option[JSONType] = parent.navigate(json).flatMap(findDeep(value, _))
    }
    /** in this case, interpret 'value' as the key and value1 as the expected pair in the map */
    def */(pair1: (Any, Any)) = new JsonDeepPairMatcher(pair1._1, pair1._2) {
      override def navigate(json: JSONType): Option[JSONType] = parent.navigate(json).flatMap(findDeep(value, _))
    }
  }

  /**
   * @return true if there is one pair is matching the (k, v) pair where k and/or v can be a Regex +
   * an ok message and a ko message
   */
  private def havePair(o: Any, pairs: Seq[(Any, Any)], k: Any, v: Any): (Boolean, String, String) =
    (havePair(pairs, k, v),
     havePairOkMessage(o, pairs, k, v),
     havePairKoMessage(o, pairs, k, v))

  /** @return ok message for the presence of a pair in a map */
  private def havePairOkMessage(o: Any, pairs: Seq[(Any, Any)], k: Any, v: Any) =
    mapString(o)+" contains "+stringOrRegex(k, v)
  /** @return ko message for the presence of a pair in a map */
  private def havePairKoMessage(o: Any, pairs: Seq[(Any, Any)], k: Any, v: Any) =
    mapString(o)+" doesn't contain "+stringOrRegex(k, v)

  /**
   * @return true if there is one pair is matching the (k, v) pair where k and/or v can be a Regex
   */
  private def havePair(pairs: Seq[(Any, Any)], k: Any, v: Any): Boolean =
    findPair(pairs, k, v).nonEmpty

  /**
    * @return true if there is one pair is matching the (k, v) pair where k and/or v can be a Regex
    */
  private def findPair(pairs: Seq[(Any, Any)], k: Any, v: Any): Option[(Any, Any)] =
     pairs.collect { case (key, value) if regexOrEqualMatch(key, k) && regexOrEqualMatch(value, v) => (key, value) }.headOption
  /**
   * @return true if there is one pair is matching the (k, v) pair where k and/or v can be a Regex +
   * an ok message and a ko message
   */
  private def containValue(values: Seq[Any], v: Any): (Boolean, String, String) =
    (values.collect { case value if regexOrEqualMatch(value, v) => v  }.nonEmpty,
      listString(values)+" contains "+stringOrRegex(v),
      listString(values)+" doesn't contain "+stringOrRegex(v))

  /**
   * @return s represented as a Regex if it is one
   */
  private def stringOrRegex(s: Any): String = s match {
    case r: Regex => q(r)+".r"
    case other    => q(other)
  }

  /**
   * @return (k, v) represented as a pair of Regex or String depending on what there types are
   */
  private def stringOrRegex(k: Any, v: Any): String = stringOrRegex(k)+" : "+stringOrRegex(v)

  /**
   * @return true if v is a regex and it matches value or if the 2 are equal
   */
  private def regexOrEqualMatch(value: Any, v: Any) = v match {
    case r: Regex => r matches value.notNull
    case other    => other == value
  }

  private def mapString(o: Any) =
    o match {
      case m: Map[_,_] => m.iterator.map { case (k, v) => k+" : "+v}.mkString("{", ",", "}")
      case other       => other.notNull.remove("\"")
    }

  private def listString(values: Seq[Any]) =
    values.mkString("[", ", ", "]")

}

private[specs2]
trait JsonBaseBeHaveMatchers { outer: JsonBaseMatchers =>

  implicit def toNotMatcherJson(result: NotMatcher[Any]) : NotMatcherJson = new NotMatcherJson(result)
  class NotMatcherJson(result: NotMatcher[Any]) {
    def /(pair: (Any, Any)): JsonPairMatcher = outer./(pair).not
    def */(pair: (Any, Any)): JsonDeepPairMatcher = outer.*/(pair).not
    def /(value: Any): JsonValueMatcher = outer./(value).not
    def */(value: Any): JsonDeepValueMatcher = outer.*/(value).not
  }
}

