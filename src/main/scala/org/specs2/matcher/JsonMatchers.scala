package org.specs2
package matcher

import scala.util.parsing.json._
import text.Quote._
import text.NotNullStrings._
import text.Regexes._
import text.Trim._
import json.Json._
import util.matching.Regex
import json.Json

/**
 * Matchers for Json expressions (entered as strings)
 */
trait JsonMatchers extends JsonBaseMatchers with JsonBaseBeHaveMatchers

private[specs2]
trait JsonMatchersLowImplicits { this: JsonBaseMatchers =>
  implicit def anyToJSonValueSpec[T]: ToJsonValueSpec[T] = new ToJsonValueSpec[T] {
    def toJsonValueSpec(a: T): JsonValueSpec = JsonEqualValue(a)
  }
}

private[specs2]
trait JsonBaseMatchers extends Expectations with JsonMatchersLowImplicits { outer =>
  /** datatype to specify how json values must be checked */
  implicit def toJsonStringMatcher(m: Matcher[String]): JsonStringMatcher = JsonStringMatcher(m)
  implicit def toJsonValue(s: Any): JsonEqualValue = JsonEqualValue(s)
  implicit def toJsonRegex(r: Regex): JsonRegex = JsonRegex(r)
  trait ToJsonValueSpec[T] {
    def toJsonValueSpec(t: T): JsonValueSpec
  }
  implicit def regexToJSonValueSpec: ToJsonValueSpec[Regex] = new ToJsonValueSpec[Regex] {
    def toJsonValueSpec(r: Regex): JsonValueSpec = r
  }
  implicit def stringMatcherToJSonValueSpec: ToJsonValueSpec[Matcher[String]] = new ToJsonValueSpec[Matcher[String]] {
    def toJsonValueSpec(m: Matcher[String]): JsonValueSpec = m
  }
  object ToJsonValueSpec {
    def apply[T : ToJsonValueSpec](t: T) = implicitly[ToJsonValueSpec[T]].toJsonValueSpec(t)
  }
  implicit def toJsonPairSpec[K : ToJsonValueSpec, V : ToJsonValueSpec](kv: (K, V)): JsonPairSpec = {
    val (key, value) = kv
    JsonPairSpec(ToJsonValueSpec(key), ToJsonValueSpec(value))
  }
  trait JsonValueSpec
  case class JsonEqualValue(v: Any) extends JsonValueSpec
  case class JsonRegex(r: Regex) extends JsonValueSpec
  case class JsonStringMatcher(m: Matcher[String]) extends JsonValueSpec
  case class JsonPairSpec(k: JsonValueSpec, v: JsonValueSpec) {
    def _1 = k; def _2 = v
  }

  /** match if the document contains the value at the top-level */
  def /(value: JsonValueSpec): JsonValueMatcher = new JsonValueMatcher(value)
  /** match if the document contains the value at any level */
  def */(value: JsonValueSpec): JsonDeepValueMatcher = new JsonDeepValueMatcher(value)

  /** match if the document contains the pair at the top level */
  def /(pair: JsonPairSpec): JsonPairMatcher = new JsonPairMatcher(pair._1, pair._2)
  /** match if the document contains the pair at any level */
  def */(pair: JsonPairSpec): JsonDeepPairMatcher = new JsonDeepPairMatcher(pair._1, pair._2)

  /** allow to match on the i-th element of an Array or a Map (0-based) */
  def /#(i: Int) = new JsonSelector(i)
  case class JsonSelector(i: Int, negate: Boolean = false) { parentSelector =>
    protected def select(json: JSONType): Option[JSONType] = json match {
      case JSONObject(map) if i >= 0 && i < map.size  => Some(JSONObject(Map(map.iterator.toSeq(i))))
      case JSONArray(list) if i >= 0 && i < list.size => list(i) match {
        case o: JSONType => Some(o)
        case other       => Some(JSONArray(List(other)))
      }
      case other         => None
    }
    /** negate the next matcher */
    def not: JsonSelector = copy(negate = true)

    def /#(j: Int) = new JsonSelector(j) {
      override def select(json: JSONType): Option[JSONType] = parentSelector.select(json).flatMap(super.select)
    }
    def /(value: JsonValueSpec): JsonValueMatcher = new JsonValueMatcher(value) {
      override def navigate(json: JSONType): Option[JSONType] = select(json)
    }.not(when = negate)
    def */(value: JsonValueSpec): JsonDeepValueMatcher = new JsonDeepValueMatcher(value) {
      override def navigate(json: JSONType): Option[JSONType] = select(json)
    }.not(when = negate)
    def /(pair: JsonPairSpec): JsonPairMatcher = new JsonPairMatcher(pair._1, pair._2) {
      override def navigate(json: JSONType): Option[JSONType] = select(json)
    }.not(when = negate)
    def */(pair: JsonPairSpec): JsonDeepPairMatcher = new JsonDeepPairMatcher(pair._1, pair._2) {
      override def navigate(json: JSONType): Option[JSONType] = select(json)
    }.not(when = negate)
  }

  class JsonPairMatcher(key: JsonValueSpec, value: JsonValueSpec) extends Matcher[String] {
    def navigate(json: JSONType): Option[JSONType] = Some(json)

    def apply[S <: String](s: Expectable[S]) = {
      parse(s.value).map(navigate) match {
        case Some(Some(JSONObject(obj))) => result(havePair(obj, obj.toSeq, key, value), s)
        case Some(Some(JSONArray(list))) => result(false, "ok", list.mkString("[", ", ", "]")+" doesn't contain "+stringOrRegex(key, value), s)
        case Some(None)                  => result(false, "ok", s.value+" is empty", s)
        case None                        => result(false, "ok", "Could not parse:\n"+s.value, s)
      }
    }
    override def not: JsonPairMatcher = new JsonPairMatcher(key, value) {
      override def apply[S <: String](s: Expectable[S]) = super.apply(s).negate
    }
    private[specs2]
    def not(when: Boolean): JsonPairMatcher = if (when) this.not else this
  }

  class JsonValueMatcher(value: JsonValueSpec) extends Matcher[Any] { parent =>
    def navigate(json: JSONType): Option[JSONType] = Some(json)

    def apply[S <: Any](s: Expectable[S]) = {
      parse(s.value.notNull).map(navigate) match {
        case Some(Some(JSONObject(obj))) => result(false, "ok", obj.map(p => p._1+" : "+p._2).mkString("{", ", ", "}")+" doesn't contain "+stringOrRegex(value), s)
        case Some(Some(JSONArray(list))) => result(containValue(list, value), s)
        case Some(None)                  => result(false, "ok", s.value+" is empty", s)
        case None                        => result(false, "ok", "Could not parse:\n"+s.value, s)
      }
    }
    override def not: JsonValueMatcher = new JsonValueMatcher(value) {
      override def apply[S <: Any](s: Expectable[S]) = super.apply(s).negate
    }
    private[specs2]
    def not(when: Boolean): JsonValueMatcher = if (when) this.not else this

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
    def /(value1: JsonValueSpec) = new JsonValueMatcher(value1) {
      override def navigate(json: JSONType): Option[JSONType] = parent.navigate(json).flatMap(find(value, _))
    }
    /** in this case, interpret 'value' as the key and key1/value1 as the expected pair in the Map */
    def /(pair1: JsonPairSpec) = new JsonPairMatcher(pair1._1, pair1._2) {
      override def navigate(json: JSONType): Option[JSONType] = parent.navigate(json).flatMap(find(value, _))
    }
    /** in this case, interpret 'value' as the key and value1 as the expected value in the Array */
    def */(value1: JsonValueSpec) = new JsonDeepValueMatcher(value1) {
      override def navigate(json: JSONType): Option[JSONType] = parent.navigate(json).flatMap(find(value, _))
    }
    /** in this case, interpret 'value' as the key and value1 as the expected pair in the map */
    def */(pair1: JsonPairSpec) = new JsonDeepPairMatcher(pair1._1, pair1._2) {
      override def navigate(json: JSONType): Option[JSONType] = parent.navigate(json).flatMap(find(value, _))
    }
  }
  class JsonDeepPairMatcher(key: JsonValueSpec, value: JsonValueSpec) extends Matcher[String] {
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

    override def not: JsonDeepPairMatcher = new JsonDeepPairMatcher(key, value) {
      override def apply[S <: String](s: Expectable[S]) = super.apply(s).negate
    }
    private[specs2]
    def not(when: Boolean): JsonDeepPairMatcher = if (when) this.not else this

  }
  class JsonDeepValueMatcher(value: JsonValueSpec) extends Matcher[Any] { parent =>
    def navigate(json: JSONType): Option[JSONType] = Some(json)

    def apply[S <: Any](s: Expectable[S]) = {
      parse(s.value.notNull).map(navigate) match {
        case Some(Some(o)) => result(containValue(terminalValues(o), value), s)
        case Some(None)    => result(false, "ok", s.value.notNull+" is empty", s)
        case None          => result(false, "ok", "Could not parse:\n"+s.value, s)
      }
    }

    override def not: JsonDeepValueMatcher = new JsonDeepValueMatcher(value) {
      override def apply[S <: Any](s: Expectable[S]) = super.apply(s).negate
    }
    private[specs2]
    def not(when: Boolean): JsonDeepValueMatcher = if (when) this.not else this

    /** select the i'th element after navigation */
    def /#(i: Int) = new JsonSelector(i) {
      override def select(json: JSONType): Option[JSONType] =
        parent.navigate(json).flatMap(j => findDeep(value, j)).flatMap(super.select)
    }
    /** in this case, interpret 'value' as the key and value1 as the expected value in the Array */
    def /(value1: JsonValueSpec) = new JsonValueMatcher(value1) {
      override def navigate(json: JSONType): Option[JSONType] = parent.navigate(json).flatMap(findDeep(value, _))
    }
    /** in this case, interpret 'value' as the key and pair1 as the expected pair in the map */
    def /(pair1: JsonPairSpec) = new JsonPairMatcher(pair1._1, pair1._2) {
      override def navigate(json: JSONType): Option[JSONType] = parent.navigate(json).flatMap(findDeep(value, _))
    }
    /** in this case, interpret 'value' as the key and value1 as the expected value in the Array */
    def */(value1: JsonValueSpec) = new JsonDeepValueMatcher(value1) {
      override def navigate(json: JSONType): Option[JSONType] = parent.navigate(json).flatMap(findDeep(value, _))
    }
    /** in this case, interpret 'value' as the key and value1 as the expected pair in the map */
    def */(pair1: JsonPairSpec) = new JsonDeepPairMatcher(pair1._1, pair1._2) {
      override def navigate(json: JSONType): Option[JSONType] = parent.navigate(json).flatMap(findDeep(value, _))
    }
  }

  /**
   * @return true if there is one pair is matching the (k, v) pair where k and/or v can be a Regex +
   * an ok message and a ko message
   */
  private def havePair(o: Any, pairs: Seq[(Any, Any)], k: JsonValueSpec, v: JsonValueSpec): (Boolean, String, String) =
    (havePair(pairs, k, v),
     havePairOkMessage(o, pairs, k, v),
     havePairKoMessage(o, pairs, k, v))

  /** @return ok message for the presence of a pair in a map */
  private def havePairOkMessage(o: Any, pairs: Seq[(Any, Any)], k: JsonValueSpec, v: JsonValueSpec) =
    mapString(o)+" contains "+stringOrRegex(k, v)
  /** @return ko message for the presence of a pair in a map */
  private def havePairKoMessage(o: Any, pairs: Seq[(Any, Any)], k: JsonValueSpec, v: JsonValueSpec) =
    mapString(o)+" doesn't contain "+stringOrRegex(k, v)

  /**
   * @return true if there is one pair is matching the (k, v) pair where k and/or v can be a Regex
   */
  private def havePair(pairs: Seq[(Any, Any)], k: JsonValueSpec, v: JsonValueSpec): Boolean =
    findPair(pairs, k, v).nonEmpty

  /**
    * @return true if there is one pair is matching the (k, v) pair where k and/or v can be a Regex
    */
  private def findPair(pairs: Seq[(Any, Any)], k: JsonValueSpec, v: JsonValueSpec): Option[(Any, Any)] =
     pairs.collect { case (key, value) if regexOrEqualMatch(key, k) && regexOrEqualMatch(value, v) => (key, value) }.headOption

  /**
   * @return find with a Regex or a String or a String matcher
   */
  private def find(key: JsonValueSpec, json: JSONType): Option[JSONType] = key match {
    case JsonEqualValue(v)    => Json.find(v.notNull, json)
    case JsonRegex(r)         => Json.find(r, json)
    case JsonStringMatcher(m) => Json.find((s: String) => m(Expectable(s)).isSuccess, json)
  }

  /**
   * @return findDeep with a Regex or a String or a String matcher
   */
  def findDeep(key: JsonValueSpec, json: JSONType): Option[JSONType] = key match {
    case JsonEqualValue(v)    => Json.findDeep(v.notNull, json)
    case JsonRegex(r)         => Json.findDeep(r, json)
    case JsonStringMatcher(m) => Json.findDeep((s: Any) => m(Expectable(s.notNull)).isSuccess, json)
  }

  /**
   * @return true if there is one pair is matching the (k, v) pair where k and/or v can be a Regex +
   * an ok message and a ko message
   */
  private def containValue(values: Seq[Any], v: JsonValueSpec): (Boolean, String, String) =
    (values.collect { case value if regexOrEqualMatch(value, v) => v  }.nonEmpty,
      listString(values)+" contains "+stringOrRegex(v),
      listString(values)+" doesn't contain "+stringOrRegex(v))

  /**
   * @return s represented as a Regex if it is one
   */
  private def stringOrRegex(s: JsonValueSpec): String = s match {
    case JsonRegex(r)         => q(r)+".r"
    case JsonStringMatcher(_) => "the specified matcher"
    case JsonEqualValue(v)    => q(v)
  }

  /**
   * @return (k, v) represented as a pair of Regex or String depending on what there types are
   */
  private def stringOrRegex(k: JsonValueSpec, v: JsonValueSpec): String = stringOrRegex(k)+" : "+stringOrRegex(v)

  /**
   * @return true if v is a regex and it matches value or if the 2 are equal
   */
  private def regexOrEqualMatch(value: Any, v: JsonValueSpec) = v match {
    case JsonRegex(r)          => r matches value.notNull
    case JsonStringMatcher(m)  => m.apply(Expectable(value.notNull)).isSuccess
    case JsonEqualValue(other) => other == value
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
    def /#(i: Int): JsonSelector = outer./#(i).not
    def /(pair: JsonPairSpec): JsonPairMatcher = outer./(pair).not
    def */(pair: JsonPairSpec): JsonDeepPairMatcher = outer.*/(pair).not
    def /(value: JsonValueSpec): JsonValueMatcher = outer./(value).not
    def */(value: JsonValueSpec): JsonDeepValueMatcher = outer.*/(value).not
  }
}

