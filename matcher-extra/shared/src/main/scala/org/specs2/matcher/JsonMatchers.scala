package org.specs2
package matcher

import org.specs2.data.Sized
import org.specs2.execute.ResultLogicalCombinators._
import org.specs2.execute._
import org.specs2.json._
import Json._
import org.specs2.text.NotNullStrings._
import text.NotNullStrings._
import json.Json._
import util.matching.Regex
import MatchersImplicits._
import Results.negateWhen

/**
 * Matchers for Json expressions (entered as strings)
 */
trait JsonMatchers extends JsonBaseMatchers with JsonBaseBeHaveMatchers

private[specs2]
trait JsonBaseMatchers extends Expectations with JsonMatchersImplicits { outer =>

  def have(m: Matcher[JsonType]): JsonMatcher = JsonMatcher.create(check = m)
  def /(selector: JsonSelector) : JsonSelectorMatcher = JsonMatcher.create(JsonQuery(First, selector))
  def */(selector: JsonSelector): JsonSelectorMatcher = JsonMatcher.create(JsonQuery(Deep, selector))
  def /#(n: Int)                : JsonSelectorMatcher = JsonMatcher.create(JsonQuery(First, JsonIndexSelector(n)))

  abstract class JsonMatcher extends Matcher[String] {
    def apply[S <: String](s: Expectable[S]) = {
      parse(s.value.notNull) match {
        case None       => result(negated, "ok", "Could not parse\n" + s.value.notNull, s)
        case Some(json) => result(negateWhen(negated)(find(Some(json), queries.toList)), s)
      }
    }

    def negate: JsonMatcher
    def negated: Boolean
    protected def queries: Seq[JsonQuery]
    protected def check: Matcher[JsonType]

    def anyValueToJsonType(value: Any): JsonType = value match {
      case n if n == null => JsonNull
      case s: String      => JsonString(s)
      case d: Double      => JsonNumber(d)
      case b: Boolean     => JsonBoolean(b)
      case (k: String, v) => JsonMap(Map(k -> v))
    }

    private def find(json: Option[JSONType], queries: List[JsonQuery]): Result = {
      def checkRest(value: Any, rest: List[JsonQuery]) =
        (value, rest) match {
          case (_, Nil)         => check(Expectable(anyValueToJsonType(value))).toResult
          case ((k, v), q :: _) =>
            if (q.selector.select(Map((k.notNull, v))).isDefined) Success()
            else                                                  Failure(s"found '${value.notNull}' but no value to select for ${q.name}")
          case (v, q :: _) =>
            if (q.selector.select(List(v)).isDefined) Success()
            else                                      Failure(s"found '${value.notNull}' but no value to select for ${q.name}")
        }

      (json, queries) match {
        case (None,    Nil)             => Success("ok")
        case (Some(JSONArray(a)), Nil)  => check(Expectable(JsonType.array(a))).toResult
        case (Some(JSONObject(o)), Nil) => check(Expectable(JsonType.map(o))).toResult
        case (None,    q :: _)          => Failure(q.selector.name + " not found")

        // FIRST
        case (Some(JSONArray(list)), JsonQuery(First, selector) :: rest) =>
          selector.select(list) match {
            case Some(v: JSONType) => find(Some(v), rest)
            case Some(v)           => checkRest(v, rest)
            case None              => selectorNotFound(selector, list)
          }

        case (Some(JSONObject(map)), JsonQuery(First, selector) :: rest) =>
          selector.select(map) match {
            case Some((k,v: JSONType)) => find(Some(v), rest)
            case Some((k,v))           => val r = checkRest((k, v), rest); if (r.isSuccess) r else checkRest(v, rest)
            case None                  => selectorNotFound(selector, map)
          }

        // DEEP
        case (Some(JSONArray(list)), JsonQuery(Deep, selector) :: rest) =>
          selector.select(list) match {
            case Some(v: JSONType) => find(Some(v), rest)
            case Some(v)           => checkRest(v, rest)
            case None    =>
              list.toStream.map {
                case v: JSONType => find(Some(v), queries)
                case v           => checkRest(v, queries)
              }.find(_.isSuccess)
                .getOrElse(selectorNotFound(selector, list))
          }

        case (Some(JSONObject(map)), JsonQuery(Deep, selector) :: rest) =>
          selector.select(map) match {
            case Some((k,v: JSONType)) => find(Some(v), rest)
            case Some((k,v))           => checkRest(v, rest)
            case None        =>
              map.values.map {
                case v: JSONType => find(Some(v), queries)
                case v           => checkRest(v, queries)
              }.find(_.isSuccess)
                .getOrElse(selectorNotFound(selector, map))
          }
      }
    }

    private def selectorNotFound[K, V](selector: JsonSelector, map: Map[K, V]) =
      selector match {
        case JsonPairSelector(_, _) | JsonValueOrKeySelector(_) =>
          Failure(s"the object\n${showMap(map)}\ndoesn't contain the ${selector.description}")

        case _ =>
          Failure(s"the object\n${showMap(map)}\ndoesn't contain the ${selector.description}\nThis selector can " +
            s"only be used with an array. Use /(k -> anyValue) if you just want to find the key 'k'")
      }

    private def selectorNotFound[T](selector: JsonSelector, list: List[T]) =
      Failure(s"the array\n${showList(list)}\ndoesn't contain the ${selector.description}")

    private def showMap[K, V](map: Map[K, V]) =
      map.map { case (k,v) => s"$k:$v" }.mkString("{",", ","}")

    private def showList[T](list: List[T]) =
      list.mkString("[",", ","]")
  }

  /**
   * This matcher can be chained to select further elements in the Json object
   */
  case class JsonSelectorMatcher(queries: Seq[JsonQuery], negated: Boolean = false) extends JsonMatcher { parent =>
    val check: Matcher[JsonType] = JsonType.anyMatch

    def /(selector: JsonSelector) : JsonSelectorMatcher = append(JsonQuery(First, selector))
    def */(selector: JsonSelector): JsonSelectorMatcher = append(JsonQuery(Deep, selector))
    def /#(n: Int)                : JsonSelectorMatcher = append(JsonQuery(First, JsonIndexSelector(n)))

    def /(kv: JsonPairSelector) : JsonSelectorMatcher = parent./(kv._1)./(kv._2)
    def */(kv: JsonPairSelector): JsonSelectorMatcher = parent.*/(kv._1)./(kv._2)

    def andHave(m: Matcher[JsonType]): JsonFinalMatcher = JsonFinalMatcher(selectAnyValueLast(queries), m)

    override def not = negate
    def negate = copy(negated = !negated)

    /** add queries one by one */
    private def append(other: JsonQuery*) =
      copy(queries = other.foldLeft(queries)((res, cur) => appendQuery(res, cur)))

    /**
     * add a new query
     *
     * If the last query selects a value, allow it to select a pair as well
     */
    private def appendQuery(previous: Seq[JsonQuery], other: JsonQuery) =
      previous match {
        case start :+ JsonQuery(qt, s: JsonValueSelector) => start :+ JsonQuery(qt, s.toValueOrKey) :+ other
        case _ => previous :+ other
      }

    /**
     * allow the last query to select a value or a key
     */
    private def selectAnyValueLast(queries: Seq[JsonQuery]) =
      queries match {
        case start :+ JsonQuery(qt, s) => start :+ JsonQuery(qt, s.toValueOrKey)
        case _ => queries
      }
  }

  /**
   * This matcher can not be chained anymore with selections
   */
  case class JsonFinalMatcher(queries: Seq[JsonQuery], check: Matcher[JsonType], negated: Boolean = false) extends JsonMatcher { parent =>
    def negate = copy(negated = !negated)
  }

  object JsonMatcher {
    def create(path: JsonQuery*)         = JsonSelectorMatcher(path)
    def create(check: Matcher[JsonType]) = JsonFinalMatcher(Nil, check)
  }

  def beJsonNull: Matcher[JsonType] = new Matcher[JsonType] {
    def apply[S <: JsonType](actual: Expectable[S]) = {
      actual.value match {
        case JsonNull => result(true, s"the value is null", s"the value is not null", actual)
        case other    => result(false, s"$other is not a null value", s"$other is not a null value", actual)
      }
    }
  }

}

/**
 * abstract JSON types for specs2
 */
sealed trait JsonType
case class JsonArray(list: List[Any]) extends JsonType
case class JsonMap(map: Map[String, Any]) extends JsonType
case class JsonString(s: String) extends JsonType
case class JsonNumber(d: Double) extends JsonType
case class JsonBoolean(b: Boolean) extends JsonType
case object JsonNull extends JsonType

object JsonType {
  def array(array: List[Any]) = JsonArray(array)
  def map(map: Map[String, Any]) = JsonMap(map)

  val anyMatch = new  Matcher[JsonType] {
    def apply[S <: JsonType](s: Expectable[S]) = result(true, "ok", "ko", s)
  }

  implicit def JsonTypeIsSized: Sized[JsonType] = new Sized[JsonType] {
    def size(json: JsonType) = json match {
      case JsonArray(list) => list.size
      case JsonMap(map)    => map.size
      case JsonString(_)   => 1
      case JsonNumber(_)   => 1
      case JsonBoolean(_)  => 1
      case JsonNull        => 0
    }
  }

  implicit def JsonTypeMatcherGenTraversable(m: ContainWithResultSeq[String]): Matcher[JsonType] = (actual: JsonType) => actual match {
    case JsonArray(list) => m(Expectable(list.map(showJson)))
    case JsonMap(map)    => m(Expectable(map.map(showJson)))
    case other           => Matcher.result(false, s"$other is not an array", Expectable(other))
  }

  implicit def JsonTypeMatcherGenTraversable(m: ContainWithResult[String]): Matcher[JsonType] = (actual: JsonType) => actual match {
    case JsonArray(list) => m(Expectable(list.map(showJson)))
    case JsonMap(map)    => m(Expectable(map.map(showJson)))
    case other           => Matcher.result(false, s"$other is not an array", Expectable(other))
  }

  implicit def JsonTypeMatcherInt(expected: Int): Matcher[JsonType] = (actual: JsonType) => actual match {
    case JsonNumber(n) => (n.toDouble == expected.toDouble, s"$n is not equal to $expected")
    case other         => (false, s"not a Number: $other")
  }

  implicit def JsonTypeMatcherDouble(expected: Double): Matcher[JsonType] = (actual: JsonType) => actual match {
    case JsonNumber(n) => (n.toDouble == expected.toDouble, s"$n is not equal to $expected")
    case other         => (false, s"not a Number: $other")
  }

  implicit def JsonTypeMatcherBigDecimal(expected: BigDecimal): Matcher[JsonType] = (actual: JsonType) => actual match {
    case JsonNumber(n) => (n.toDouble == expected.toDouble, s"$n is not equal to $expected")
    case other         => (false, s"not a Number: $other")
  }

  implicit def JsonTypeMatcherBoolean(expected: Boolean): Matcher[JsonType] = (actual: JsonType) => actual match {
    case JsonBoolean(b) => (b == expected, s"$b is not equal to $expected")
    case other          => (false, s"$other is not a Boolean")
  }

  implicit def JsonTypeMatcherString(expected: String): Matcher[JsonType] = (actual: JsonType) => actual match {
    case JsonString(a) => (a == expected, s"$a is not equal to $expected")
    case other         => (false, s"not a String: $other")
  }

}

/**
 * Different ways of selecting elements in a Json object
 */
trait JsonSelectors {
  sealed trait JsonSelector {
    def select(list: List[Any]): Option[Any]
    def select(map: Map[String, Any]): Option[(String, Any)]
    def name: String
    def description: String

    def toValueOrKey = JsonValueOrKeySelector(this)
  }

  trait JsonValueSelector extends JsonSelector

  case class JsonEqualValueSelector(v: Any) extends JsonValueSelector {
    def select(names: List[Any]) = names.find(_.notNull == v.notNull)
    def select(map: Map[String, Any]) = None
    def name = s"'${v.notNull}'"
    def description: String = s"value $name"
  }
  case class JsonIntSelector(n: Int) extends JsonValueSelector {
    def select(values: List[Any]) =
      values.find {
        case d: Double => d.toInt == n
        case i: Int    => i == n
        case other     => false
      }
    def select(map: Map[String, Any]) = None
    def name = n.toString
    def description: String = s"value $name"
  }
  case class JsonDoubleSelector(d: Double) extends JsonValueSelector {
    def select(values: List[Any]) =
      values.find {
        case db: Double => db == d
        case i: Int     => i == d
        case other     => false
      }

    def select(map: Map[String, Any]) = None
    def name = d.toString
    def description: String = s"value $name"
  }
  case class JsonIndexSelector(n: Int) extends JsonSelector {
    def select(names: List[Any]) = names.zipWithIndex.find { case (_, i) => i == n }.map(_._1)
    def select(map: Map[String, Any]) = map.zipWithIndex.find { case (_, i) => i == n }.map(_._1)
    def name = s"index $n'"
    def description: String = s"value $name"
  }
  case class JsonRegexSelector(r: Regex) extends JsonValueSelector {
    def select(names: List[Any]) = names.find(_.notNull matches r.toString).map(_.notNull)
    def select(map: Map[String, Any]) = None
    def name = s"'$r'"
    def description: String = s"regex $name"
  }
  case class JsonMatcherSelector(m: Matcher[String]) extends JsonValueSelector {
    def select(names: List[Any])      = names.find(n => m(Expectable(n.notNull)).isSuccess)
    def select(map: Map[String, Any]) = None
    def name = "matcher"
    def description: String = s"specified $name"
  }
  case class JsonPairSelector(_1: JsonSelector, _2: JsonSelector) extends JsonSelector {
    def select(list: List[Any]): Option[Any] = None
    def select(map: Map[String, Any]): Option[(String, Any)] =
      _1.select(map.keys.toList).flatMap(k => map.find { case (k1, v) => k.notNull == k1 && _2.select(List(v)).isDefined })

    def name = s"${_1.name}:${_2.description}"
    def description: String = s"pair $name"
  }
  case class JsonValueOrKeySelector(selector: JsonSelector) extends JsonSelector {
    def select(list: List[Any]): Option[Any] = selector.select(list)
    def select(map: Map[String, Any]): Option[(String, Any)] =
      selector.select(map.keys.toList).flatMap(k => map.find { case (k1, v) => k.notNull == k1 })

    def name = selector.name
    def description: String = s"selector ${selector.description}"
  }

  sealed trait JsonQueryType
  case object First extends JsonQueryType
  case object Deep extends JsonQueryType

  case class JsonQuery(query: JsonQueryType, selector: JsonSelector) {
    def name = selector.name
  }

  val anyValue: Matcher[Any] =
    new NeutralMatcher[Any]

}

private[specs2]
trait JsonMatchersImplicits extends JsonMatchersLowImplicits { this: JsonBaseMatchers =>
  /** datatype to specify how json values must be checked */
  implicit def toJsonValueSelectorStringMatcher[M <: Matcher[String]](m: M): JsonSelector = JsonMatcherSelector(m)
  implicit def toJsonValueSelectorStringValue(s: String): JsonSelector                    = JsonEqualValueSelector(s)
  implicit def toJsonValueSelectorRegex(r: Regex): JsonSelector                           = JsonRegexSelector(r)
  implicit def toJsonValueSelectorDoubleValue(d: Double): JsonSelector                    = JsonDoubleSelector(d)
  implicit def toJsonValueSelectorIntValue(i: Int): JsonSelector                          = JsonIntSelector(i)
  implicit def toJsonValueSelectorBooleanValue(b: Boolean): JsonSelector                  = JsonEqualValueSelector(b.toString)

  implicit def regexToJsonSelector: ToJsonSelector[Regex] = new ToJsonSelector[Regex] {
    def toJsonSelector(r: Regex): JsonSelector = r
  }
  implicit def matcherToJsonSelector[M <: Matcher[String]]: ToJsonSelector[M] = new ToJsonSelector[M] {
    def toJsonSelector(m: M): JsonSelector = m
  }
  implicit def stringMatcherToJsonSelector: ToJsonSelector[Matcher[String]] = new ToJsonSelector[Matcher[String]] {
    def toJsonSelector(m: Matcher[String]): JsonSelector = m
  }
  object ToJsonSelector {
    def apply[T : ToJsonSelector](t: T) = implicitly[ToJsonSelector[T]].toJsonSelector(t)
  }

  implicit def toJsonSelectorPair[K : ToJsonSelector, V : ToJsonSelector](kv: (K, V)): JsonPairSelector =
    JsonPairSelector(implicitly[ToJsonSelector[K]].toJsonSelector(kv._1), implicitly[ToJsonSelector[V]].toJsonSelector(kv._2))
}

private[specs2]
trait JsonMatchersLowImplicits extends JsonSelectors { this: JsonBaseMatchers =>
  trait ToJsonSelector[T] {
    def toJsonSelector(t: T): JsonSelector
  }

  implicit def stringToJsonSelector: ToJsonSelector[String] = new ToJsonSelector[String] {
    def toJsonSelector(a: String): JsonSelector = JsonEqualValueSelector(a)
  }
  implicit def doubleToJsonSelector: ToJsonSelector[Double] = new ToJsonSelector[Double] {
    def toJsonSelector(a: Double): JsonSelector = JsonDoubleSelector(a)
  }
  implicit def intToJsonSelector: ToJsonSelector[Int] = new ToJsonSelector[Int] {
    def toJsonSelector(a: Int): JsonSelector = JsonIntSelector(a)
  }
  implicit def booleanToJsonSelector: ToJsonSelector[Boolean] = new ToJsonSelector[Boolean] {
    def toJsonSelector(a: Boolean): JsonSelector = JsonEqualValueSelector(a.toString)
  }
}

private[specs2]
trait JsonBaseBeHaveMatchers extends BeHaveMatchers { outer: JsonBaseMatchers =>

  implicit def toNotMatcherJson(result: NotMatcher[Any]) : NotMatcherJson = new NotMatcherJson(result)
  class NotMatcherJson(result: NotMatcher[Any]) {
    def have(m: Matcher[JsonType])   = outer.have(m).negate
    def /#(i: Int)                   = outer./#(i).negate
    def /(selector: JsonSelector)    = outer./(selector).negate
    def */(selector: JsonSelector)   = outer.*/(selector).negate
  }
}
