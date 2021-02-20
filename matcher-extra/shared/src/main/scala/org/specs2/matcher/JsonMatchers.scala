package org.specs2
package matcher

import org.specs2.data.Sized
import org.specs2.execute.ResultLogicalCombinators.*
import org.specs2.execute.*
import org.specs2.json.*
import Json.*
import org.specs2.text.NotNullStrings.*
import text.NotNullStrings.*
import json.Json.*
import util.matching.Regex
import Matcher.{given}
import Result.*
import Results.negateWhen
import JsonMatchers.*

/**
 * Matchers for Json expressions (entered as strings)
 */
trait JsonMatchers extends Expectations with JsonMatchersImplicits:
  outer =>

  def have(m: Matcher[JsonType]): JsonMatcher = JsonMatcher.create(check = m)
  def /(selector: JsonSelector) : JsonSelectorMatcher = JsonMatcher.create(JsonQuery(First, selector))
  def */(selector: JsonSelector): JsonSelectorMatcher = JsonMatcher.create(JsonQuery(Deep, selector))
  def /#(n: Int)                : JsonSelectorMatcher = JsonMatcher.create(JsonQuery(First, JsonIndexSelector(n)))

  abstract class JsonMatcher extends Matcher[String]:
    def apply[S <: String](s: Expectable[S]) =
      parse(s.value.notNull) match
        case None       => result(negated, "Could not parse\n" + s.value.notNull)
        case Some(json) => negateWhen(negated)(find(Some(json), queries.toList))

    def negate: JsonMatcher
    def negated: Boolean

    protected def queries: Seq[JsonQuery]
    protected def check: Matcher[JsonType]

    def anyValueToJsonType(value: Any): JsonType = value match
      case n if n == null => JsonNull
      case s: String      => JsonString(s)
      case d: Double      => JsonNumber(d)
      case b: Boolean     => JsonBoolean(b)
      case (k: String, v) => JsonMap(Map(k -> v))

    private def find(json: Option[JSONType], queries: List[JsonQuery]): Result =
      def checkRest(value: Any, rest: List[JsonQuery]) =
        (value, rest) match
          case (_, Nil)         => check(createExpectable(anyValueToJsonType(value)))
          case ((k, v), q :: _) =>
            if q.selector.select(Map((k.notNull, v))).isDefined then
              Success()
            else
              Failure(s"found '${value.notNull}' but no value to select for ${q.name}")

          case (v, q :: _) =>
            if q.selector.select(List(v)).isDefined then
              Success()
            else
              Failure(s"found '${value.notNull}' but no value to select for ${q.name}")

      (json, queries) match
        case (None,    Nil)             => Success("ok")
        case (Some(JSONArray(a)), Nil)  => check(createExpectable(JsonType.array(a)))
        case (Some(JSONObject(o)), Nil) => check(createExpectable(JsonType.map(o)))
        case (None,    q :: _)          => Failure(q.selector.name + " not found")

        // FIRST
        case (Some(JSONArray(list)), JsonQuery(First, selector) :: rest) =>
          selector.select(list) match
            case Some(v: JSONType) => find(Some(v), rest)
            case Some(v)           => checkRest(v, rest)
            case None              => selectorNotFound(selector, list)

        case (Some(JSONObject(map)), JsonQuery(First, selector) :: rest) =>
          selector.select(map) match
            case Some((k,v: JSONType)) => find(Some(v), rest)
            case Some((k,v))           => val r = checkRest((k, v), rest); if r.isSuccess then r else checkRest(v, rest)
            case None                  => selectorNotFound(selector, map)

        // DEEP
        case (Some(JSONArray(list)), JsonQuery(Deep, selector) :: rest) =>
          selector.select(list) match
            case Some(v: JSONType) => find(Some(v), rest)
            case Some(v)           => checkRest(v, rest)
            case None    =>
              list.to(LazyList).map {
                case v: JSONType => find(Some(v), queries)
                case v           => checkRest(v, queries)
              }.find(_.isSuccess)
                .getOrElse(selectorNotFound(selector, list))

        case (Some(JSONObject(map)), JsonQuery(Deep, selector) :: rest) =>
          selector.select(map) match
            case Some((k,v: JSONType)) => find(Some(v), rest)
            case Some((k,v))           => checkRest(v, rest)
            case None        =>
              map.values.map {
                case v: JSONType => find(Some(v), queries)
                case v           => checkRest(v, queries)
              }.find(_.isSuccess)
                .getOrElse(selectorNotFound(selector, map))

    private def selectorNotFound[K, V](selector: JsonSelector, map: Map[K, V]) =
      selector match
        case JsonPairSelector(_, _) | JsonValueOrKeySelector(_) =>
          Failure(s"the object\n${showMap(map)}\ndoesn't contain the ${selector.description}")

        case _ =>
          Failure(s"the object\n${showMap(map)}\ndoesn't contain the ${selector.description}\nThis selector can " +
            s"only be used with an array. Use /(k -> anyValue) if you just want to find the key 'k'")

    private def selectorNotFound[T](selector: JsonSelector, list: List[T]) =
      Failure(s"the array\n${showList(list)}\ndoesn't contain the ${selector.description}")

    private def showMap[K, V](map: Map[K, V]) =
      map.map { case (k,v) => s"$k:$v" }.mkString("{",", ","}")

    private def showList[T](list: List[T]) =
      list.mkString("[",", ","]")

  /**
   * This matcher can be chained to select further elements in the Json object
   */
  case class JsonSelectorMatcher(queries: Seq[JsonQuery], negated: Boolean = false) extends JsonMatcher:
    parent =>
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
      previous match
        case start :+ JsonQuery(qt, s: JsonValueSelector) => start :+ JsonQuery(qt, s.toValueOrKey) :+ other
        case _ => previous :+ other

    /**
     * allow the last query to select a value or a key
     */
    private def selectAnyValueLast(queries: Seq[JsonQuery]) =
      queries match
        case start :+ JsonQuery(qt, s) => start :+ JsonQuery(qt, s.toValueOrKey)
        case _ => queries

  /**
   * This matcher can not be chained anymore with selections
   */
  case class JsonFinalMatcher(queries: Seq[JsonQuery], check: Matcher[JsonType], negated: Boolean = false) extends JsonMatcher { parent =>
    def negate = copy(negated = !negated)
  }

  object JsonMatcher:
    def create(path: JsonQuery*): JsonSelectorMatcher = JsonSelectorMatcher(path)
    def create(check: Matcher[JsonType]): JsonFinalMatcher = JsonFinalMatcher(Nil, check)

  def beJsonNull: Matcher[JsonType] =
    new Matcher[JsonType]:
      def apply[S <: JsonType](actual: Expectable[S]) =
        actual.value match
          case JsonNull => result(true, s"the value is not null")
          case other    => result(false, s"$other is not a null value")

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

object JsonType:
  def array(array: List[Any]) = JsonArray(array)
  def map(map: Map[String, Any]) = JsonMap(map)

  val anyMatch = new  Matcher[JsonType] {
    def apply[S <: JsonType](s: Expectable[S]) = result(true, "ko")
  }

  given Sized[JsonType] with
    def size(json: JsonType): Int = json match
      case JsonArray(list) => list.size
      case JsonMap(map)    => map.size
      case JsonString(_)   => 1
      case JsonNumber(_)   => 1
      case JsonBoolean(_)  => 1
      case JsonNull        => 0

  given Conversion[ContainWithResultSeq[String], Matcher[JsonType]] with
    def apply(m: ContainWithResultSeq[String]): Matcher[JsonType] =
      (actual: JsonType) => actual match
        case JsonArray(list) => m(createExpectable(list.map(showJson)))
        case JsonMap(map)    => m(createExpectable(map.toList.map(showJson)))
        case other           => result(false, s"$other is not an array")

  given Conversion[ContainWithResult[String], Matcher[JsonType]] with
    def apply(m: ContainWithResult[String]): Matcher[JsonType] =
     (actual: JsonType) => actual match
       case JsonArray(list) => m(createExpectable(list.map(showJson)))
       case JsonMap(map)    => m(createExpectable(map.toList.map(showJson)))
       case other           => result(false, s"$other is not an array")

  given Conversion[Int, Matcher[JsonType]] with
    def apply(expected: Int): Matcher[JsonType] =
      (actual: JsonType) => actual match
        case JsonNumber(n) => (n.toDouble == expected.toDouble, s"$n is not equal to $expected")
        case other         => (false, s"not a Number: $other")

  given Conversion[Double, Matcher[JsonType]] with
    def apply(expected: Double): Matcher[JsonType] =
      (actual: JsonType) => actual match
        case JsonNumber(n) => (n.toDouble == expected.toDouble, s"$n is not equal to $expected")
        case other         => (false, s"not a Number: $other")

  given Conversion[BigDecimal, Matcher[JsonType]] with
    def apply(expected: BigDecimal): Matcher[JsonType] =
      (actual: JsonType) => actual match
        case JsonNumber(n) => (n.toDouble == expected.toDouble, s"$n is not equal to $expected")
        case other         => (false, s"not a Number: $other")

  given Conversion[Boolean, Matcher[JsonType]] with
    def apply(expected: Boolean): Matcher[JsonType] =
      (actual: JsonType) => actual match
        case JsonBoolean(b) => (b == expected, s"$b is not equal to $expected")
        case other          => (false, s"$other is not a Boolean")

  given Conversion[String, Matcher[JsonType]] with
    def apply(expected: String): Matcher[JsonType] =
      (actual: JsonType) => actual match
        case JsonString(a) => (a == expected, s"$a is not equal to $expected")
        case other         => (false, s"not a String: $other")


/**
 * Different ways of selecting elements in a Json object
 */
trait JsonSelectors:
  sealed trait JsonSelector:
    def select(list: List[Any]): Option[Any]
    def select(map: Map[String, Any]): Option[(String, Any)]
    def name: String
    def description: String

    def toValueOrKey = JsonValueOrKeySelector(this)

  trait JsonValueSelector extends JsonSelector

  case class JsonEqualValueSelector(v: Any) extends JsonValueSelector:
    def select(names: List[Any]) = names.find(_.notNull == v.notNull)
    def select(map: Map[String, Any]) = None
    def name = s"'${v.notNull}'"
    def description: String = s"value $name"

  case class JsonIntSelector(n: Int) extends JsonValueSelector:
    def select(values: List[Any]) =
      values.find {
        case d: Double => d.toInt == n
        case i: Int    => i == n
        case other     => false
      }
    def select(map: Map[String, Any]) = None
    def name = n.toString
    def description: String = s"value $name"

  case class JsonDoubleSelector(d: Double) extends JsonValueSelector:
    def select(values: List[Any]) =
      values.find {
        case db: Double => db == d
        case i: Int     => i == d
        case other     => false
      }

    def select(map: Map[String, Any]) = None
    def name = d.toString
    def description: String = s"value $name"

  case class JsonIndexSelector(n: Int) extends JsonSelector:
    def select(names: List[Any]) = names.zipWithIndex.find { case (_, i) => i == n }.map(_._1)
    def select(map: Map[String, Any]) = map.zipWithIndex.find { case (_, i) => i == n }.map(_._1)
    def name = s"index $n'"
    def description: String = s"value $name"

  case class JsonRegexSelector(r: Regex) extends JsonValueSelector:
    def select(names: List[Any]) = names.find(_.notNull `matches` r.toString).map(_.notNull)
    def select(map: Map[String, Any]) = None
    def name = s"'$r'"
    def description: String = s"regex $name"

  case class JsonMatcherSelector(m: Matcher[String]) extends JsonValueSelector:
    def select(names: List[Any]) = names.find(n => m(createExpectable(n.notNull)).isSuccess)
    def select(map: Map[String, Any]) = None
    def name = "matcher"
    def description: String = s"specified $name"

  case class JsonPairSelector(_1: JsonSelector, _2: JsonSelector) extends JsonSelector:
    def select(list: List[Any]): Option[Any] = None
    def select(map: Map[String, Any]): Option[(String, Any)] =
      _1.select(map.keys.toList).flatMap(k => map.find { case (k1, v) => k.notNull == k1 && _2.select(List(v)).isDefined })
    def name = s"${_1.name}:${_2.description}"
    def description: String = s"pair $name"

  case class JsonValueOrKeySelector(selector: JsonSelector) extends JsonSelector:
    def select(list: List[Any]): Option[Any] = selector.select(list)
    def select(map: Map[String, Any]): Option[(String, Any)] =
      selector.select(map.keys.toList).flatMap(k => map.find { case (k1, v) => k.notNull == k1 })

    def name = selector.name
    def description: String = s"selector ${selector.description}"

  sealed trait JsonQueryType
  case object First extends JsonQueryType
  case object Deep extends JsonQueryType

  case class JsonQuery(query: JsonQueryType, selector: JsonSelector):
    def name = selector.name

  val anyValue: Matcher[Any] =
    new AlwaysMatcher[Any]

private[specs2]
trait JsonMatchersImplicits extends JsonMatchersLowImplicits:

  /** datatype to specify how json values must be checked */
  given [M <: Matcher[String]]: Conversion[M, JsonSelector] with
    def apply(m: M): JsonSelector =
      JsonMatcherSelector(m)

  given Conversion[String, JsonSelector] with
    def apply(s: String): JsonSelector =
      JsonEqualValueSelector(s)

  given Conversion[Regex, JsonSelector] with
    def apply(r: Regex): JsonSelector =
      JsonRegexSelector(r)

  given Conversion[Double, JsonSelector] with
    def apply(d: Double): JsonSelector =
      JsonDoubleSelector(d)

  given Conversion[Int, JsonSelector] with
    def apply(i: Int): JsonSelector =
      JsonIntSelector(i)

  given Conversion[Boolean, JsonSelector] with
    def apply(b: Boolean): JsonSelector =
      JsonEqualValueSelector(b.toString)

  given ToJsonSelector[Regex] with
    def toJsonSelector(r: Regex): JsonSelector = r

  given [M <: Matcher[String]]: ToJsonSelector[M] with
    def toJsonSelector(m: M): JsonSelector = m

  given ToJsonSelector[Matcher[String]] with
    def toJsonSelector(m: Matcher[String]): JsonSelector = m

  object ToJsonSelector:
    def apply[T : ToJsonSelector](t: T) = summon[ToJsonSelector[T]].toJsonSelector(t)

  given [K : ToJsonSelector, V : ToJsonSelector]: Conversion[(K, V), JsonPairSelector] with
    def apply(kv: (K, V)): JsonPairSelector =
      JsonPairSelector(summon[ToJsonSelector[K]].toJsonSelector(kv._1), summon[ToJsonSelector[V]].toJsonSelector(kv._2))

private[specs2]
trait JsonMatchersLowImplicits extends JsonSelectors:
  trait ToJsonSelector[T]:
    def toJsonSelector(t: T): JsonSelector

  given ToJsonSelector[String] with
    def toJsonSelector(a: String): JsonSelector =
      JsonEqualValueSelector(a)

  given ToJsonSelector[Double] with
    def toJsonSelector(a: Double): JsonSelector =
      JsonDoubleSelector(a)

  given ToJsonSelector[Int] with
    def toJsonSelector(a: Int): JsonSelector =
      JsonIntSelector(a)

  given ToJsonSelector[Boolean] with
    def toJsonSelector(a: Boolean): JsonSelector =
      JsonEqualValueSelector(a.toString)

object JsonMatchers extends JsonMatchers
