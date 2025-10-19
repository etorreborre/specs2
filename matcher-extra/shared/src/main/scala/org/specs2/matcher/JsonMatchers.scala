package org.specs2
package matcher

import org.specs2.collection.Sized
import org.specs2.execute.ResultLogicalCombinators.*
import org.specs2.execute.*
import org.specs2.json.*
import org.specs2.text.NotNullStrings.*
import Json.*
import util.matching.Regex
import Matcher.{given}
import Result.*
import Results.negateWhen
import JsonMatchers.*

/** Matchers for Json expressions (entered as strings)
  */
trait JsonMatchers extends Expectations with JsonMatchersImplicits:
  outer =>

  def have(m: Matcher[JsonType]): JsonMatcher = JsonMatcher.create(check = m)
  def /(selector: JsonSelector): JsonSelectorMatcher = JsonMatcher.create(JsonQuery(First, selector))
  def */(selector: JsonSelector): JsonSelectorMatcher = JsonMatcher.create(JsonQuery(Deep, selector))
  def /#(n: Int): JsonSelectorMatcher = JsonMatcher.create(JsonQuery(First, JsonIndexSelector(n)))

  abstract class JsonMatcher extends Matcher[String]:
    def apply[S <: String](s: Expectable[S]) =
      parseEither(s.value.notNull) match
        case Right(json) => negateWhen(negated)(find(Some(json), queries.toList))
        case Left(e)     => result(negated, "Could not parse\n" + s.value.notNull + s"\n$e")

    def negate: JsonMatcher
    def negated: Boolean

    protected def queries: Seq[JsonQuery]
    protected def check: Matcher[JsonType]

    private def anyValueToJsonType(value: Any): JsonType = value.asInstanceOf[Matchable] match
      case n if n == null => JsonNull
      case s: String      => JsonString(s)
      case d: Double      => JsonNumber(d)
      case b: Boolean     => JsonBoolean(b)
      case (k, v)         => JsonMap(Map(k.toString -> v))

    private def find(json: Option[JSONType], queries: List[JsonQuery]): Result =
      def checkRest(value: Any, rest: List[JsonQuery]) =
        (value.asInstanceOf[Matchable], rest.asInstanceOf[Matchable]) match
          case (_, Nil)         => check(createExpectable(anyValueToJsonType(value)))
          case ((k, v), q :: _) =>
            if rest.head.selector.select((k, v)).isDefined then Success()
            else Failure(s"found '${value.notNull}' but no value to select for ${rest.head.name}")

          case (v, q :: _) =>
            if rest.head.selector.select(v).isDefined then Success()
            else Failure(s"found '${value.notNull}' but no value to select for ${rest.head.name}")

      (json, queries) match
        case (None, Nil)                => Success("ok")
        case (Some(JSONArray(a)), Nil)  => check(createExpectable(JsonType.array(a)))
        case (Some(JSONObject(o)), Nil) => check(createExpectable(JsonType.map(o)))
        case (None, q :: _)             => Failure(q.selector.name + " not found")

        // FIRST
        case (Some(JSONArray(list)), JsonQuery(First, selector) :: rest) =>
          selector.select(list) match
            case Some(v) =>
              v.asInstanceOf[Matchable] match
                case j: JSONType => find(Some(j), rest)
                case _           => checkRest(v, rest)
            case _ => selectorNotFound(selector, list)

        case (Some(JSONObject(map)), JsonQuery(First, selector) :: rest) =>
          selector.select(map) match
            case Some((k, v)) =>
              v.asInstanceOf[Matchable] match
                case j: JSONType => find(Some(j), rest)
                case _           => val r = checkRest((k, v), rest); if r.isSuccess then r else checkRest(v, rest)
            case _ =>
              selectorNotFound(selector, map)

        // DEEP
        case (Some(JSONArray(list)), JsonQuery(Deep, selector) :: rest) =>
          selector.select(list) match
            case Some(v) =>
              v.asInstanceOf[Matchable] match
                case j: JSONType => find(Some(j), rest)
                case _           => checkRest(v, rest)
            case _ =>
              list
                .to(LazyList)
                .map { v =>
                  v.asInstanceOf[Matchable] match
                    case j: JSONType => find(Some(j), queries)
                    case _           => checkRest(v, queries)
                }
                .find(_.isSuccess)
                .getOrElse(selectorNotFound(selector, list))

        case (Some(JSONObject(map)), JsonQuery(Deep, selector) :: rest) =>
          selector.select(map) match
            case Some((k, v)) =>
              v.asInstanceOf[Matchable] match
                case j: JSONType => find(Some(j), rest)
                case _           => checkRest(v, rest)
            case _ =>
              map.values
                .map { v =>
                  v.asInstanceOf[Matchable] match
                    case j: JSONType => find(Some(j), queries)
                    case _           => checkRest(v, queries)
                }
                .find(_.isSuccess)
                .getOrElse(selectorNotFound(selector, map))

    private def selectorNotFound[K, V](selector: JsonSelector, map: Map[K, V]) =
      selector match
        case JsonPairSelector(_, _) | JsonValueOrKeySelector(_) =>
          Failure(s"the object\n${showMap(map)}\ndoesn't contain the ${selector.description}")

        case _ =>
          Failure(
            s"the object\n${showMap(map)}\ndoesn't contain the ${selector.description}\nThis selector can " +
              s"only be used with an array. Use /(k -> anyValue) if you just want to find the key 'k'"
          )

    private def selectorNotFound[T](selector: JsonSelector, list: List[T]) =
      Failure(s"the array\n${showList(list)}\ndoesn't contain the ${selector.description}")

    private def showMap[K, V](map: Map[K, V]) =
      map
        .asInstanceOf[Map[K, Matchable]]
        .map {
          case (k, v: String) => s"\"$k\": \"$v\""
          case (k, v)         => s"\"$k\": $v"
        }
        .mkString("{", ", ", "}")

    private def showList(list: List[Any]) =
      list
        .asInstanceOf[List[Matchable]]
        .map {
          case t: String => s"\"$t\""
          case t         => t.toString
        }
        .mkString("[", ", ", "]")

  /** This matcher can be chained to select further elements in the Json object
    */
  case class JsonSelectorMatcher(queries: Seq[JsonQuery], negated: Boolean = false) extends JsonMatcher:
    parent =>
    val check: Matcher[JsonType] = JsonType.anyMatch

    def /(selector: JsonSelector): JsonSelectorMatcher = append(JsonQuery(First, selector))
    def */(selector: JsonSelector): JsonSelectorMatcher = append(JsonQuery(Deep, selector))
    def /#(n: Int): JsonSelectorMatcher = append(JsonQuery(First, JsonIndexSelector(n)))

    def /(kv: JsonPairSelector): JsonSelectorMatcher = parent./(kv._1)./(kv._2)
    def */(kv: JsonPairSelector): JsonSelectorMatcher = parent.*/(kv._1)./(kv._2)

    def andHave(m: Matcher[JsonType]): JsonFinalMatcher = JsonFinalMatcher(selectAnyValueLast(queries), m)

    override def not = negate
    def negate = copy(negated = !negated)

    /** add queries one by one */
    private def append(other: JsonQuery*) =
      copy(queries = other.foldLeft(queries)((res, cur) => appendQuery(res, cur)))

    /** add a new query
      *
      * If the last query selects a value, allow it to select a pair as well
      */
    private def appendQuery(previous: Seq[JsonQuery], other: JsonQuery) =
      previous match
        case start :+ JsonQuery(qt, s: JsonSelector) =>
          start :+ JsonQuery(qt, s.toValueOrKey) :+ other
        case _ =>
          previous :+ other

    /** allow the last query to select a value or a key
      */
    private def selectAnyValueLast(queries: Seq[JsonQuery]) =
      queries match
        case start :+ JsonQuery(qt, s) => start :+ JsonQuery(qt, s.toValueOrKey)
        case _                         => queries

  /** This matcher can not be chained anymore with selections
    */
  case class JsonFinalMatcher(queries: Seq[JsonQuery], check: Matcher[JsonType], negated: Boolean = false)
      extends JsonMatcher { parent =>
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

/** abstract JSON types for specs2
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

  val anyMatch = new Matcher[JsonType] {
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
      (actual: JsonType) =>
        actual match
          case JsonArray(list) => m(createExpectable(list.map(showJson)))
          case JsonMap(map)    => m(createExpectable(map.toList.map(showJson)))
          case other           => result(false, s"$other is not an array")

  given Conversion[ContainWithResult[String], Matcher[JsonType]] with
    def apply(m: ContainWithResult[String]): Matcher[JsonType] =
      (actual: JsonType) =>
        actual match
          case JsonArray(list) => m(createExpectable(list.map(showJson)))
          case JsonMap(map)    => m(createExpectable(map.toList.map(showJson)))
          case other           => result(false, s"$other is not an array")

  given Conversion[Int, Matcher[JsonType]] with
    def apply(expected: Int): Matcher[JsonType] =
      (actual: JsonType) =>
        actual match
          case JsonNumber(n) => (n.toDouble == expected.toDouble, s"$n is not equal to $expected")
          case other         => (false, s"not a Number: $other")

  given Conversion[Double, Matcher[JsonType]] with
    def apply(expected: Double): Matcher[JsonType] =
      (actual: JsonType) =>
        actual match
          case JsonNumber(n) => (n.toDouble == expected.toDouble, s"$n is not equal to $expected")
          case other         => (false, s"not a Number: $other")

  given Conversion[BigDecimal, Matcher[JsonType]] with
    def apply(expected: BigDecimal): Matcher[JsonType] =
      (actual: JsonType) =>
        actual match
          case JsonNumber(n) => (n.toDouble == expected.toDouble, s"$n is not equal to $expected")
          case other         => (false, s"not a Number: $other")

  given Conversion[Boolean, Matcher[JsonType]] with
    def apply(expected: Boolean): Matcher[JsonType] =
      (actual: JsonType) =>
        actual match
          case JsonBoolean(b) => (b == expected, s"$b is not equal to $expected")
          case other          => (false, s"$other is not a Boolean")

  given Conversion[String, Matcher[JsonType]] with
    def apply(expected: String): Matcher[JsonType] =
      (actual: JsonType) =>
        actual match
          case JsonString(a) => (a == expected, s"$a is not equal to $expected")
          case other         => (false, s"not a String: $other")

/** Different ways of selecting elements in a Json object
  */
trait JsonSelectors:
  sealed trait JsonSelector:
    // select a value amongst a list of values
    def select(list: List[Any]): Option[Any]

    // select a value amongst a map of values
    def select(map: Map[String, Any]): Option[(String, Any)]

    // select a value amongst a pair of values
    def select(keyValue: (String, Any)): Option[Any]

    // select a value amongst a single value (String, Double, Int, Boolean, null)
    def select(value: Any): Option[Any]

    // name of the selector used in failure messages
    def name: String

    // full description of the selector used in failure messages
    def description: String

    // return this selector, applicable to either values or keys
    def toValueOrKey: JsonSelector =
      JsonValueOrKeySelector(this)

  case class JsonIntSelector(n: Int) extends JsonSelector:
    def select(values: List[Any]): Option[Any] =
      values.find { v => this.select(v).isDefined }

    def select(map: Map[String, Any]): Option[(String, Any)] =
      None

    def select(keyValue: (String, Any)): Option[Any] =
      this.select(keyValue._2)

    def select(value: Any): Option[Any] =
      value.asInstanceOf[Matchable] match
        case d: Double => if d.toInt == n then Some(value) else None
        case i: Int    => if i == n then Some(value) else None
        case _         => None

    def name: String =
      n.toString

    def description: String =
      s"value $name"

  case class JsonDoubleSelector(d: Double) extends JsonSelector:
    def select(values: List[Any]): Option[Any] =
      values.find { v => this.select(v).isDefined }

    def select(map: Map[String, Any]): Option[(String, Any)] =
      None

    def select(keyValue: (String, Any)): Option[Any] =
      this.select(keyValue._2)

    def select(value: Any): Option[Any] =
      value.asInstanceOf[Matchable] match
        case db: Double => if db == d then Some(value) else None
        case i: Int     => if i == d then Some(value) else None
        case _          => None

    def name: String =
      d.toString

    def description: String =
      s"value $name"

  case class JsonStringSelector(s: String) extends JsonSelector:
    def select(values: List[Any]): Option[Any] =
      values.find { v => this.select(v).isDefined }

    def select(map: Map[String, Any]): Option[(String, Any)] =
      None

    def select(keyValue: (String, Any)): Option[Any] =
      this.select(keyValue._2)

    def select(value: Any): Option[Any] =
      value.asInstanceOf[Matchable] match
        case s1: String => if s == s1 then Some(value) else None
        case _          => None

    def name: String =
      s"\"${s.notNull}\""

    def description: String =
      s"value $name"

  case class JsonBooleanSelector(b: Boolean) extends JsonSelector:
    def select(values: List[Any]): Option[Any] =
      values.find { v => this.select(v).isDefined }

    def select(map: Map[String, Any]): Option[(String, Any)] =
      None

    def select(keyValue: (String, Any)): Option[Any] =
      this.select(keyValue._2)

    def select(value: Any): Option[Any] =
      value.asInstanceOf[Matchable] match
        case b1: Boolean => if b == b1 then Some(value) else None
        case _           => None

    def name: String =
      b.toString

    def description: String =
      s"value $b"

  case class JsonIndexSelector(n: Int) extends JsonSelector:
    def select(names: List[Any]): Option[Any] =
      names.zipWithIndex.find { case (_, i) => i == n }.map(_._1)

    def select(map: Map[String, Any]): Option[(String, Any)] =
      map.zipWithIndex.find { case (_, i) => i == n }.map(_._1)

    def select(keyValue: (String, Any)): Option[Any] =
      None

    def select(value: Any): Option[Any] =
      value.asInstanceOf[Matchable] match
        case l: List[?]   => this.select(l)
        case m: Map[?, ?] => this.select(m)
        case _            => None

    def name: String =
      s"index $n"

    def description: String =
      s"value $name"

  case class JsonRegexSelector(r: Regex) extends JsonSelector:
    def select(names: List[Any]): Option[Any] =
      names.find(_.notNull `matches` r.toString).map(_.notNull)

    def select(map: Map[String, Any]): Option[(String, Any)] =
      None

    def select(keyValue: (String, Any)): Option[Any] =
      None

    def select(value: Any): Option[Any] =
      value.asInstanceOf[Matchable] match
        case s: String => if s.notNull `matches` r.toString then Some(s) else None
        case _         => None

    def name: String =
      s"\"$r\""

    def description: String =
      s"regex $name"

  case class JsonStringMatcherSelector(m: Matcher[String]) extends JsonSelector:
    def select(names: List[Any]): Option[Any] =
      names.find(n => m(createExpectable(n.notNull)).isSuccess)

    def select(map: Map[String, Any]): Option[(String, Any)] =
      None
    def select(keyValue: (String, Any)): Option[Any] =
      None

    def select(value: Any): Option[Any] =
      value.asInstanceOf[Matchable] match
        case s: String => if m(createExpectable(s.notNull)).isSuccess then Some(s) else None
        case _         => None

    def name: String =
      "string matcher"

    def description: String =
      s"specified $name"

  case class JsonDoubleMatcherSelector(m: Matcher[Double]) extends JsonSelector:
    def select(names: List[Any]): Option[Any] =
      None

    def select(map: Map[String, Any]): Option[(String, Any)] =
      None
    def select(keyValue: (String, Any)): Option[Any] =
      None

    def select(value: Any): Option[Any] =
      value.asInstanceOf[Matchable] match
        case d: Double => if (m(createExpectable(d)).isSuccess) Some(d) else None
        case _         => None

    def name: String =
      "double matcher"

    def description: String =
      s"specified $name"

  case class JsonBooleanMatcherSelector(m: Matcher[Boolean]) extends JsonSelector:
    def select(names: List[Any]): Option[Any] =
      None

    def select(map: Map[String, Any]): Option[(String, Any)] =
      None
    def select(keyValue: (String, Any)): Option[Any] =
      None

    def select(value: Any): Option[Any] =
      value.asInstanceOf[Matchable] match
        case b: Boolean => if (m(createExpectable(b)).isSuccess) Some(b) else None
        case _          => None

    def name: String =
      "boolean matcher"

    def description: String =
      s"specified $name"

  // The type parameter for the matcher is irrelevant here since we want to select any value.
  // Unit is chosen instead of Any to avoid ambiguity with other selectors for implicit conversions.
  case class JsonAnyMatcherSelector(m: Matcher[Unit]) extends JsonSelector:
    def select(names: List[Any]): Option[Any] =
      names.headOption

    def select(map: Map[String, Any]): Option[(String, Any)] =
      None
    def select(keyValue: (String, Any)): Option[Any] =
      None

    def select(value: Any): Option[Any] =
      Some(value)

    def name: String =
      "Any matcher"

    def description: String =
      s"specified $name"

  case class JsonPairSelector(_1: JsonSelector, _2: JsonSelector) extends JsonSelector:
    def select(list: List[Any]): Option[Any] =
      None

    def select(map: Map[String, Any]): Option[(String, Any)] =
      _1.select(map.keys.toList)
        .flatMap(k => map.find { case (k1, v) => k.notNull == k1 && _2.select(List(v)).isDefined })

    def select(keyValue: (String, Any)): Option[Any] =
      if _1.select(keyValue._1).isDefined && _2.select(keyValue._2).isDefined
      then Some(keyValue)
      else None

    def select(value: Any): Option[Any] =
      value.asInstanceOf[Matchable] match
        case m: Map[?, ?] => this.select(m)
        case kv: (?, ?)   => this.select(kv)
        case _            => None

    def name: String =
      s"${_1.name}: ${_2.name}"

    def description: String =
      s"pair $name"

  case class JsonValueOrKeySelector(selector: JsonSelector) extends JsonSelector:
    def select(list: List[Any]): Option[Any] =
      selector.select(list)

    def select(map: Map[String, Any]): Option[(String, Any)] =
      selector.select(map.keys.toList).flatMap(k => map.find { case (k1, v) => k.notNull == k1 })

    def select(keyValue: (String, Any)): Option[Any] =
      selector.select(keyValue._1).orElse(selector.select(keyValue._2))

    def select(value: Any): Option[Any] =
      value.asInstanceOf[Matchable] match
        case l: List[?]   => this.select(l)
        case m: Map[?, ?] => this.select(m)
        case kv: (?, ?)   => this.select(kv.asInstanceOf[(String, Any)])
        case _            => None

    def name: String =
      selector.name

    def description: String =
      s"${selector.description}"

  sealed trait JsonQueryType
  case object First extends JsonQueryType
  case object Deep extends JsonQueryType

  case class JsonQuery(query: JsonQueryType, selector: JsonSelector):
    def name: String =
      selector.name

  val anyValue: Matcher[Unit] =
    new AlwaysMatcher[Unit]

private[specs2] trait JsonMatchersImplicits extends JsonMatchersLowImplicits:

  /** datatype to specify how json values must be checked */
  given [M <: Matcher[String]]: Conversion[M, JsonSelector] with
    def apply(m: M): JsonSelector =
      JsonStringMatcherSelector(m)

  given doubleMatcherSelector: Conversion[Matcher[Double], JsonSelector] with
    def apply(m: Matcher[Double]): JsonSelector =
      JsonDoubleMatcherSelector(m)

  given booleanMatcherSelector: Conversion[Matcher[Boolean], JsonSelector] with
    def apply(m: Matcher[Boolean]): JsonSelector =
      JsonBooleanMatcherSelector(m)

  given anyMatcherSelector: Conversion[Matcher[Unit], JsonSelector] with
    def apply(m: Matcher[Unit]): JsonSelector =
      JsonAnyMatcherSelector(m)

  given Conversion[String, JsonSelector] with
    def apply(s: String): JsonSelector =
      JsonStringSelector(s)

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
      JsonBooleanSelector(b)

  given [K, V](using k: Conversion[K, JsonSelector], v: Conversion[V, JsonSelector]): Conversion[
    (K, V),
    JsonPairSelector
  ] with
    def apply(kv: (K, V)): JsonPairSelector =
      JsonPairSelector(k(kv._1), v(kv._2))

private[specs2] trait JsonMatchersLowImplicits extends JsonSelectors:

  trait ToJsonSelector[T]:
    def toJsonSelector(t: T): JsonSelector

//   given ToJsonSelector[String] with
//     def toJsonSelector(a: String): JsonSelector =
//       JsonStringSelector(a)

//   given ToJsonSelector[Double] with
//     def toJsonSelector(a: Double): JsonSelector =
//       JsonDoubleSelector(a)

//   given ToJsonSelector[Int] with
//     def toJsonSelector(a: Int): JsonSelector =
//       JsonIntSelector(a)

//   given ToJsonSelector[Boolean] with
//     def toJsonSelector(b: Boolean): JsonSelector =
//       JsonBooleanSelector(b)

object JsonMatchers extends JsonMatchers
