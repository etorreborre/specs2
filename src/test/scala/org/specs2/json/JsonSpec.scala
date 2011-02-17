package org.specs2
package json

import matcher._
import util.parsing.json._
import org.scalacheck._
import JsonGen._

class JsonSpec extends SpecificationWithJUnit with ScalaCheck {
  implicit val jsonParams = set(maxSize -> 3); def is =
  
  "The pairs of a json document must beEmpty or have values with a terminal type" ! check { (json: JSONType) =>
    Json.pairs(json) must haveTerminalValues
  }^
  "The values of a json document must beEmpty or have values with a terminal type" ! check { (json: JSONType) =>
    Json.values(json) must beTerminalValues
  }

  def beTerminalValues: Matcher[Seq[Any]] = beTerminalValue.forall
  def beTerminalValue = new Matcher[Any] {
    def apply[S <: Any](v: Expectable[S]) = {
      val value = v.value
      result(isTerminal(value), value+" is terminal", value+" is not terminal", v)
    }
    def isTerminal(a: Any) = a match {
      case JSONArray(_)  => false
      case JSONObject(_) => false
      case other         => true
    }
  }
  def haveTerminalValues: Matcher[Seq[(Any, Any)]] = haveTerminalValue.forall
  def haveTerminalValue = beTerminalValue ^^ ((p:(Any, Any)) => p._2)
}

import Gen._

/**
 * Generator of JSONType objects with a given tree depth
 */
trait JsonGen {
  implicit def arbitraryJsonType: Arbitrary[JSONType] = Arbitrary { sized(depth => jsonType(depth)) }

  def jsonType(depth: Int): Gen[JSONType] = oneOf(jsonArray(depth), jsonObject(depth))

  def jsonArray(depth: Int): Gen[JSONArray] = for {
    n    <- choose(1, 4)
    vals <- values(n, depth)
  } yield JSONArray(vals)

  def jsonObject(depth: Int): Gen[JSONObject] = for {
    n    <- choose(1, 4)
    ks   <- keys(n)
    vals <- values(n, depth)
  } yield JSONObject(Map((ks zip vals):_*))

  def keys(n: Int) = listOfN(n, oneOf("a", "b", "c"))
  def values(n: Int, depth: Int) = listOfN(n, value(depth))
  def value(depth: Int) = if (depth == 0) terminalType else oneOf(jsonType(depth - 1), terminalType)
  def terminalType = oneOf(1, 2, "m", "n", "o")
}
object JsonGen extends JsonGen