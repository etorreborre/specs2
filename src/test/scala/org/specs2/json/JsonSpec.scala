package org.specs2
package json

import mutable.Specification
import matcher._
import util.parsing.json._
import org.scalacheck._
import JsonGen._

class JsonSpec extends Specification with ScalaCheck {

  "The pairs of a json document must only have values with a terminal type" ! prop { (json: JSONType) =>
    Json.pairs(json) must haveTerminalValues
  }
  "The values of a json document must only have values with a terminal type" ! prop { (json: JSONType) =>
    Json.values(json) must beTerminalValues
  }
  "The find method returns None if a key is not present at the first level of a document" ! prop { (json: JSONType) =>
    Json.find("xx", json) must beNone
  }
  "The find method returns Some(value) if a key is present at the first level of a document" ! prop { (json: JSONType) =>
    Json.find("key", new JSONObject(Map("key"->json))) must beSome(json)
  }
  "The findDeep method returns Some(value) if a key is present somewhere in a document and points to a JSON object" ! prop { (json: JSONType) =>
    Json.findDeep("a", json) must beSome.iff(json.toString.contains(""""a" : """))
  }
  "The parser must be threadsafe" >> prop { i: Int =>
    (1 to 100).par.map(j => Json.parse("hello world")) must not(throwAn[Exception])
  }

  implicit lazy val jsonParams: Parameters = set(maxSize = 3)

  def isTerminal(a: Any) = a match {
    case JSONArray(_)  => false
    case JSONObject(_) => false
    case other         => true
  }

  def beTerminalValues: Matcher[Seq[Any]] = beTerminalValue.forall
  def beTerminalValue: Matcher[Any] = (isTerminal(_:Any), (_:Any)+" is not terminal")

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