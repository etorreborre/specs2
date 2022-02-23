package org.specs2
package json

import mutable.Specification
import org.scalacheck.*
import JsonGen.{given}
import Json.*

class JsonSpec extends Specification with ScalaCheck:

  "showJson must be robust against null values" >> prop { (json: JSONType) =>
    parseEither(showJson(json)) must beRight
  }.set(maxSize = 10); br

  "double quotes can be parsed" >> {
    parseEither(raw"""{"a": "hello\"world"}""") must beRight
  }

import Gen.*

/** Generator of JSONType objects with a given tree depth
  */
trait JsonGen:
  given Arbitrary[JSONType] = Arbitrary { sized(depth => jsonType(depth)) }

  def jsonType(depth: Int): Gen[JSONType] = oneOf(jsonArray(depth), jsonObject(depth))

  def jsonArray(depth: Int): Gen[JSONArray] = for
    n <- choose(1, 4)
    vals <- values(n, depth)
  yield JSONArray(vals)

  def jsonObject(depth: Int): Gen[JSONObject] = for
    n <- choose(1, 4)
    ks <- keys(n)
    vals <- values(n, depth)
  yield JSONObject(Map(ks zip vals*))

  def keys(n: Int) = listOfN(n, oneOf("a", "b", "c"))
  def values(n: Int, depth: Int) = listOfN(n, value(depth))
  def value(depth: Int) = if depth <= 0 then terminalType else oneOf(jsonType(depth - 1), terminalType)
  def terminalType = oneOf(1, 2, "m", "n", "o", null)
object JsonGen extends JsonGen
