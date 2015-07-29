package org.specs2
package json

import mutable.Specification
import org.scalacheck._
import JsonGen._
import Json._

class JsonSpec extends Specification with ScalaCheck {

  "showJson must be robust against null values" >> prop { (json: JSONType) =>
    parse(showJson(json)) must beSome
  }.set(maxSize = 10)

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
  } yield JSONObject(Map(ks zip vals:_*))

  def keys(n: Int) = listOfN(n, oneOf("a", "b", "c"))
  def values(n: Int, depth: Int) = listOfN(n, value(depth))
  def value(depth: Int) = if (depth <= 0) terminalType else oneOf(jsonType(depth - 1), terminalType)
  def terminalType = oneOf(1, 2, "m", "n", "o", null)
}
object JsonGen extends JsonGen
