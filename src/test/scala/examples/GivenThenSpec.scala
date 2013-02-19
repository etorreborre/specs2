package examples

import org.specs2._
import specification._

/**
 * This specification shows that it is possible to write a Given/When/Then specification and omit the When step
 */
class GivenThenSpec extends Specification { def is =

  "A given-then example for a calculator"                      ^ br^
    "Given the following number: ${1}"                         ^ number1^
    "Then adding 2 to that number must be: ${3}"               ^ adding2^
    "And it should be >: ${0}"                                 ^ greaterThan^
                                                               end

  object number1 extends Given[Addition] {
    def extract(text: String): Addition = Addition(extract1(text).toInt, 2)
  }
  object adding2 extends Then[Addition] {
    def extract(addition: Addition, text: String) = addition.calculate must_== extract1(text).toInt
  }
  object greaterThan extends Then[Addition] {
    def extract(addition: Addition, text: String) = addition.calculate must be_>=(extract1(text).toInt)
  }
  case class Addition(n1: Int, n2: Int) {
    def calculate: Int = n1 + n2
  }
}