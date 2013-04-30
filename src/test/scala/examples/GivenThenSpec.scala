package examples

import org.specs2._
import specification._
import text.RegexExtractor

/**
 * This specification shows that it is possible to write a Given/When/Then specification and omit the When step
 */
class GivenThenSpec extends Specification with GivenWhenThen { def is =

  "A given-then example for a calculator"                      ^ br^
    "Given the following number: ${1}"                         ^ number1^
    "Then adding 2 to that number must be: ${3}"               ^ adding2^
    "And it should be >: ${0}"                                 ^ greaterThan^
                                                               end

  import RegexExtractor._

  val number1: Given[Addition]    = (text: String) => Addition(extract1(text).toInt, 2)
  val adding2: Then[Addition]     = (addition: Addition) => (text: String) => { addition.calculate must_== extract1(text).toInt }
  val greaterThan: Then[Addition] = (addition: Addition) => (text: String) => { addition.calculate must be_>=(extract1(text).toInt) }

  case class Addition(n1: Int, n2: Int) {
    def calculate: Int = n1 + n2
  }
}