package examples

import org.specs2._
import specification._

/**
 * This specification shows how to use the Given/When/Then style for acceptance specifications.
 *
 * Meaningful values are extracted from the example descriptions and used to define expectations.
 */
class GivenWhenThenSpec extends Specification { def is =

  "A given-when-then example for a calculator"                 ^
    "Given the following number: ${1}"                         ^ number1 ^
    "And a second number: ${2}"                                ^ number2 ^
    "When I use this operator: ${+}"                           ^ operator ^
    "Then I should get: ${3}"                                  ^ result ^
    "And it should be >: ${0}"                                 ^ greaterThan ^
                                                               end^
                                                               p^
  "Now with the multiplication"                                ^
    "Given the following number: ${4}"                         ^ number1 ^
    "And a second number: ${5}"                                ^ number2 ^
    "When I use this operator: ${*}"                           ^ operator ^
    "Then I should get: ${20}"                                 ^ result ^
    "And it should be >: ${10}"                                ^ greaterThan ^
    "But not should be >: ${50}"                               ^ lowerThan ^
                                                               end ^ noindent

  object number1 extends Given[Int] {
    def extract(text: String): Int = extract1(text).toInt
  }
  object number2 extends When[Int, (Int, Int)] {
    def extract(number1: Int, text: String) = (number1, extract1(text).toInt)
  }
  object operator extends When[(Int, Int), Operation] {
    def extract(numbers: (Int, Int), text: String) = Operation(numbers._1, numbers._2, extract1(text))
  }
  object result extends Then[Operation] {
    def extract(operation: Operation, text: String) = operation.calculate  must_== extract1(text).toInt
  }
  object greaterThan extends Then[Operation] {
    def extract(operation: Operation, text: String) = operation.calculate  must be_>=(extract1(text).toInt)
  }
  object lowerThan extends Then[Operation] {
    def extract(operation: Operation, text: String) = operation.calculate  must be_<=(extract1(text).toInt)
  }
  case class Operation(n1: Int, n2: Int, operator: String) {
    def calculate: Int = if (operator == "+") n1 + n2 else n1 * n2
  }
}