package examples

import org.specs2._
import specification._

/**
 * This specification shows how to use the Given/When/Then style for acceptance specifications.
 *
 * Meaningful values are extracted from the example descriptions and used to define expectations.
 */
class GivenWhenThenSpec extends Specification { def is =

  "A given-when-then example for a calculator"                 ^ br^
    "Given the following number: ${1}"                         ^ number1^
    "And a second number: ${2}"                                ^ number2^
    "And a third number: ${6}"                                 ^ number3^
    "When I use this operator: ${+}"                           ^ operator^
    "Then I should get: ${9}"                                  ^ result^
    "And it should be >: ${0}"                                 ^ greaterThan^
                                                               endp^
  "Now with the multiplication"                                ^ br^
    "Given the following number: ${4}"                         ^ number1^
    "And a second number: ${5}"                                ^ number2^
    "And a third number: ${6}"                                 ^ number3^
    "When I use this operator: ${*}"                           ^ operator^
    "Then I should get: ${120}"                                ^ result^
    "And it should be >: ${10}"                                ^ greaterThan^
    "But not should be >: ${500}"                              ^ lowerThan^
                                                               end


  val number1: Given[Int]                        = (_:String).toInt
  val number2: When[Int, (Int, Int)]             = (n1: Int) => (s: String) => (n1, s.toInt)
  val number3: When[(Int, Int), (Int, Int, Int)] = (numbers: (Int, Int)) => (s: String) => (numbers._1, numbers._2, s.toInt)

  // when there are too many When[T, S] consecutive steps, it is possible to follow them with a When[Seq[T], S]
  val operator: When[Seq[Int], Operation]        = (numbers: Seq[Int]) => (s: String) => Operation(numbers, s)

  val result: Then[Operation]      = (operation: Operation) => (s: String) => { operation.calculate  must_== s.toInt }
  val greaterThan: Then[Operation] = (operation: Operation) => (s: String) => { operation.calculate  must be_>= (s.toInt) }
  val lowerThan: Then[Operation]   = (operation: Operation) => (s: String) => { operation.calculate  must be_<= (s.toInt) }

  case class Operation(numbers: Seq[Int], operator: String) {
    def calculate: Int = if (operator == "+") numbers.sum else numbers.reduce(_ * _)
  }
}