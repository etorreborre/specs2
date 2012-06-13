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
    "Given the following number: ${1}"                         ^ aNumber^
    "And a second number: ${2}"                                ^ aNumber^
    "And a third number: ${6}"                                 ^ aNumber^
    "When I use this operator: ${+}"                           ^ operator^
    "Then I should get: ${9}"                                  ^ result^
    "And it should be >: ${0}"                                 ^ greaterThan^
                                                               endp^
  "Now with the multiplication"                                ^ br^
    "Given the following number: ${4}"                         ^ aNumber^
    "And a second number: ${5}"                                ^ aNumber^
    "And a third number: ${6}"                                 ^ aNumber^
    "When I use this operator: ${*}"                           ^ operator^
    "Then I should get: ${120}"                                ^ result^
    "And it should be >: ${10}"                                ^ greaterThan^
    "But not should be >: ${500}"                              ^ lowerThan^
                                                               end


  val aNumber: Given[Int] = (_:String).toInt

  // when there are too many Given[T, S] consecutive steps, it is possible to follow them with a When[Seq[T], S]
  val operator: When[Seq[Int], Operation] = (numbers: Seq[Int]) => (s: String) => Operation(numbers, s)

  val result: Then[Operation]      = (operation: Operation) => (s: String) => { operation.calculate  must_== s.toInt }
  val greaterThan: Then[Operation] = (operation: Operation) => (s: String) => { operation.calculate  must be_>= (s.toInt) }
  val lowerThan: Then[Operation]   = (operation: Operation) => (s: String) => { operation.calculate  must be_<= (s.toInt) }

  case class Operation(numbers: Seq[Int], operator: String) {
    def calculate: Int = if (operator == "+") numbers.sum else numbers.product
  }
}