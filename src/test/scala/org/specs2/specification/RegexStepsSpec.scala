package org.specs2
package specification
import execute._

class RegexStepsSpec extends SpecificationWithJUnit { def is =

  "Given/When/Then specifications can be written by adding extractors after Text fragments"                             ^
    "A Given[T] extractor extracts the text from the previous Text fragment"                                            ^
      "returning an object having the type T"                                                                           ! given^
      "or an Error if any"                                                                                              ! given2^
                                                                                                                        p^
    "A When[T,S] extractor extracts the text from the previous Text fragment, and combines it with the Given State"     ^
      "returning an object having the type S"                                                                           ! when^
                                                                                                                        end


  def given = ("Given the following number: 3" ^ number1).context().right.toOption must beSome(3)
  def given2 = ("Given the following number: x" ^ number1).context().left.toOption must beSome.like { case Error(_,_) => ok }

  def when = {
    "Given the following number: 3" ^ number1 ^
    "And a second number: 2"        ^ number2
  }.context().right.toOption must beSome((3, 2))

  object number1 extends Given[Int]("Given the following number: (.*)") {
    def extract(text: String): Int = extract1(text).toInt
  }
  object number2 extends When[Int, (Int, Int)]("And a second number: (.*)") {
    def extract(number1: Int, text: String) = (number1, extract1(text).toInt)
  }
  case class Operation(n1: Int, n2: Int, operator: String) {
    def calculate: Int = if (operator == "+") n1 + n2 else n1 * n2
  }
  object operator extends When[(Int, Int), Operation]("When I use this operator: (.*)") {
    def extract(numbers: (Int, Int), text: String) = Operation(numbers._1, numbers._2, extract1(text))
  }
  object result extends Then[Operation]("Then I should get: (.*)") {
    def extract(operation: Operation, text: String) = operation.calculate  must_== extract1(text).toInt
  }
  object greaterThan extends Then[Operation]("And it should be >: (.*)") {
    def extract(operation: Operation, text: String) = operation.calculate  must be_>=(extract1(text).toInt)
  }
}