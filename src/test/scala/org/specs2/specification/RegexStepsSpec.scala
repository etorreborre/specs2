package org.specs2
package specification
import matcher._
import control.ImplicitParameters

class RegexStepsSpec extends Specification with ResultMatchers with DataTables { def is =

  "Given/When/Then specifications can be written by adding extractors after Text fragments"                             ^
    "A Given[T] extractor extracts the text from the previous Text fragment"                                            ^
      "returning an object having the type T"                                                                           ! given^
      "or an Error if any"                                                                                              ! given2^
                                                                                                                        p^
    "A When[T,S] extractor extracts the text from the previous Text fragment, combines it with the current State"       ^
      "returning an object having the type S"                                                                           ! when^
                                                                                                                        p^
    "A Then[T] extractor extracts the text from the previous Text fragment, and combines it with the current State"     ^
      "returning a Result"                                                                                              ! then^
                                                                                                                        p^
    "A Given[Y] extractor can be used as a Given[X] step if Y <: X, with an implicit conversion"                        ! convert1^
    "A Then[X] extractor can be used as a Then[Y] step if Y <: X, with an implicit conversion"                          ! convert2^
    "A When[P, Q] extractor can be used as a When[R, S] step if P <: R and Q >: S, with an implicit conversion"         ! convert3^
                                                                                                                        endp^
    "Variables delimiters must be removed from descriptions"                                                            ! strip^ endp^
    "Two Given steps can be followed by a When step where the input of the When step pairs the Given outputs"           ^ givens1^ endp^
    "Two Given steps can be followed by a Then step where the input of the Then step pairs the Given outputs"           ^ givens2^ endp^
    "Several Given steps can be followed by a When step where the input of the When step tuples the Given outputs"      ^ givens3^
                                                                                                                        endp^
    "Factory methods can be used to create Given/When/Then steps from simple functions"                                 ^
      "Given"                                                                                                           ^
        "with a function"                                                                                               ! factory.given1^
        "with a regular expression for parsing the whole text and a function"                                           ! factory.given2^
        "with a regular expression for grouping elements and a function"                                                ! factory.given3^ endp^
      "When"                                                                                                            ^
        "with a function"                                                                                               ! factory.when1^
        "with a regular expression for parsing the whole text and a function"                                           ! factory.when2^
        "with a regular expression for grouping elements and a function"                                                ! factory.when3^ endp^
      "Then"                                                                                                            ^
        "with a function"                                                                                               ! factory.then1^
        "with a regular expression for parsing the whole text and a function"                                           ! factory.then2^
        "with a regular expression for grouping elements and a function"                                                ! factory.then3^
                                                                                                                        endp^
    "A G/W/T specification must have a title"                                                                           ! spec1^
                                                                                                                        end


  def given = number1.extractContext("Given the following number: ${3}") must beRight(3)
  def given2 = number1.extractContext("Given the following number: ${x}") must beLeft.like { case e => e must beError }

  def when = number2.extractContext(Right(1), "And a second number: ${2}") must beRight((1, 2))

  def then = equalToLast.extractContext(Right(1), "Then it is ${1}") must beRight.like { case (s, r) => r must beSuccessful }

  def givens1 =
    "with number ${0}"   ^ number0 ^
    "and number ${1}"    ^ number1 ^
    "when adding"        ^ when0and1 ^
    "the result is ${1}" ^ then0plus1

  def givens2 =
    "with number ${0}"   ^ number0 ^
    "and number ${1}"    ^ number1 ^
    "the result is ${1}" ^ then0and1

  def givens3 =
    "with number ${0}"   ^ number0 ^
    "and number ${1}"    ^ number1 ^
    "and number ${2}"    ^ number1 ^
    "and number ${3}"    ^ number1 ^
    "when adding"        ^ when0to3 ^
    "the result is ${6}" ^ then0to3

  trait X; trait Y extends X { override def toString = "Y"}
  trait P; trait R extends P
  trait S; trait Q extends S { override def toString = "Q"}

  def convert1 = {
    val givenY = new Given[Y] { def extract(s: String) = new Y {} }
    val givenX: Given[X] = givenY
    givenX.extract("").toString must_== "Y"
  }

  def convert2 = {
    val thenX = new Then[X] { def extract(x: X, s: String) = success }
    val thenY: Then[Y] = thenX
    thenY.extract(new Y {}, "") must beSuccessful
  }

  def convert3 = {
    val whenPQ = new When[P, Q] { def extract(p: P, s: String) = new Q {} }
    val whenRS: When[R, S] = whenPQ
    whenRS.extract(new R {}, "").toString must_== "Q"
  }

  def strip = {
    "string"                       || "result"                           |>
    "${abc}"                       !! "abc"                              |
    "${abc\\def}"                  !! "abc\\def"                         |
    { (toStrip, result) => RegexStep.strip(toStrip) === result }
  }

  object factory {
    def given1 = {
      val number0: Given[Int] = { (s1: String, s2: String) => s1.toInt + s2.toInt }
      number0.extract("Two numbers ${1} and ${2}") === 3
    }
    def given2 = {
      val number0 = readAs(".*?(\\d+).*(\\d+).*") and { (s1: String, s2: String) => s1.toInt + s2.toInt }
      number0.extract("Two numbers 1 and 2") === 3
    }
    def given3 = {
      val number0 = groupAs("\\d+") and { (s1: String, s2: String) => s1.toInt + s2.toInt }
      number0.extract("Two numbers 1 and 2") === 3
    }
    def when1 = {
      val number0: When[Int, (Int, Int)] = { n1: Int => (s2: String) => (n1,  s2.toInt) }
      number0.extract(1, "with one more number ${2}") === (1, 2)
    }
    def when2 = {
      val number0 = readAs(".*?(\\d+).*(\\d+).*") and { n1: Int => (s1: String, s2: String) => (n1, s1.toInt + s2.toInt) }
      number0.extract(1, "Two numbers 2 and 3") === (1, 5)
    }
    def when3 = {
      val number0 = groupAs("\\d+") and { n1: Int => (s1: String, s2: String) => (n1, s1.toInt + s2.toInt) }
      number0.extract(1, "Two numbers 1 and 2") === (1, 3)
    }
    def then1 = {
      val number0: Then[Int] = { n1: Int => (s2: String) => n1 + s2.toInt === 3 }
      number0.extract(1, "with one more number ${2}") must beSuccessful
    }
    def then2 = {
      val number0 = readAs(".*?(\\d+).*(\\d+).*") then { n1: Int => (s1: String, s2: String) => n1 + s1.toInt + s2.toInt === 6 }
      number0.extract(1, "Two numbers 2 and 3") must beSuccessful
    }
    def then3 = {
      val number0 = groupAs("\\d+") then { n1: Int => (s1: String, s2: String) => n1 + s1.toInt + s2.toInt === 4 }
      number0.extract(1, "Two numbers 1 and 2") must beSuccessful
    }
  }

  def spec1 = new Specification { def is = "a number ${0}" ^ number0 }.content.specName.title must not beEmpty

  object number0 extends Given[Int] {
    def extract(text: String): Int = extract1(text).toInt
  }
  object number1 extends Given[Int] {
    def extract(text: String): Int = extract1(text).toInt
  }
  object number2 extends When[Int, (Int, Int)] {
    def extract(number1: Int, text: String) = (number1, extract1(text).toInt)
  }
  object when0and1 extends When[(Int, Int), Int] {
    def extract(number0and1: (Int, Int), text: String) = {
      val (n0, n1) = number0and1
      n0 + n1
    }
  }
  object when0to3 extends When[(Int, Int, Int, Int), Int] {
    def extract(numbers: (Int, Int, Int, Int), text: String) = {
      val (n0, n1, n2, n3) = numbers
      n0 + n1 + n2 + n3
    }
  }
  object then0to3 extends Then[Int] {
    def extract(number: Int, text: String) = {
      number must_== extract1(text).toInt
    }
  }
  object then0plus1 extends Then[Int] {
    def extract(number0plus1: Int, text: String) = {
      number0plus1 must_== extract1(text).toInt
    }
  }
  object then0and1 extends Then[(Int, Int)] {
    def extract(number0and1: (Int, Int), text: String) = {
      val (n0, n1) = number0and1
      (n0 + n1) must_== extract1(text).toInt
    }
  }
  object equalToLast extends Then[Int] {
    def extract(number: Int, text: String) = number must_== extract1(text).toInt
  }
  case class Operation(n1: Int, n2: Int, operator: String) {
    def calculate: Int = if (operator == "+") n1 + n2 else n1 * n2
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
}