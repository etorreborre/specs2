package org.specs2
package specification

import matcher._
import control.ImplicitParameters
import io.Output
import reporter.SilentConsoleReporter
import main.Arguments
import text.RegexExtractor

class RegexExtractorsSpec extends Specification with ResultMatchers with DataTables with Grouped with GivenWhenThen { def is = s2"""

 Given/When/Then specifications can be written by adding extractors after Text fragments
   A Given[T] extractor extracts the text from the previous Text fragment
     returning an object having the type T                                                                              ${g1.e1}
     or a Failure if any expectation fails                                                                              ${g1.e2}
     or a Skipped if any Skipped is thrown                                                                              ${g1.e3}
     or a Failure if the values could not be extracted                                                                  ${g1.e4}
     or an Error if any exception occurred                                                                              ${g1.e5}

   A When[T,S] extractor extracts the text from the previous Text fragment, combines it with the current State
     returning an object having the type S                                                                              ${g2.e1}

   A Then[T] extractor extracts the text from the previous Text fragment, and combines it with the current State
     returning a Result                                                                                                 ${g3.e1}

   A Given[Y] extractor can be used as a Given[X] step if Y <: X, with an implicit conversion                           ${g4.e1}
   A Then[X] extractor can be used as a Then[Y] step if Y <: X, with an implicit conversion                             ${g4.e2}
   A When[P, Q] extractor can be used as a When[R, S] step if P <: R and Q >: S, with an implicit conversion            ${g4.e3}

   Variables delimiters must be removed from descriptions                                                               ${g5.e1}
   Two Given steps can be followed by a When step where the input of the When step pairs the Given outputs              ${g6.e1}
   Two Given steps can be followed by a Then step where the input of the Then step pairs the Given outputs              ${g6.e2}
   Several Given steps can be followed by a When step where the input of the When step tuples the Given outputs         ${g6.e3}

   Factory methods can be used to create Given/When/Then steps from simple functions
     Given
       with a function                                                                                                  ${g7.e1}
       with a function, using the full string when there's no delimited variable                                        ${g7.e2}
       with a regular expression for parsing the whole text and a function                                              ${g7.e3}
       with a regular expression for grouping elements and a function                                                   ${g7.e4}
       with a function returning a MatchResult[T]                                                                       ${g7.e5}

     When
       with a function                                                                                                  ${g8.e1}
       with a regular expression for parsing the whole text and a function                                              ${g8.e2}
       with a regular expression for grouping elements and a function                                                   ${g8.e3}
       with a function returning a MatchResult[T]                                                                       ${g8.e4}

     Then
       with a function                                                                                                  ${g9.e1}
       with a regular expression for parsing the whole text and a function                                              ${g9.e2}
       with a regular expression for grouping elements and a function                                                   ${g9.e3}

   A G/W/T specification must have a title                                                                              ${g10.e1}
   A G/W/T specification can use Before/After Example traits                                                            ${g11.e1}
   A G/W/T specification can use simple contexts                                                                        ${g11.e2}
                                                                                                                        """


  "given" - new g1 {
    e1 := number1.extractContext("Given the following number: ${3}") must beRight(3)
    e2 := number1WithFailure.extractContext("") must beLeft.like { case e => e must beFailing }
    e3 := number1WithSkipped.extractContext("") must beLeft.like { case e => e must beSkipped }
    e4 := number1WithMatchError.extractContext("${1} and 2") must beLeft.like { case e => e must beFailing(message = "\\Qcouldn't extract 2 variables from: ${1} and 2\\E") }
    e5 := number1.extractContext("Given the following number: ${x}") must beLeft.like { case e => e must beError }
  }

  "when" - new g2 {
    e1 := number2.extractContext(Right(1), "And a second number: ${2}") must beRight((1, 2))
  }

  "then" - new g3 {
    e1 := equalToLast.extractContext(Right(1), "Then it is ${1}") must beRight.like { case (s, r) => r must beSuccessful }
  }

  "conversions" - new g4 {
    trait X; trait Y extends X { override def toString = "Y"}
    trait P; trait R extends P
    trait S; trait Q extends S { override def toString = "Q"}

    e1 := {
      val givenY = new Given[Y] { def extract(s: String) = new Y {} }
      val givenX: Given[X] = givenY
      givenX.extract("").toString must_== "Y"
    }

    e2 := {
      val thenX = new Then[X] { def extract(x: X, s: String) = success }
      val thenY: Then[Y] = thenX
      thenY.extract(new Y {}, "") must beSuccessful
    }

    e3 := {
      val whenPQ = new When[P, Q] { def extract(p: P, s: String) = new Q {} }
      val whenRS: When[R, S] = whenPQ
      whenRS.extract(new R {}, "").toString must_== "Q"
    }
  }

  "stripping values" - new g5 {
    e1 := {
        "string"                       || "result"                           |>
        "${abc}"                       !! "abc"                              |
        "${abc\\def}"                  !! "abc\\def"                         |
        { (toStrip, result) => RegexExtractor.strip(toStrip) === result }
    }
  }


  "combinations" - new g6 {
    e1 := {
      "with number ${0}"   ^ number0 ^
      "and number ${1}"    ^ number1 ^
      "when adding"        ^ when0and1 ^
      "the result is ${1}" ^ then0plus1; ok
    }

    e2 := {
      "with number ${0}"   ^ number0 ^
      "and number ${1}"    ^ number1 ^
      "the result is ${1}" ^ then0and1; ok
    }

    e3 := {
      "with number ${0}"   ^ number0 ^
      "and number ${1}"    ^ number1 ^
      "and number ${2}"    ^ number1 ^
      "and number ${3}"    ^ number1 ^
      "when adding"        ^ when0to3 ^
      "the result is ${6}" ^ then0to3; ok
    }
  }

  "factory methods for Given" - new g7 {
    e1 := {
      val number0: Given[Int] = { (s1: String, s2: String) => s1.toInt + s2.toInt }
      number0.extract("Two numbers ${1} and ${2}") === 3
    }
    e2 := {
      val number0: Given[Int] = { (s1: String) => s1.size }
      number0.extract("hello") === 5
    }
    e3 := {
      val number0 = readAs(".*?(\\d+).*(\\d+).*") and { (s1: String, s2: String) => s1.toInt + s2.toInt }
      number0.extract("Two numbers 1 and 2") === 3
    }
    e4 := {
      val number0 = groupAs("\\d+") and { (s1: String, s2: String) => s1.toInt + s2.toInt }
      number0.extract("Two numbers 1 and 2") === 3
    }
    e5 := {
      val number0: Given[Int] = { (s1: String) => (s1.size ==== 5) }
      number0.extract("hello") === 5
    }
  }
  "factory methods for When" - new g8 {
    e1 := {
      val number0: When[Int, (Int, Int)] = { n1: Int => (s2: String) => (n1,  s2.toInt) }
      number0.extract(1, "with one more number ${2}") === (1, 2)
    }
    e2 := {
      val number0 = readAs(".*?(\\d+).*(\\d+).*") and { n1: Int => (s1: String, s2: String) => (n1, s1.toInt + s2.toInt) }
      number0.extract(1, "Two numbers 2 and 3") === (1, 5)
    }
    e3 := {
      val number0 = groupAs("\\d+") and { n1: Int => (s1: String, s2: String) => (n1, s1.toInt + s2.toInt) }
      number0.extract(1, "Two numbers 1 and 2") === (1, 3)
    }
    e4 := {
      val number0 = groupAs("\\d+") and { n1: Int => (s1: String, s2: String) => (n1, s1.toInt + s2.toInt) must not(beNull) }
      number0.extract(1, "Two numbers 1 and 2") === (1, 3)
    }
  }
  "factory methods for Given" - new g9 {
    e1 := {
      val number0: Then[Int] = { n1: Int => (s2: String) => n1 + s2.toInt === 3 }
      number0.extract(1, "with one more number ${2}") must beSuccessful
    }
    e2 := {
      val number0 = readAs(".*?(\\d+).*(\\d+).*") andThen { n1: Int => (s1: String, s2: String) => n1 + s1.toInt + s2.toInt === 6 }
      number0.extract(1, "Two numbers 2 and 3") must beSuccessful
    }
    e3 := {
      val number0 = groupAs("\\d+") andThen { n1: Int => (s1: String, s2: String) => n1 + s1.toInt + s2.toInt === 4 }
      number0.extract(1, "Two numbers 1 and 2") must beSuccessful
    }
  }

  "title" - new g10 {
    e1 := { new Specification { def is = "a number ${0}" ^ number0 }.content.specName.title must not beEmpty }
  }

  "contexts" - new g11 {
    e1 := {
      val spec = new Specification with BeforeExample with io.StringOutput { def is =
        "a number ${0}" ^ number0 ^ "then it is ${0}" ^ then0to3
        def before { println("before") }
      }
      SilentConsoleReporter.report(spec)(Arguments())
      spec.messages must contain("before")
    }
    e2 := {
      val spec = new Specification with io.StringOutput { def is =
        "a number ${0}" ^ number0 ^ "then it is ${0}" ^ then0
        val then0: Then[Int] = (i: Int) => (s: String) => context { 0 === 0 }
        val context = new Before { def before { println("before") } }
      }
      SilentConsoleReporter.report(spec)(Arguments())
      spec.messages must contain("before")
    }
  }

  object number0 extends Given[Int] {
    def extract(text: String): Int = extract1(text).toInt
  }
  object number1 extends Given[Int] {
    def extract(text: String): Int = extract1(text).toInt
  }
  object number1WithMatchError extends Given[Int] {
    def extract(text: String): Int = { val (s1, s2) = extract2(text); s1.toInt+s2.toInt }
  }
  object number1WithFailure extends Given[Int] with ThrownExpectations {
    def extract(text: String): Int = { failure("xxx"); extract1(text).toInt }
  }
  object number1WithSkipped extends Given[Int] with ThrownExpectations {
    def extract(text: String): Int = { skipped("ooo"); extract1(text).toInt }
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