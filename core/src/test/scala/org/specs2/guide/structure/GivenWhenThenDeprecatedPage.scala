package org.specs2.guide.structure

import org.specs2.guide.UserGuidePage

class GivenWhenThenDeprecatedPage extends UserGuidePage { def is = "Given When Then".title ^ """
### Given When Then

_This is the original way of writing Given/When/Then specifications in specs2. While this approach has nice properties: no variables, type-safety, it also has performance issues, so it is recommended to use [this one](org.specs2.guide.GivenWhenThenPage.html) instead._

The Given/When/Then style of writing specifications is supported by interspersing Text fragments, with Given/When/Then `RegexExtractors` which extract meaningful values from the text. Here's an example specification for a simple calculator:

      "A given-when-then example for the addition"                 ^
        "Given the following number: ${1}"                         ^ number1 ^
        "And a second number: ${2}"                                ^ number2 ^
        "Then I should get: ${3}"                                  ^ result ^
                                                                   end

      object number1 extends Given[Int] {
        def extract(text: String): Int = extract1(text).toInt
      }
      case class Addition(n1: Int, n2: Int) {
        def add: Int = n1 + n2
      }
      object number2 extends When[Int, Addition] {
        def extract(number1: Int, text: String) = Addition(number1, extract1(text).toInt)
      }
      object result extends Then[Addition] {
        def extract(addition: Addition, text: String): Result = addition.add must_== extract1(text).toInt
      }

Here's some explanation of the object definitions that support the G/W/T style:

 * `number1` is a `Given` step. It is parametrized with the type `Int` meaning that its `extract` method is supposed to extract an Int from the preceding text. It does so by using the `extract1` inherited method, which parses the text for `${}` expressions and return a tuple (with 1 element here) containing all the values enclosed in `${}`.

 * `number2` is a `When` step. It is paramerized with an `Int`, the result from the previous extraction, and an `Addition` which is the result of extracting the second number and putting the 2 together. In that case the method which must be defined is `extract(Int, String): Addition`.

 * finally the `result` object defines the outcome of the Addition. Its `extract` method takes an `Addition` and the current text to return a `Result`

#### Sequencing

A G/W/T sequence can contain more than just 3 steps. However the compiler will check that:

 * only a `Given[T]` extractor can start a sequence
 * only a `Given[S]`, a `When[T, S]` or a `Then[T]` extractor can follow a `Given[T]` extractor
 * only a `When[T1, T2, S]` or a `Then[T1, T2]` can follow a sequence of `Given[T1], Given[T2]` extractors (up to 8 Given steps, after that types are paired)
 * only a `When[S, U]` extractor or a `Then[S]` can follow a `When[T, S]` extractor
 * only a `Then[S]` can follow a `Then[S]` extractor

To be more concrete, here are a few valid sequences:

 * Given[T] / When[T, S] / Then[S]
 * Given[T] / Given[T2] / Given[T2] / When[T, T1, T2, R] / Then[R]
 * Given[T] / Given[T2] / Given[T3] / Given[T4] / Then[T, T1, T2, T3, T4]
 * Given[T] / Given[T2] / ... / Given[T8] / Then[T, T1, T2, T3, T4, T5, T6, (T7, T8)]
 * Given[T] / When[T, S] / Then[S] / Then[S]
 * Given[T] / Then[T] / Then[T]
 * Given[T] / When[T, S] / When[S, U] / Then[U]

#### Extract methods

The `Given`, `When`, `Then` classes provide several convenience methods to extract strings from the preceding text: the `extract1, extract2,...`
 methods will extracts the values delimited by `${}` for up to 10 values.

#### User regexps

In the original way of declaring Given/When/Then steps, the text is left completely void of markers to extract meaningful values. The user then
 needs to specify a regular expression where groups are used to show where those values are:

      object number1 extends Given[Int]("Given the following number: (.*)") {
        def extract(text: String): Int = extract1(text).toInt
      }

The advantage of using this way is that the text is left in it's pristine form, the drawback is that most of the text is duplicated in 2 places, adding more maintenance burden.

#### Factory methods

There are some factory and implicit conversion methods to create Given/When/Then steps by passing functions and / or regular expressions:

 * convert a function `String... => T` to a `Given[T]` step (*note the use of `and` after `readAs` and `groupAs`*)

        // this assumes that the Int to extract is delimited with ${}
        val number1: Given[Int] = (s: String) => s.toInt
        number1.extract("pay ${100} now") === 100

        // if no variable is present, the whole text is passed to the function
        val number1: Given[Int] = (s: String) => s.size
        number1.extract("all") === 3

        // this uses a regular expression with capturing groups matching the full text
        val number1: Given[Int] = readAs(".*(\d+).*") and { (s: String) => s.toInt }
        number1.extract("pay 100 now") === 100

        // this uses capturing groups directly
        val number1: Given[Int] = groupAs("\d+") and { (s: String) => s.toInt }
        number1.extract("pay 100 now") === 100

        // if the Given step is only side-effecting we can omit the `and` call
        // this simplifies the use of Given steps in Unit Specifications
        val number1: Given[Unit] = groupAs("\d+") { (s: String) => value = s.toInt }

 * convert a function `String... => MatchResult[T]` to a `Given[T]` step, passing the `Expectable` value to the next step

        val number1: Given[Int] = (s: String) => s.toInt must be_>=(10)
        number1.extract("pay ${100} now") === 100

 * convert a function `T => String... => S` to a `When[T, S]` step (*note the use of `and` after `readAs` and `groupAs`*)

        // this assumes that the Int to extract is delimited with ${}
        val number2: When[Int, (Int, Int)] = (n1: Int) => (s: String) => (n1, s.toInt)
        number2.extract(100, "with a discount of ${10}%") === (100, 10)

        // this uses a regular expression with capturing groups matching the full text
        val number2: When[Int, (Int, Int)] = readAs(".*(\d+).*") and { (n1: Int) => (s: String) => (n1, s.toInt) }
        number2.extract(100, "with a discount of 10%") === (100, 10)

        // this uses capturing groups directly
        val number2: When[Int, (Int, Int)] = groupAs("\d+") and { (n1: Int) => (s: String) => (n1, s.toInt) }
        number2.extract(100, "with a discount of 10%") === (100, 10)

 * convert a function `T => String... => MatchResult[S]` to a `When[T, S]` step, passing the `Expectable` value to the next step

        val number2: When[Int, Int] = (n1: Int) => (s: String) => (n1 + s.toInt) must be_>=(n1)
        number2.extract(100, "and add ${10}") === 110

 * convert a function `T => String... => Result` to a `Then[T]` step (*note the use of `then` after `readAs` and `groupAs`*)

        // this assumes that the Int to extract is delimited with ${}
        val number3: Then[(Int, Int)] = (n: (Int, Int)) => (s: String) => discount(n._1, n._2) must_== s.toInt
        number3.extract((100, 10), "the result is ${90}") must beSuccessful

        // this uses a regular expression with capturing groups matching the full text
        val number3: Then[(Int, Int)] = readAs(".*(\d+).*") andThen { (n: (Int, Int)) => (s: String) => discount(n._1, n._2) must_== s.toInt }
        number3.extract((100, 10), "the result is 90") must beSuccessful

        // this uses capturing groups directly
        val number3: Then[(Int, Int)] = groupAs("\d+") andThen { (n: (Int, Int)) => (s: String) => discount(n._1, n._2) must_== s.toInt }
        number3.extract((100, 10), "the result is 90") must beSuccessful

        // if the Then step is only side-effecting we can omit the `then` call
        // this simplifies the use of Then steps in Unit Specifications
        val number3: Then[Unit] = groupAs("\d+") { (s: String) => value must_== s.toInt }

#### G/W/T sequences

Given the rule saying that only a `Then` block can follow another `Then` block you might think that it is not possible to start another G/W/T
sequence in the same specification! Fortunately it is possible by just terminating the first sequence with an `end` fragment:

      "A given-when-then example for the addition"                 ^
        "Given the following number: ${1}"                         ^ number1 ^
        "And a second number: ${2}"                                ^ number2 ^
        "Then I should get: ${3}"                                  ^ addition ^
                                                                   end^
      "A given-when-then example for the multiplication"           ^
        "Given the following number: ${1}"                         ^ number1 ^
        "And a second number: ${2}"                                ^ number2 ^
        "Then I should get: ${2}"                                  ^ multiplication ^
                                                                   end

#### Multiple steps

If there are lots of consecutive `When` steps collecting the same kind of arguments, it will be easier to collect them in a `Seq[T]` rather than a `TupleN[T]`:

      "A given-when-then example for the addition"                 ^
        "Given the following number: ${1}"                         ^ number1 ^
        "And a second number: ${2}"                                ^ number2 ^
        "And a third number: ${3}"                                 ^ number3

      val number1: Given[Int]               = (_:String).toInt
      val number2: When[Int, (Int, Int)]    = (n1: Int) => (s: String) => (n1, s.toInt)
      val number3: When[Seq[Int], Seq[Int]] = (numbers: Seq[Int]) => (s: String) => numbers :+ s.toInt

#### Contexts

There are 2 ways to create [contexts](#Contexts) for G/W/T specifications:

 * create a context object (a `Before` for example) and apply it to the part of the `Then` definition returning a `Result`

         val before = new Before { def before { println("code executed before the step") } }
         val then1: Then[Int] = (i: Int) => (s: String) => before { s.toInt === i }

 * use the `BeforeExample`, `AfterExample`,... traits

         class MySpecification extends Specification with BeforeExample { def is =
           "A given-when-then example for the addition"                 ^
             "Given the following number: ${1}"                         ^ number1 ^
             "And a second number: ${2}"                                ^ number2 ^
             "And a third number: ${3}"                                 ^ number3

           // and so on...

           def before { resetCalculator }
         }

##### ScalaCheck

Once you've created a given G/W/T sequence, you can be tempted to copy and paste it in order to check the same scenario with different values. The trouble with this is the duplication of text which leads to more maintenance down the road.

This can be avoided and even enhanced by using ScalaCheck to generate more values for the same scenario. For the calculator above you could write:

      import org.scalacheck.Gen._
      import specification.gen._

      class GivenWhenThenScalacheckSpec extends Specification with ScalaCheck { def is =

        "A given-when-then example for a calculator"                                   ^
          "Given a first number n1"                                                    ^ number1 ^
          "And a second number n2"                                                     ^ number2 ^
          "When I add them"                                                            ^ add ^
          "Then I should get n1 + n2"                                                  ^ result ^
                                                                                       end

        object number1 extends Given[Int] {
          def extract(text: String) = choose(-10, 10)
        }
        object number2 extends When[Int, (Int, Int)] {
          def extract(number1: Int, text: String) = for { n2 <- choose(-10, 10) } yield (number1, n2)
        }
        object add extends When[(Int, Int), Addition] {
          def extract(numbers: (Int, Int), text: String) = Addition(numbers._1, numbers._2)
        }
        object mult extends When[(Int, Int), Multiplication] {
          def extract(numbers: (Int, Int), text: String) = Multiplication(numbers._1, numbers._2)
        }
        object result extends Then[Addition] {
          def extract(text: String)(implicit op: Arbitrary[Addition]) = {
            check { (op: Addition) => op.calculate must_== op.n1 + op.n2 }
          }
        }
        case class Addition(n1: Int, n2: Int) extends Operation { def calculate: Int = n1 + n2 }
      }

The main differences with a "normal" G/W/T sequence are:

 * the import of step classes from `org.specs2.specification.gen` instead of `org.specs2.specification`
 * the return values from the `extract` methods of the `Given` and `When` steps which must return ScalaCheck generators (cf `number1` and `number2`). For the `add` step there is an implicit conversion transforming any value of type `T` to a `Gen[T]`
 * the use of the ScalaCheck trait to access the `check` function transforming a function to a `org.scalacheck.Prop` and then to a `Result`
 * the `extract` method of the `Then` step takes an implicit `Arbitrary[T]` parameter which is used by the `check` method to create a ScalaCheck property

#### Single step

A `GivenThen` step can be used to extract values from a single piece of text and return a `Result`:

      "given the name: ${eric}, then the age is ${18}" ! new GivenThen {
        def extract(text: String) = {
          val (name, age) = extract2(text)
          age.toInt must_== 18
        }
      }

You can also use the `so` object. This object provides an `apply` method expecting a `PartialFunction` and does the value extraction:

      import org.specs2.specification.so

      "given the name: ${eric}, then the age is ${18}" ! so { case (name: String, age: String) =>
        age.toInt must_== 18
      }

#### Conversions

Given / When / Then steps are invariant in their type parameters. This might be detrimental to reuse. For example, if you've defined a `Then[X]` step to check something about a value of type `X`, it would make sense to reuse the same step with a value of type `Y` when `Y <: X`. In order to do this you can use some implicit conversions which will translate steps between types when it makes sense:

      val thenX = new Then[X] {
        def extract(x: X, s: String) = success // check something about x
      }
      // thenX can be reused as a Then[Y] step because Y <: X
      val thenY: Then[Y] = thenX

#### Unit specification

Given / When / Step can also be used in a unit specification by using the &lt;&lt; operator and local variables:

        "A given-when-then example for a calculator".txt.br

          "Given the following number: ${1}" << { s: String =>
            a = s.toInt
          }
          "And a second number: ${2}" << { s: String =>
            b = s.toInt
          }
          "When I use this operator: ${+}" << { s: String =>
            result = Operation(a, b, s).calculate
          }
          "Then I should get: ${3}" << { s: String =>
            result === s.toInt
          }
          "And it should be > ${0}" << { s: String =>
            result must be_>(s.toInt)
          }

        var a, b, result: Int = 0

        case class Operation(n1: Int, n2: Int, operator: String) {
          def calculate: Int = if (operator == "+") n1 + n2 else n1 * n2
        }

If you want to use your own regular expression parsing, the &lt;&lt; operator also accepts `Given[Unit]` and `Then[Unit]` steps:

        "Given the following number: 1" << readAs(".*(\\d).*") { s: String =>
          a = s.toInt
        }
        "And a second number: 2" << groupAs("\\d") { s: Seq[String] =>
          b = s.head.toInt
        }
        "When I use this operator: +" << groupAs("[\\+\\-]") { s: String =>
          result = Operation(a, b, s).calculate
        }
        "Then I should get: 3" << groupAs("\\d") { s: String =>
          result === s.toInt
        }
        "And it should be > 0" << groupAs("\\d") { s: String =>
          result must be_>(s.toInt)
        }

Similarly, ScalaCheck generator and properties are supported:

        "Given a first number n1" << {
          n1 = choose(-10, 10)
        }
        "And a second number n2" << {
          n2 = choose(-10, 10)
        }
        "When I add them" << {
          operation = Arbitrary {
            for (a1 <- n1; a2 <- n2) yield Addition(a1, a2)
          }
        }
        "Then I should get n1 + n2" << check { (op: Addition) =>
          op.calculate must_== op.n1 + op.n2
        }

        var n1, n2: Gen[Int] = null
        implicit var operation: Arbitrary[Addition] = null
                                                         """
}