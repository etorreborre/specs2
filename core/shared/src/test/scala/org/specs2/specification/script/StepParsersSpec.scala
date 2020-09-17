package org.specs2
package specification
package script

import matcher._
import StepParsers.{given _, _}
import scala.util.matching.Regex

class StepParsersSpec extends Spec with TypedEqual { def is = s2"""


 Delimited parsers can be used to extract values from specifications

 The default delimiters are `{}`
   one value can be extracted with a function with one argument                   $braces1
   two values can be extracted with a function with two arguments                 $braces2
   a sequence of values can be extracted with a function taking a Seq of values   $braces3
   it is possible to extract more values than the converting function             $braces4
   however extracting less values than the converting function returns an error   $braces5

 It is possible to use other delimiters like `[]`
   by passing a new regular expression directly to the parser                     $brackets1
     the stripping must be done with the new regexp                               $brackets2
   by specifying another implicit regular expression                              $brackets3

"""

  def braces1 = StepParser((_:String).toInt).parse("a value {1}") === Right(("a value 1", 1))
  def braces2 = StepParser((s1: String, s2: String) => (s1.toInt, s2.toInt)).parse("2 values {1} and {2}") === Right(("2 values 1 and 2", (1, 2)))
  def braces3 = StepParser.seq((seq: Seq[String]) => seq.map(_.toInt).sum).parse("values {1}, {2}, {3}") === Right(("values 1, 2, 3", 6))
  def braces4 = StepParser((s1: String, s2: String) => (s1.toInt, s2.toInt)).parse("3 values {1} and {2} and {3}") === Right(("3 values 1 and 2 and 3", (1, 2)))
  def braces5 = StepParser((s1: String, s2: String) => (s1.toInt, s2.toInt)).parse("1 value {1}") must beLeft

  def brackets1 = StepParser((_:String).toInt).withRegex("""\[([^\]]+)\]""".r).parse("a value [1]") === Right(("a value 1", 1))
  def brackets2 = StepParser((s: String) => s).withRegex("""\[([^\]]+)\]""".r).parse("a value [{1}]") === Right(("a value {1}", "{1}"))
  def brackets3 =
    implicit val stepParserRegex: Regex =
      """\[([^\]]+)\]""".r

    StepParser((_:String).toInt).parse("a value [1]") === Right(("a value 1", 1))
}
