package org.specs2
package specification

class StepParsersSpec extends Specification with GWT with Grouped { def is = s2""" ${"Step parsers".title}


 Delimited parsers can be used to extract values from specifications

 The defaul delimiters are `{}`
   one value can be extracted with a function with one argument                       ${g1.e1}
   two values can be extracted with a function with two arguments                     ${g1.e2}
   a sequence of values can be extracted with a function taking a Seq of values       ${g1.e3}
   it is possible to extract more values than the converting function                 ${g1.e4}
   however extracting less values than the converting function returns an error       ${g1.e5}

 It is possible to use other delimiters like `[]`
   by passing a new regular expression directly to the parser                         ${g1.e1}
   by specifying another implicit regular expression                                  ${g2.e2}
                                                                                         """

  "{} delimiters" - new g1 {
   e1 := StepParser((_:String).toInt).parse("a value {1}") === Right(1)
   e2 := StepParser((s1: String, s2: String) => (s1.toInt, s2.toInt)).parse("2 values {1} and {2}") === Right((1, 2))
   e3 := StepParser((seq: Seq[String]) => seq.map(_.toInt).sum).parse("values {1}, {2}, {3}") === Right(6)
   e4 := StepParser((s1: String, s2: String) => (s1.toInt, s2.toInt)).parse("3 values {1} and {2} and {3}") === Right((1, 2))
   e5 := StepParser((s1: String, s2: String) => (s1.toInt, s2.toInt)).parse("1 value {1}") must beLeft
  }

  "[] delimiters" - new g2 {
    e1 := StepParser((_:String).toInt).withRegex("""\[([^\]]+)\]""".r).parse("a value [1]") === Right(1)
    e2 := {
      implicit val stepParserRegex = """\[([^\]]+)\]""".r
      StepParser((_:String).toInt).parse("a value {1}") === Right(1)
    }
  }

  lazy val anInt = StepParser((_:String).toInt)

}
