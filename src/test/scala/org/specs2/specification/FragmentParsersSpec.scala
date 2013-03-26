package org.specs2
package specification

import execute.FailureException

class FragmentParsersSpec extends Specification with RegexFragmentParsers with Grouped { def is =
                                                                                                    s2"""

 Delimited parsers can be used to extract values from specifications and create Steps.

 The defaul delimiters are `{}`
   extract one value ${g1.e1}
   extract 2 values ${g1.e2}
   extract a Seq of values ${g1.e3}
   extracting more values than the converting function is ok ${g1.e4}
   extracting less values than the converting function is an error ${g1.e5}

 It is possible to use other delimiters like `[]`
   by passing a new regular expression directly to the parser ${g1.e1}
   by specifying another implicit regular expression ${g2.e2}

 Regular expression can also be used to extract values ${g3.e1}

 Extracted values
   lead to the creation of `Steps` ${g4.e1}
   if they are used to create expectations this creates `Examples` ${g4.e2}

                                                                                                     """
  "{} delimiters" - new g1 {
   e1 := FragmentParser((_:String).toInt).parse("a value {1}") === 1
   e2 := FragmentParser((s1: String, s2: String) => (s1.toInt, s2.toInt)).parse("2 values {1} and {2}") === (1, 2)
   e3 := FragmentParser((seq: Seq[String]) => seq.map(_.toInt).sum).parse("values {1}, {2}, {3}") === 6
   e4 := FragmentParser((s1: String, s2: String) => (s1.toInt, s2.toInt)).parse("3 values {1} and {2} and {3}") === (1, 2)
   e5 := FragmentParser((s1: String, s2: String) => (s1.toInt, s2.toInt)).parse("1 value {1}") must throwA[FailureException]
  }
  "[] delimiters" - new g2 {
    e1 := extract((_:String).toInt).withRegex("""\[([^\]]+)\]""".r).parse("a value [1]") === 1
    e2 := {
      implicit val fragmentParserRegex = """\[([^\]]+)\]""".r
      extract((_:String).toInt).parse("a value {1}") === 1
    }
  }
  "Regular expressions" - new g3 {
    e1 := {
      val parser = ("abc".r ~> "\\d+".r <~ "rest") ^^ { case digits => digits.toInt }
      extract(parser).parse("abc 12345 rest") === 12345
    }
  }

  val anInt = extract((_:String).toInt)
  val fs1 = (s2""" text {1} $anInt """)
  val fs2 = (s2""" text {1} ${anInt { i => i must be_>(0) }} """)

  "Fragments creation" - new g4 {
    e1 := fs1.fragments.map(_.toString) must containMatch("Step")
    e2 := fs2.fragments.map(_.toString) must containMatch("Example")
  }

}
