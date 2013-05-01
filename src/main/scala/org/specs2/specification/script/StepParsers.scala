package org.specs2
package specification
package script

import util.matching.Regex
import control.ImplicitParameters
import control.Exceptions._
import text.RegexExtractor
import RegexExtractor._

trait StepParsers extends ImplicitParameters {
  implicit lazy val stepParserRegex = """\{([^}]+)\}""".r

  def apply[T](f: String => T)(implicit fpr: Regex): DelimitedStepParser[T] = new DelimitedStepParser1[T](f).withRegex(fpr)
  def apply[T](f: (String, String) => T)(implicit fpr: Regex): DelimitedStepParser[T] = new DelimitedStepParser2[T](f).withRegex(fpr)
  def apply[T](f: (String, String, String) => T)(implicit fpr: Regex): DelimitedStepParser[T] = new DelimitedStepParser3[T](f).withRegex(fpr)
  def apply[T](f: (String, String, String, String) => T)(implicit  fpr: Regex): DelimitedStepParser[T] = new DelimitedStepParser4[T](f).withRegex(fpr)
  def apply[T](f: (String, String, String, String, String) => T)(implicit fpr: Regex): DelimitedStepParser[T] = new DelimitedStepParser5[T](f).withRegex(fpr)
  def apply[T](f: (String, String, String, String, String, String) => T)(implicit fpr: Regex): DelimitedStepParser[T] = new DelimitedStepParser6[T](f).withRegex(fpr)
  def apply[T](f: (String, String, String, String, String, String, String) => T)(implicit fpr: Regex): DelimitedStepParser[T] = new DelimitedStepParser7[T](f).withRegex(fpr)
  def apply[T](f: (String, String, String, String, String, String, String, String) => T)(implicit fpr: Regex): DelimitedStepParser[T] = new DelimitedStepParser8[T](f).withRegex(fpr)
  def apply[T](f: (String, String, String, String, String, String, String, String, String) => T)(implicit fpr: Regex): DelimitedStepParser[T] = new DelimitedStepParser9[T](f).withRegex(fpr)
  def apply[T](f:(String, String, String, String, String, String, String, String, String, String) => T)(implicit fpr: Regex): DelimitedStepParser[T] = new DelimitedStepParser10[T](f).withRegex(fpr)
  def apply[T](f: Seq[String] => T)(implicit fpr: Regex, p: ImplicitParam): DelimitedStepParser[T] = new DelimitedStepParserSeq[T](f).withRegex(fpr)

  /** factory method to create a Given or a Then element from a regex */
  def readAs(regex: String) = new ReadAs(regex.r)
  /** factory method to create a Given or a Then element from a regex, using a regex denoting groups to extract */
  def groupAs(groupRegex: String) = new ReadAs(groups = s"($groupRegex)".r)

  import RegexExtractor._

  /** This class creates Given or Then extractors from a regular expression and a function */
  class ReadAs(regex: Regex = "".r, groups: Regex = """\{([^}]+)\}""".r) {
    def apply(f: String => Unit) = and[Unit](f)

    def apply(f: (String, String) => Unit) = and[Unit](f)
    def apply(f: (String, String, String) => Unit) = and[Unit](f)
    def apply(f: (String, String, String, String) => Unit) = and[Unit](f)
    def apply(f: (String, String, String, String, String) => Unit) = and[Unit](f)
    def apply(f: (String, String, String, String, String, String) => Unit) = and[Unit](f)
    def apply(f: (String, String, String, String, String, String, String) => Unit) = and[Unit](f)
    def apply(f: (String, String, String, String, String, String, String, String) => Unit) = and[Unit](f)
    def apply(f: (String, String, String, String, String, String, String, String, String) => Unit) = and[Unit](f)
    def apply(f: (String, String, String, String, String, String, String, String, String, String) => Unit) = and[Unit](f)
    def apply(f: Seq[String] => Unit)(implicit p: ImplicitParam) = and[Unit](f)(p,p)

    private def value[T](t: =>T) = trye(t)(identity)

    def and[T](f: String => T) = new StepParser[T] {
      def parse(text: String) = value(f(extract1(text, regex, groups)))
    }
    def and[T](f: (String, String) => T) = new StepParser[T] {
      def parse(text: String) = value(f.tupled(extract2(text, regex, groups)))
    }
    def and[T](f: (String, String, String) => T) = new StepParser[T] {
      def parse(text: String) = value(f.tupled(extract3(text, regex, groups)))
    }
    def and[T](f: (String, String, String, String) => T) = new StepParser[T] {
      def parse(text: String) = value(f.tupled(extract4(text, regex, groups)))
    }
    def and[T](f: (String, String, String, String, String) => T) = new StepParser[T] {
      def parse(text: String) = value(f.tupled(extract5(text, regex, groups)))
    }
    def and[T](f: (String, String, String, String, String, String) => T) = new StepParser[T] {
      def parse(text: String) = value(f.tupled(extract6(text, regex, groups)))
    }
    def and[T](f: (String, String, String, String, String, String, String) => T) = new StepParser[T] {
      def parse(text: String) = value(f.tupled(extract7(text, regex, groups)))
    }
    def and[T](f: (String, String, String, String, String, String, String, String) => T) = new StepParser[T] {
      def parse(text: String) = value(f.tupled(extract8(text, regex, groups)))
    }
    def and[T](f: (String, String, String, String, String, String, String, String, String) => T) = new StepParser[T] {
      def parse(text: String) = value(f.tupled(extract9(text, regex, groups)))
    }
    def and[T](f: (String, String, String, String, String, String, String, String, String, String) => T) = new StepParser[T] {
      def parse(text: String) = value(f.tupled(extract10(text, regex, groups)))
    }
    def and[T](f: Seq[String] => T)(implicit p1: ImplicitParam, p2: ImplicitParam) = new StepParser[T] {
      def parse(text: String)  = value(f(extractAll(text, regex, groups)))
    }
  }
}

trait StandardDelimitedStepParsers extends StepParsers {

  def anInt     = StepParser((_: String).toInt)
  def twoInts   = StepParser((s1: String, s2: String) => (s1.toInt, s2.toInt))
  def threeInts = StepParser((s1: String, s2: String, s3: String) => (s1.toInt, s2.toInt, s3.toInt))

  def aDouble      = StepParser((_: String).toDouble)
  def twoDoubles   = StepParser((s1: String, s2: String) => (s1.toDouble, s2.toDouble))
  def threeDoubles = StepParser((s1: String, s2: String, s3: String) => (s1.toDouble, s2.toDouble, s3.toDouble))

  def aString      = StepParser((s:String) => s)
  def twoStrings   = StepParser((s1:String, s2: String) => (s1, s2))
  def threeStrings = StepParser((s1:String, s2: String, s3: String) => (s1, s2, s3))
}

trait StandardRegexStepParsers extends StepParsers {
  // definitions taken from the JavaTokenParsers trait
  private val wholeNumber = """-?\d+"""
  private val decimalNumber = """(\d+(\.\d*)?|\d*\.\d+)"""
  private val stringLiteral = ("\""+"""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*"""+"\"")

  def anInt     = groupAs(wholeNumber).and((_: String).toInt)
  def twoInts   = groupAs(wholeNumber).and((s1: String, s2: String) => (s1.toInt, s2.toInt))
  def threeInts = groupAs(wholeNumber).and((s1: String, s2: String, s3: String) => (s1.toInt, s2.toInt, s3.toInt))

  def aDouble      = groupAs("-?"+decimalNumber).and((_: String).toDouble)
  def twoDoubles   = groupAs("-?"+decimalNumber).and((s1: String, s2: String) => (s1.toDouble, s2.toDouble))
  def threeDoubles = groupAs("-?"+decimalNumber).and((s1: String, s2: String, s3: String) => (s1.toDouble, s2.toDouble, s3.toDouble))

  def aString      = groupAs(stringLiteral).and((s:String) => s)
  def twoStrings   = groupAs(stringLiteral).and((s1:String, s2: String) => (s1, s2))
  def threeStrings = groupAs(stringLiteral).and((s1:String, s2: String, s3: String) => (s1, s2, s3))
}


