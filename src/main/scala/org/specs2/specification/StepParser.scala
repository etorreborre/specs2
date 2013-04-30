package org.specs2
package specification

import util.matching.Regex
import control.Exceptions._
import control.ImplicitParameters

trait StepParser[T] {
  /** parse some text and extract some well-type value T */
  def parse(text: String): Either[String, T]
  /** if the original text contains delimiters to indicate the values to extract, remove them */
  def strip(text: String) = text
}

abstract class DelimitedStepParser[T](protected val regex: Regex = """\{([^}]+)\}""".r) extends StepParser[T] { parent =>
  override def strip(text: String) = RegexStep.strip(text, "".r, regex)

  /** use another regex with this parser */
  def withRegex(r: Regex) =
    new DelimitedStepParser[T](r) {
      def parse(text: String) = parent.parse(text)
      override def strip(text: String) = parent.strip(text)
    }
}
class DelimitedStepParser1[T](f: String => T) extends DelimitedStepParser[T] {
  def parse(text: String) = trye(f(RegexStep.extract1(text, "".r, regex)))(_.getMessage)
}
class DelimitedStepParser2[T](f: (String, String) => T) extends DelimitedStepParser[T] {
  def parse(text: String) = trye(f.tupled(RegexStep.extract2(text, "".r, regex)))(_.getMessage)
}
class DelimitedStepParser3[T](f: (String, String, String) => T) extends DelimitedStepParser[T] {
  def parse(text: String) = trye(f.tupled(RegexStep.extract3(text, "".r, regex)))(_.getMessage)
}
class DelimitedStepParser4[T](f: (String, String, String, String) => T) extends DelimitedStepParser[T] {
  def parse(text: String) = trye(f.tupled(RegexStep.extract4(text, "".r, regex)))(_.getMessage)
}
class DelimitedStepParser5[T](f: (String, String, String, String, String) => T) extends DelimitedStepParser[T] {
  def parse(text: String) = trye(f.tupled(RegexStep.extract5(text, "".r, regex)))(_.getMessage)
}
class DelimitedStepParser6[T](f: (String, String, String, String, String, String) => T) extends DelimitedStepParser[T] {
  def parse(text: String) = trye(f.tupled(RegexStep.extract6(text, "".r, regex)))(_.getMessage)
}
class DelimitedStepParser7[T](f: (String, String, String, String, String, String, String) => T) extends DelimitedStepParser[T] {
  def parse(text: String) = trye(f.tupled(RegexStep.extract7(text, "".r, regex)))(_.getMessage)
}
class DelimitedStepParser8[T](f: (String, String, String, String, String, String, String, String) => T) extends DelimitedStepParser[T] {
  def parse(text: String) = trye(f.tupled(RegexStep.extract8(text, "".r, regex)))(_.getMessage)
}
class DelimitedStepParser9[T](f: (String, String, String, String, String, String, String, String, String) => T) extends DelimitedStepParser[T] {
  def parse(text: String) = trye(f.tupled(RegexStep.extract9(text, "".r, regex)))(_.getMessage)
}
class DelimitedStepParser10[T](f: (String, String, String, String, String, String, String, String, String, String) => T) extends DelimitedStepParser[T] {
  def parse(text: String) = trye(f.tupled(RegexStep.extract10(text, "".r, regex)))(_.getMessage)
}
class DelimitedStepParserSeq[T](f: Seq[String] => T) extends DelimitedStepParser[T] {
  def parse(text: String) = trye(f(RegexStep.extractAll(text, "".r, regex)))(_.getMessage)
}

object StepParser extends StepParsers

