package org.specs2
package specification
package script

import util.matching.Regex
import control.Exceptions._
import text._

/**
 * A StepParser is a function to extract a value of type `T` from a piece of text
 * It can also strip the text from delimiters if any
 */
trait StepParser[T] {
  /** parse some text and extract some well-type value T */
  def parse(text: String): Either[Exception, T]
  /** if the original text contains delimiters to indicate the values to extract, remove them */
  def strip(text: String) = text
}

/**
 * A Delimited step parser uses a delimiter (`{}` by default) to know which string to extract from the text
 */
abstract class DelimitedStepParser[T](protected val regex: Regex = """\{([^}]+)\}""".r) extends StepParser[T] { parent =>
  override def strip(text: String) = RegexExtractor.strip(text, "".r, regex)

  /** use another regex with this parser */
  def withRegex(r: Regex) =
    new DelimitedStepParser[T](r) {
      def parse(text: String) = parent.parse(text)
      override def strip(text: String) = parent.strip(text)
    }
  
  protected def value[T](t: =>T) = trye(t)(identity)

}
class DelimitedStepParser1[T](f: String => T) extends DelimitedStepParser[T] {
  def parse(text: String) = value(f(RegexExtractor.extract1(text, "".r, regex)))
}
class DelimitedStepParser2[T](f: (String, String) => T) extends DelimitedStepParser[T] {
  def parse(text: String) = value(f.tupled(RegexExtractor.extract2(text, "".r, regex)))
}
class DelimitedStepParser3[T](f: (String, String, String) => T) extends DelimitedStepParser[T] {
  def parse(text: String) = value(f.tupled(RegexExtractor.extract3(text, "".r, regex)))
}
class DelimitedStepParser4[T](f: (String, String, String, String) => T) extends DelimitedStepParser[T] {
  def parse(text: String) = value(f.tupled(RegexExtractor.extract4(text, "".r, regex)))
}
class DelimitedStepParser5[T](f: (String, String, String, String, String) => T) extends DelimitedStepParser[T] {
  def parse(text: String) = value(f.tupled(RegexExtractor.extract5(text, "".r, regex)))
}
class DelimitedStepParser6[T](f: (String, String, String, String, String, String) => T) extends DelimitedStepParser[T] {
  def parse(text: String) = value(f.tupled(RegexExtractor.extract6(text, "".r, regex)))
}
class DelimitedStepParser7[T](f: (String, String, String, String, String, String, String) => T) extends DelimitedStepParser[T] {
  def parse(text: String) = value(f.tupled(RegexExtractor.extract7(text, "".r, regex)))
}
class DelimitedStepParser8[T](f: (String, String, String, String, String, String, String, String) => T) extends DelimitedStepParser[T] {
  def parse(text: String) = value(f.tupled(RegexExtractor.extract8(text, "".r, regex)))
}
class DelimitedStepParser9[T](f: (String, String, String, String, String, String, String, String, String) => T) extends DelimitedStepParser[T] {
  def parse(text: String) = value(f.tupled(RegexExtractor.extract9(text, "".r, regex)))
}
class DelimitedStepParser10[T](f: (String, String, String, String, String, String, String, String, String, String) => T) extends DelimitedStepParser[T] {
  def parse(text: String) = value(f.tupled(RegexExtractor.extract10(text, "".r, regex)))
}
class DelimitedStepParserSeq[T](f: Seq[String] => T) extends DelimitedStepParser[T] {
  def parse(text: String) = value(f(RegexExtractor.extractAll(text, "".r, regex)))
}

object StepParser extends StepParsers

