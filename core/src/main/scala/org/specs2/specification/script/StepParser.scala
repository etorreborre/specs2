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
  /** 
   * parse some text and extract some well-type value T
   * if the original text contains delimiters to indicate the values to extract, remove them
   */
  def parse(text: String): Either[Throwable, (String, T)]
  def strip(text: String): String
}

/**
 * A Delimited step parser uses a delimiter (`{}` by default) to know which string to extract from the text
 */
abstract class DelimitedStepParser[T](regex: Regex) extends StepParser[T] {
  def parse(text: String) = trye((strip(text), parse1(text)))(identity)
  def strip(text: String) = RegexExtractor.strip(text, "".r, regex)

  /** use another regex with this parser */
  def withRegex(r: Regex): DelimitedStepParser[T]
  
  protected def parse1(text: String): T
}

class DelimitedStepParser1[T](f: String => T, regex: Regex = StepParsers.stepParserRegex) extends DelimitedStepParser[T](regex) {
  def parse1(text: String) = f(RegexExtractor.extract1(text, "".r, regex))
  def withRegex(r: Regex) = new DelimitedStepParser1(f, r)
}
class DelimitedStepParser2[T](f: (String, String) => T, regex: Regex = StepParsers.stepParserRegex) extends DelimitedStepParser[T](regex) {
  def parse1(text: String) = f.tupled(RegexExtractor.extract2(text, "".r, regex))
  def withRegex(r: Regex) = new DelimitedStepParser2(f, r)
}
class DelimitedStepParser3[T](f: (String, String, String) => T, regex: Regex = StepParsers.stepParserRegex) extends DelimitedStepParser[T](regex) {
  def parse1(text: String) = f.tupled(RegexExtractor.extract3(text, "".r, regex))
  def withRegex(r: Regex) = new DelimitedStepParser3(f, r)
}
class DelimitedStepParser4[T](f: (String, String, String, String) => T, regex: Regex = StepParsers.stepParserRegex) extends DelimitedStepParser[T](regex) {
  def parse1(text: String) = f.tupled(RegexExtractor.extract4(text, "".r, regex))
  def withRegex(r: Regex) = new DelimitedStepParser4(f, r)
}
class DelimitedStepParser5[T](f: (String, String, String, String, String) => T, regex: Regex = StepParsers.stepParserRegex) extends DelimitedStepParser[T](regex) {
  def parse1(text: String) = f.tupled(RegexExtractor.extract5(text, "".r, regex))
  def withRegex(r: Regex) = new DelimitedStepParser5(f, r)
}
class DelimitedStepParser6[T](f: (String, String, String, String, String, String) => T, regex: Regex = StepParsers.stepParserRegex) extends DelimitedStepParser[T](regex) {
  def parse1(text: String) = f.tupled(RegexExtractor.extract6(text, "".r, regex))
  def withRegex(r: Regex) = new DelimitedStepParser6(f, r)
}
class DelimitedStepParser7[T](f: (String, String, String, String, String, String, String) => T, regex: Regex = StepParsers.stepParserRegex) extends DelimitedStepParser[T](regex) {
  def parse1(text: String) = f.tupled(RegexExtractor.extract7(text, "".r, regex))
  def withRegex(r: Regex) = new DelimitedStepParser7(f, r)
}
class DelimitedStepParser8[T](f: (String, String, String, String, String, String, String, String) => T, regex: Regex = StepParsers.stepParserRegex) extends DelimitedStepParser[T](regex) {
  def parse1(text: String) = f.tupled(RegexExtractor.extract8(text, "".r, regex))
  def withRegex(r: Regex) = new DelimitedStepParser8(f, r)
}
class DelimitedStepParser9[T](f: (String, String, String, String, String, String, String, String, String) => T, regex: Regex = StepParsers.stepParserRegex) extends DelimitedStepParser[T](regex) {
  def parse1(text: String) = f.tupled(RegexExtractor.extract9(text, "".r, regex))
  def withRegex(r: Regex) = new DelimitedStepParser9(f, r)
}
class DelimitedStepParser10[T](f: (String, String, String, String, String, String, String, String, String, String) => T, regex: Regex = StepParsers.stepParserRegex) extends DelimitedStepParser[T](regex) {
  def parse1(text: String) = f.tupled(RegexExtractor.extract10(text, "".r, regex))
  def withRegex(r: Regex) = new DelimitedStepParser10(f, r)
}
class DelimitedStepParserSeq[T](f: Seq[String] => T, regex: Regex = StepParsers.stepParserRegex) extends DelimitedStepParser[T](regex) {
  def parse1(text: String) = f(RegexExtractor.extractAll(text, "".r, regex))
  def withRegex(r: Regex) = new DelimitedStepParserSeq(f, r)
}

object StepParser extends StepParsers

