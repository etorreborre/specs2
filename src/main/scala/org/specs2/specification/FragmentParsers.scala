package org.specs2
package specification

import util.parsing.combinator.RegexParsers
import util.parsing.input.CharSequenceReader
import util.matching._
import execute._
import control.ImplicitParameters
import control.Exceptions._

/**
 * Fragment parsers can be used in interpolated specifications to extract values from the preceding text and
 * create Steps or Examples
 */
trait FragmentParsers extends FragmentParserApply with FragmentParserExtract { outer: Specification =>

  /** a previously extracted value can be used in another Extract definition */
  implicit def extractedValue[T](fp: FragmentParser[T]): T = fp.get

  /** an extracted value can be transformed as a Step or an Example if it has a check function */
  implicit def extractedIsSpecPart[T](fp: FragmentParser[T]): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = {
      val splitted = text.split("\n")
      val previousText = splitted.mkString.trim

      // for an example add 2 spaces for the status sign
      val strippedText =
        (if (fp.isChecked) (splitted.dropRight(1) :+ ("  "+splitted.last)) else splitted).
         map(fp.strip).mkString("\n")

      val result =
        if (fp.isChecked) asResultIsSpecPart(fp.checkResult(previousText)).append(strippedText, expression)
        else              fragmentIsSpecPart(Step(fp.extract(previousText))).append(strippedText, expression)

      fs append result
    }
  }
}

trait StepParser[T] {
  /** parse some text and extract some well-type value T */
  def parse(text: String): Either[String, T]
  /** if the original text contains delimiters to indicate the values to extract, remove them */
  def strip(text: String) = text
}

/**
 * a Fragment parser is responsible for extracting a value from some previous text and, if equipped with a verification
 * function, to check that this value is ok. The value might be a tuple or a sequence
 */
trait FragmentParser[T] extends StepParser[T] { outer =>

  protected def copy(check: Option[T => Result] = None, name: Option[String] = None): FragmentParser[T]

  protected def extractor: Extractor[T]

  def isChecked = extractor.check.isDefined
  def checkResult(text: String): Result = extractor.check.map(_ { extract(text); get }).getOrElse(Success())
  def extract(text: String): Result = extractor.extract(parse(text).right.get)
  def get = extractor.get
  def apply[S : AsResult](check: T => S): FragmentParser[T] = copy(check = Some((t: T) => ResultExecution.execute(t)((t1: T) => AsResult(check(t1)))))
  def andThen[S : AsResult](check: T => S) = apply(check)
  def withName(n: String) = copy(name = Some(n))
}

case class Extractor[T](check: Option[T => Result] = None, name: Option[String] = None, var field: Either[Result, T] = Left(Pending())) {
  private val failureMessage = s"${name.map(n => s"the variable $n was").getOrElse("variable")} not set"

  def extract(parsed: =>T): Result = {
    field = ResultExecution.executeEither(parsed)(identity)
    field match {
      case Left(r)  => r
      case Right(_) => execute.Success()
    }
  }
  def set(c: Option[T => Result] = None, n: Option[String] = None) = Extractor(check.orElse(c), name.orElse(n), field)
  def get = field.right.getOrElse(throw new FailureException(execute.Failure(failureMessage)))
  def apply[S : AsResult](check: T => S) = copy(check = Some((t: T) => ResultExecution.execute(t)((t1: T) => AsResult(check(t1)))))
  def andThen[S : AsResult](check: T => S) = apply(check)
  def withName(n: String) = copy(name = Some(n))
}

abstract class DelimitedVariablesParser[T](val extractor: Extractor[T] = Extractor[T](),
                                           protected val regex: Regex = """\{([^}]+)\}""".r) extends FragmentParser[T] { parent =>
  def parse(text: String): Either[String, T]
  override def strip(text: String) = RegexStep.strip(text, "".r, regex)

  /** use another regex with this parser */
  def withRegex(r: Regex) =
    new DelimitedVariablesParser[T](extractor, r) {
      def parse(text: String) = parent.parse(text)
      override def strip(text: String) = parent.strip(text)
    }

  protected def copy(check: Option[T => Result] = None, name: Option[String] = None): FragmentParser[T] =
    new DelimitedVariablesParser[T](extractor.set(check, name), regex) {
      def parse(text: String) = parent.parse(text)
      override def strip(text: String) = parent.strip(text)
    }
}
class DelimitedVariablesParser1[T](f: String => T) extends DelimitedVariablesParser[T] {
  def parse(text: String) = trye(f(RegexStep.extract1(text, "".r, regex)))(_.getMessage)
}
class DelimitedVariablesParser2[T](f: (String, String) => T) extends DelimitedVariablesParser[T] {
  def parse(text: String) = trye(f.tupled(RegexStep.extract2(text, "".r, regex)))(_.getMessage)
}
class DelimitedVariablesParser3[T](f: (String, String, String) => T) extends DelimitedVariablesParser[T] {
  def parse(text: String) = trye(f.tupled(RegexStep.extract3(text, "".r, regex)))(_.getMessage)
}
class DelimitedVariablesParser4[T](f: (String, String, String, String) => T) extends DelimitedVariablesParser[T] {
  def parse(text: String) = trye(f.tupled(RegexStep.extract4(text, "".r, regex)))(_.getMessage)
}
class DelimitedVariablesParser5[T](f: (String, String, String, String, String) => T) extends DelimitedVariablesParser[T] {
  def parse(text: String) = trye(f.tupled(RegexStep.extract5(text, "".r, regex)))(_.getMessage)
}
class DelimitedVariablesParser6[T](f: (String, String, String, String, String, String) => T) extends DelimitedVariablesParser[T] {
  def parse(text: String) = trye(f.tupled(RegexStep.extract6(text, "".r, regex)))(_.getMessage)
}
class DelimitedVariablesParser7[T](f: (String, String, String, String, String, String, String) => T) extends DelimitedVariablesParser[T] {
  def parse(text: String) = trye(f.tupled(RegexStep.extract7(text, "".r, regex)))(_.getMessage)
}
class DelimitedVariablesParser8[T](f: (String, String, String, String, String, String, String, String) => T) extends DelimitedVariablesParser[T] {
  def parse(text: String) = trye(f.tupled(RegexStep.extract8(text, "".r, regex)))(_.getMessage)
}
class DelimitedVariablesParser9[T](f: (String, String, String, String, String, String, String, String, String) => T) extends DelimitedVariablesParser[T] {
  def parse(text: String) = trye(f.tupled(RegexStep.extract9(text, "".r, regex)))(_.getMessage)
}
class DelimitedVariablesParser10[T](f: (String, String, String, String, String, String, String, String, String, String) => T) extends DelimitedVariablesParser[T] {
  def parse(text: String) = trye(f.tupled(RegexStep.extract10(text, "".r, regex)))(_.getMessage)
}
class DelimitedVariablesParserSeq[T](f: Seq[String] => T) extends DelimitedVariablesParser[T] {
  def parse(text: String) = trye(f(RegexStep.extractAll(text, "".r, regex)))(_.getMessage)
}

trait RegexFragmentParser extends RegexParsers {

  case class RegexFragmentParser[T](p: Parser[T], extractor: Extractor[T] = Extractor[T]()) extends FragmentParser[T] { parent =>
    def parse(text: String) = p.apply(new CharSequenceReader(text, 0)) match {
      case Success(t,_)   => Right(t)
      case Failure(m,_)   => Left(m)
      case Error(m,_)     => Left(m)
    }

    protected def copy(check: Option[T => Result] = None, name: Option[String] = None): FragmentParser[T] =
      new RegexFragmentParser[T](p, extractor.set(check, name))
  }

  implicit def parserToStepParser[T](p: Parser[T]): StepParser[T] = new StepParser[T] {
    def parse(text: String) = p.apply(new CharSequenceReader(text, 0)) match {
      case Success(t,_)   => Right(t)
      case Failure(m,_)   => Left(m)
      case Error(m,_)     => Left(m)
    }
  }
}

object FragmentParser extends FragmentParserApply with FragmentParserExtract
trait FragmentParserApply extends RegexFragmentParser with ImplicitParameters {
  implicit lazy val fragmentParserRegex = """\{([^}]+)\}""".r

  def apply[T](f: String => T)(implicit fpr: Regex): DelimitedVariablesParser[T] = new DelimitedVariablesParser1[T](f).withRegex(fpr)
  def apply[T](f: (String, String) => T)(implicit fpr: Regex): DelimitedVariablesParser[T] = new DelimitedVariablesParser2[T](f).withRegex(fpr)
  def apply[T](f: (String, String, String) => T)(implicit fpr: Regex): DelimitedVariablesParser[T] = new DelimitedVariablesParser3[T](f).withRegex(fpr)
  def apply[T](f: (String, String, String, String) => T)(implicit  fpr: Regex): DelimitedVariablesParser[T] = new DelimitedVariablesParser4[T](f).withRegex(fpr)
  def apply[T](f: (String, String, String, String, String) => T)(implicit fpr: Regex): DelimitedVariablesParser[T] = new DelimitedVariablesParser5[T](f).withRegex(fpr)
  def apply[T](f: (String, String, String, String, String, String) => T)(implicit fpr: Regex): DelimitedVariablesParser[T] = new DelimitedVariablesParser6[T](f).withRegex(fpr)
  def apply[T](f: (String, String, String, String, String, String, String) => T)(implicit fpr: Regex): DelimitedVariablesParser[T] = new DelimitedVariablesParser7[T](f).withRegex(fpr)
  def apply[T](f: (String, String, String, String, String, String, String, String) => T)(implicit fpr: Regex): DelimitedVariablesParser[T] = new DelimitedVariablesParser8[T](f).withRegex(fpr)
  def apply[T](f: (String, String, String, String, String, String, String, String, String) => T)(implicit fpr: Regex): DelimitedVariablesParser[T] = new DelimitedVariablesParser9[T](f).withRegex(fpr)
  def apply[T](f:(String, String, String, String, String, String, String, String, String, String) => T)(implicit fpr: Regex): DelimitedVariablesParser[T] = new DelimitedVariablesParser10[T](f).withRegex(fpr)
  def apply[T](f: Seq[String] => T)(implicit fpr: Regex, p: ImplicitParam): DelimitedVariablesParser[T] = new DelimitedVariablesParserSeq[T](f).withRegex(fpr)
}
trait FragmentParserExtract extends RegexFragmentParser with ImplicitParameters {
  def extract[T](f: String => T)(implicit fpr: Regex): DelimitedVariablesParser[T] = new DelimitedVariablesParser1[T](f).withRegex(fpr)
  def extract[T](f: (String, String) => T)(implicit fpr: Regex): DelimitedVariablesParser[T] = new DelimitedVariablesParser2[T](f).withRegex(fpr)
  def extract[T](f: (String, String, String) => T)(implicit fpr: Regex): DelimitedVariablesParser[T] = new DelimitedVariablesParser3[T](f).withRegex(fpr)
  def extract[T](f: (String, String, String, String) => T)(implicit  fpr: Regex): DelimitedVariablesParser[T] = new DelimitedVariablesParser4[T](f).withRegex(fpr)
  def extract[T](f: (String, String, String, String, String) => T)(implicit fpr: Regex): DelimitedVariablesParser[T] = new DelimitedVariablesParser5[T](f).withRegex(fpr)
  def extract[T](f: (String, String, String, String, String, String) => T)(implicit fpr: Regex): DelimitedVariablesParser[T] = new DelimitedVariablesParser6[T](f).withRegex(fpr)
  def extract[T](f: (String, String, String, String, String, String, String) => T)(implicit fpr: Regex): DelimitedVariablesParser[T] = new DelimitedVariablesParser7[T](f).withRegex(fpr)
  def extract[T](f: (String, String, String, String, String, String, String, String) => T)(implicit fpr: Regex): DelimitedVariablesParser[T] = new DelimitedVariablesParser8[T](f).withRegex(fpr)
  def extract[T](f: (String, String, String, String, String, String, String, String, String) => T)(implicit fpr: Regex): DelimitedVariablesParser[T] = new DelimitedVariablesParser9[T](f).withRegex(fpr)
  def extract[T](f:(String, String, String, String, String, String, String, String, String, String) => T)(implicit fpr: Regex): DelimitedVariablesParser[T] = new DelimitedVariablesParser10[T](f).withRegex(fpr)
  def extract[T](f: Seq[String] => T)(implicit fpr: Regex, p: ImplicitParam): DelimitedVariablesParser[T] = new DelimitedVariablesParserSeq[T](f).withRegex(fpr)
}

/**
 * Fragment parsers using Scala regex parser combinators
 */
trait RegexFragmentParsers extends FragmentParsers with RegexParsers { this: Specification =>
  def extract[T](p: Parser[T]): FragmentParser[T] = new RegexFragmentParser[T](p)
  def apply[T](p: Parser[T]): FragmentParser[T] = new RegexFragmentParser[T](p)
}
