package org.specs2
package specification

import control.Exceptions._
import execute._
import util.matching.Regex

/**
 * A Regular expression step which takes a text and extracts meaningful values according to
 * a regular expression:
 *
 * - either a user-defined expression
 * - or if the used-defined expression is empty, a default regex where values to capture are delimited with `${}`
 *
 * It provides methods to extract either all the groups as a list, or a number of values as a tuple
 *
 */
abstract class RegexStep[P, T](regex: String = "", defaultRegex: String = RegexStep.DEFAULT_REGEX) {

  /** @return the regex that must be used */
  protected def regexToUse = if (regex.isEmpty) defaultRegex else regex
  /** extract all groups and return a list of strings */
  def extractAll(text: String) =
    if (regex.isEmpty) defaultRegex.r.findAllIn(text).matchData.collect { case Regex.Groups(g) => g }.toList
    else               regex.r.unapplySeq(text).get

  /** remove value markers `${}` from the text */
  def strip(text: String): String =
    if (regex.isEmpty) RegexStep.strip(text, defaultRegex)
    else               text

  /** remove value markers `${}` from a Text fragment */
  def strip(f: Fragment): Fragment =
    f match {
      case Text(t) => Text(strip(t))
      case other   => other
    }

  /** extract the value contained in the first group */
  def extract1(t: String) = (extractAll(t): @unchecked) match { case s1::_ => s1 }
  def extract2(t: String) = (extractAll(t): @unchecked) match { case s1::s2::_ => (s1,s2) }
  def extract3(t: String) = (extractAll(t): @unchecked) match { case s1::s2::s3::_ => (s1,s2,s3) }
  def extract4(t: String) = (extractAll(t): @unchecked) match { case s1::s2::s3::s4::_ => (s1,s2,s3,s4) }
  def extract5(t: String) = (extractAll(t): @unchecked) match { case s1::s2::s3::s4::s5::_ => (s1,s2,s3,s4,s5) }
  def extract6(t: String) = (extractAll(t): @unchecked) match { case s1::s2::s3::s4::s5::s6::_ => (s1,s2,s3,s4,s5,s6) }
  def extract7(t: String) = (extractAll(t): @unchecked) match { case s1::s2::s3::s4::s5::s6::s7::_ => (s1,s2,s3,s4,s5,s6,s7) }
  def extract8(t: String) = (extractAll(t): @unchecked) match { case s1::s2::s3::s4::s5::s6::s7::s8::_ => (s1,s2,s3,s4,s5,s6,s7,s8) }
  def extract9(t: String) = (extractAll(t): @unchecked) match { case s1::s2::s3::s4::s5::s6::s7::s8::s9::_ => (s1,s2,s3,s4,s5,s6,s7,s8,s9) }
  def extract10(t: String) = (extractAll(t): @unchecked) match { case s1::s2::s3::s4::s5::s6::s7::s8::s9::s10::_ => (s1,s2,s3,s4,s5,s6,s7,s8,s9,s10) }
}

object RegexStep {
  private val DEFAULT_REGEX = """\$\{([^}]+)\}"""

  /**
   * Apparently, the expression to replace can have any regex special character except '\'
   */
  def strip(text: String, regex: String = DEFAULT_REGEX): String =
    regex.r.replaceAllIn(text, (_:Regex.Match) match { case Regex.Groups(v) => v.replace("\\", "\\\\") })
}

/**
 * This step can start a sequence of Given / When / Then.
 *
 * It must define the extract function creating a value of type T from the extracted values
 */
abstract class Given[T](val regex: String = "") extends RegexStep[Unit, T](regex) {
  /** if the extraction goes wrong, then an Error is propagated */
  private[specs2] def extractContext(text: String): Either[Result, T] = trye(extract(text))((e:Exception) => Error(e))

  def extract(text: String): T
}

/**
 * This step define conditions in a sequence of Given / When / Then.
 *
 * It must define the extract function taking the previous state of extracted values, P, and creating a new state
 * of type T from the extracted values
 */
abstract class When[P, T](val regex: String = "") extends RegexStep[P, T](regex) {
  /**
   * if the previous extraction went wrong, then a Skipped result is propagated.
   * Otherwise if the current extraction goes wrong, then an Error is propagated
   */
  private[specs2] def extractContext(p: Either[Result, P], text: String): Either[Result, T] = p match {
    case Left(l)  => Left(Skipped(l.message))
    case Right(r) => trye(extract(r, text))((e:Exception) => Error(e))
  }
  def extract(p: P, text: String): T
}

/**
 * This step define checks in a sequence of Given / When / Then.
 *
 * It must define the extract function taking the state of extracted values, T, and return a `Result`
 */
abstract class Then[T](regex: String = "") extends RegexStep[Either[Result, T], (T, Result)](regex) {
  /**
   * if the previous extraction went wrong, then a Skipped result is propagated.
   * Otherwise if the current extraction goes wrong, then an Error is propagated
   */
  private[specs2] def extractContext(t: Either[Result, T], text: String): Either[Result, (T, Result)] = t match {
    case Left(l)  => Left(Skipped(l.message))
    case Right(r) => trye((r, extract(r, text)))((e:Exception) => Error(e))
  }
  def extract(t: T, text: String): Result
}

abstract class GivenThen(regex: String= "") extends RegexStep[String, Result](regex) {
  def extract(text: String): Result
}

class GivenThenFunction[R <% Result](f: PartialFunction[Any, R], regex: String= "") extends GivenThen(regex) {
  def extract(text: String): Result = {
    regexToUse.r.findAllIn(text).size match {
      case 1 => f(extract1(text))
      case 2 => f(extract2(text))
      case 3 => f(extract3(text))
      case 4 => f(extract4(text))
      case 5 => f(extract5(text))
      case 6 => f(extract6(text))
      case 7 => f(extract7(text))
      case 8 => f(extract8(text))
      case 9 => f(extract9(text))
      case 10 => f(extract10(text))
    }
  }
}
object so {
  def apply[R <% Result](f: PartialFunction[Any, R]): GivenThen = new GivenThenFunction(f)
}
