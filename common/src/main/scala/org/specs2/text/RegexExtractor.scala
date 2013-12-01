package org.specs2
package text

import util.matching.Regex
import control.Exceptions._
import execute._
import control.Exceptions

/**
 * A Regular expression step which takes a text and extracts meaningful values according to
 * a regular expression:
 *
 * - either a user-defined expression for the *full text to extract*: e.g. "Given the following number: (.*)"
 * - or a default regex for groups capturing value delimited with `${}`
 *
 * It provides methods to extract either all the groups as a list, or a number of values as a tuple
 *
 */
abstract class RegexExtractor[P, T](private var fullRegex: String = "", private var groupRegex: String = RegexExtractor.DEFAULT_REGEX) {

  private def full: Regex = fullRegex.r
  private def group: Regex = groupRegex.r
  /** regex to use for a step with a partial function */
  protected def regexToUse = if (full.toString.isEmpty) group else full

  /** change the regexps */
  def withRegex(full: String = "", group: String = RegexExtractor.DEFAULT_REGEX): this.type = {
    fullRegex = full
    groupRegex = group
    this
  }
  /** remove value markers `${}` from the text */
  def strip(text: String): String = RegexExtractor.strip(text, full, group)

  /** extract the value contained in the first group */
  def extract1(t: String) = RegexExtractor.extract1(t, full, group)
  def extract2(t: String) = RegexExtractor.extract2(t, full, group)
  def extract3(t: String) = RegexExtractor.extract3(t, full, group)
  def extract4(t: String) = RegexExtractor.extract4(t, full, group)
  def extract5(t: String) = RegexExtractor.extract5(t, full, group)
  def extract6(t: String) = RegexExtractor.extract6(t, full, group)
  def extract7(t: String) = RegexExtractor.extract7(t, full, group)
  def extract8(t: String) = RegexExtractor.extract8(t, full, group)
  def extract9(t: String) = RegexExtractor.extract9(t, full, group)
  def extract10(t: String)= RegexExtractor.extract10(t, full, group)
  def extractAll(t: String) = RegexExtractor.extractAll(t, full, group)
}

object RegexExtractor {
  val DEFAULT_REGEX = """\$\{([^}]+)\}"""

  def extract[R](text: String, f: PartialFunction[Any, R], regexToUse: =>Regex = DEFAULT_REGEX.r): R = tryWithRegex(text, regexToUse) {
    regexToUse.findAllIn(text).size match {
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
  /** extract all groups and return a list of strings */
  def extractAll(text: String, full: =>Regex = "".r, group: =>Regex = DEFAULT_REGEX.r): List[String] = tryWithRegex(text, regexToUse(full, group)) {
    if (full.toString.isEmpty) group.findAllIn(text.trim).matchData.collect { case Regex.Groups(g) => g }.toList
    else                       full.unapplySeq(text.trim).getOrElse(throw new FailureException(Failure(s"could not extract '$full' from $text"))).toList
  }

  private def regexToUse(full: =>Regex, group: =>Regex) = if (full.toString.isEmpty) group else full

  def strip(text: String): String = strip(text, "".r, DEFAULT_REGEX.r)
  /**
   * Apparently, the expression to replace can have any regex special character except '\'
   */
  def strip(text: String, full: =>Regex, group: =>Regex): String = tryWithRegex(text, regexToUse(full, group)) {
    if (full.toString.isEmpty) group.replaceAllIn(text, (_:Regex.Match) match { case Regex.Groups(v) => v.replace("\\", "\\\\") })
    else                       text
  }
  def extract1(t: String , full: =>Regex = "".r, group: =>Regex = DEFAULT_REGEX.r) = check(1, t, (extractAll(t, full, group): @unchecked) match { case s1::_ => s1 }                                                                   )
  def extract2(t: String , full: =>Regex = "".r, group: =>Regex = DEFAULT_REGEX.r) = check(2, t, (extractAll(t, full, group): @unchecked) match { case s1::s2::_ => (s1,s2) }                                                          )
  def extract3(t: String , full: =>Regex = "".r, group: =>Regex = DEFAULT_REGEX.r) = check(3, t, (extractAll(t, full, group): @unchecked) match { case s1::s2::s3::_ => (s1,s2,s3) }                                                   )
  def extract4(t: String , full: =>Regex = "".r, group: =>Regex = DEFAULT_REGEX.r) = check(4, t, (extractAll(t, full, group): @unchecked) match { case s1::s2::s3::s4::_ => (s1,s2,s3,s4) }                                            )
  def extract5(t: String , full: =>Regex = "".r, group: =>Regex = DEFAULT_REGEX.r) = check(5, t, (extractAll(t, full, group): @unchecked) match { case s1::s2::s3::s4::s5::_ => (s1,s2,s3,s4,s5) }                                     )
  def extract6(t: String , full: =>Regex = "".r, group: =>Regex = DEFAULT_REGEX.r) = check(6, t, (extractAll(t, full, group): @unchecked) match { case s1::s2::s3::s4::s5::s6::_ => (s1,s2,s3,s4,s5,s6) }                              )
  def extract7(t: String , full: =>Regex = "".r, group: =>Regex = DEFAULT_REGEX.r) = check(7, t, (extractAll(t, full, group): @unchecked) match { case s1::s2::s3::s4::s5::s6::s7::_ => (s1,s2,s3,s4,s5,s6,s7) }                       )
  def extract8(t: String , full: =>Regex = "".r, group: =>Regex = DEFAULT_REGEX.r) = check(8, t, (extractAll(t, full, group): @unchecked) match { case s1::s2::s3::s4::s5::s6::s7::s8::_ => (s1,s2,s3,s4,s5,s6,s7,s8) }                )
  def extract9(t: String , full: =>Regex = "".r, group: =>Regex = DEFAULT_REGEX.r) = check(9, t, (extractAll(t, full, group): @unchecked) match { case s1::s2::s3::s4::s5::s6::s7::s8::s9::_ => (s1,s2,s3,s4,s5,s6,s7,s8,s9) }         )
  def extract10(t: String, full: =>Regex = "".r, group: =>Regex = DEFAULT_REGEX.r) = check(10, t, (extractAll(t, full, group): @unchecked) match { case s1::s2::s3::s4::s5::s6::s7::s8::s9::s10::_ => (s1,s2,s3,s4,s5,s6,s7,s8,s9,s10) })

  /**
   * check if the extraction was successful. If we were trying to extract one variable and if it was not found, return the full
   * string. Otherwise return a Failure explaining how many variables we were trying to extract
   */
  private def check[T](variablesNb: Int, string: String, extract: =>T) = tryOr(extract) {
    case e: MatchError => if (variablesNb == 1) string.asInstanceOf[T] else throw new FailureException(Failure("couldn't extract "+variablesNb+" variables from: "+string))
    case other         => throw other
  }

  private def tryWithRegex[T](text: String, regex: =>Regex)(code: =>T): T =
    Exceptions.tryOr(code) {
      case f: FailureException => throw f
      case e: MatchError => throw e
      case other         => throw new ErrorException(new Error(s"could not extract the regex from $text: ${other.getMessage}", other))
    }
}
