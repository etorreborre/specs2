package org.specs2
package text

import matcher.TypedEqual
import RegexExtractor.*
import execute.{ErrorException, FailureException}
import util.matching.Regex

class RegexExtractorSpec extends Spec with TypedEqual {
  def is = s2"""

RegexExtractors can extract up to 10 distinct parameters from a piece of text
 one parameter $extraction1
 2 parameters $extraction2
 the sequences of all parameters $extraction3
 even if there are more parameters than necessary $extraction4
 it's ok if there is nothing to extract and only one string is expected $extraction5
 if the regular expression can't parse anything, the full string is returned $extraction6

Exceptions are thrown when the extraction doesn't work
 FailureException if there are not enough parameters $exception1
 ErrorException if the regular expression is malformed $exception2

"""
  val REGEX = """\|\{([^}]+)\}""".r

  def extraction1 = extract1("hello |{world}!", group = REGEX) === "world"
  def extraction2 = extract2("|{hello} |{world}!", group = REGEX) === (("hello", "world"))
  def extraction3 = extractAll("|{hello} |{world}, I'm |{Eric}!", group = REGEX) === Seq("hello", "world", "Eric")
  def extraction4 = extract2("|{hello} |{world}, I'm |{Eric}!", group = REGEX) === (("hello", "world"))
  def extraction5 = extract1("hello world") === "hello world"
  def extraction6 = extract1("hello |{world}", group = "^+?".r) === "hello |{world}"

  def exception1 = extract2("hello |{world}", group = REGEX) must throwA[FailureException]
  def exception2 = extract1("hello |{world}", group = new Regex("][")) must throwAn[ErrorException]
}
