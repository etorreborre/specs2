package org.specs2
package text

import specification._
import RegexExtractor._
import execute.{ErrorException, FailureException}
import util.matching.Regex

class RegexExtractorSpec extends script.Specification with Groups { def is = s2"""

 # RegexExtractors can extract up to 10 distinct parameters from a piece of text
 + one parameter
 + 2 parameters
 + the sequences of all parameters
 + even if there are more parameters than necessary
 + it's ok if there is nothing to extract and only one string is expected
 + if the regular expression can't parse anything, the full string is returned

 # Exceptions are thrown when the extraction doesn't work
 + FailureException if there are not enough parameters
 + ErrorException if the regular expression is malformed
"""

  "extraction" - new group {
    eg := extract1("hello ${world}!") === "world"
    eg := extract2("${hello} ${world}!") === ("hello", "world")
    eg := extractAll("${hello} ${world}, I'm ${Eric}!") === Seq("hello", "world", "Eric")
    eg := extract2("${hello} ${world}, I'm ${Eric}!") === ("hello", "world")
    eg := extract1("hello world")=== "hello world"
    eg := extract1("hello ${world}", group = "^+?".r) === "hello ${world}"
  }

  "errors" -  new group {
    eg := extract2("hello ${world}") must throwA[FailureException]
    eg := extract1("hello ${world}", group = new Regex("][")) must throwA[ErrorException]
  }
}
