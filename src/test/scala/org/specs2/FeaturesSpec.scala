package org.specs2
import specification._
import execute._

class FeaturesSpec extends Specification with FeaturesResults {
  override def args = "pending"
  val examples = 
"""
 These are the list of features to develop / port for specs2.
 
 The prioritization idea is to:
 
 * check the feasability of old features in a fully functional setting
 * re-evaluate the necessity of some existing features and provide the most important ones first
     including the ones necessary for a proper development of specs2 itself
 * make sure at each step that everything is well specified
 * dependencies are well-controled
 * implicits visibility is reduced
 * specs2 API is kept private unless necessary
 """^^
  "High priority"^
    "A Console reporter"^
      "with statistics" ! done^
      "with stacktraces" ! done^
      "with output configuration"^
        "no-stacktraces" ! done^
        "fail only" ! done^
        "pending only" ! done^
        "examples filter" ! todo^
    "A specs file runner"^
      "reporter all classes on the path" ! done^
      "according to a regular expression" ! todo^
      "reporting the final statistics" ! todo^
    "Matchers"^
      "with logical combinators" ! todo^
      "for strings" ! done^
      "for iterables" ! todo^
    "Spec for before/after/around"^
      "before/after" ! done^
      "example isolation" ! done^
      "around" ! done^
      "first/last" ! done^
      "beforeSpec/afterSpec" ! done^
    "A literate specs environment"^
      "with an html reporter" ! todo^
      "with non mutable forms" ! todo^
    "A JUnit4 reporter"^
      "with suites and test cases" ! todo^
    "A sbt reporter"^
      "based on a generic notifier" ! todo^
  par^    
  "Low priority"^
    "A Console reporter"^
      "with a timer" ! todo^
      "with colored output" ! todo^
      "with intermediary stats where required" ! todo^
    "StackTraces"^
      "sanitized" ! todo^
    "Detailed diffs"^
      "non mutable version" ! todo^
    "Configuration"^
      "specify its behavior" ! todo^
    "Command line arguments"^
      "implement an easy to maintain system coherent with the config" ! todo
}