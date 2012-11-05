package org.specs2
import specification._
import execute._

class FeaturesSpec extends Specification { def is = literate ^
                                                                                                                        """
 This is a features / TODO list for specs2. It is an informal list of offers no guarantee
 of implementation!

                                                                                                                        """^
  releases ^
  preview  ^
  low      ^
  end

 val releases =
 "1.3 features                                                                                                          "^
   "Package dependencies specification                                                                                  "! todo^
   "Reporters                                                                                                           "^
     "pdf                                                                                                               "! todo^
     "Teamcity                                                                                                          "! todo^
     "junit-xml                                                                                                         "! todo^
   "Lift                                                                                                                "^
     "specify the example webapp                                                                                        "! todo^
                                                                                                                        p^
 "1.2 features                                                                                                          "^
   "Typechecked Given/When/Then                                                                                         "! done^
      "integrated with ScalaCheck                                                                                       "! done^
  "stopOnFail argument to stop the execution after the first failure                                                    "! done^
  "Integration with IntelliJ                                                                                            "! done^
                                                                                                                        p^
 "1.1 features                                                                                                          "^
   "Tags                                                                                                                "! done^
   "Parser matchers                                                                                                     "! done^
                                                                                                                        p^
 "1.0 features                                                                                                          "^
   "Publish                                                                                                             "^
     "use sbt to publish                                                                                                "! done^
     "use implicit.ly                                                                                                   "! done^
   "Implementation                                                                                                      "^
     "review all the code and specifications                                                                            "! todo^
   "Html reporter                                                                                                       "^
     "with a table of content                                                                                           "! done^
     "with breadcrumbs                                                                                                  "! done^
     "with non mutable forms                                                                                            "! done^
   "ScalaInterpreter                                                                                                    "^
     "with appropriate matchers                                                                                         "! done^
   "Detailed diffs                                                                                                      "! done^
     "Document the new arguments                                                                                        "! done^
     "Split matcher messages on 2 lines when too long                                                                   "! done^
   "JUnit matchers                                                                                                      "! done^
     "Using a comparison failure for == mismatches                                                                      "! done^
   "Specs-like specification                                                                                            "! done^
   "Auto-examples by using the expectation ok message                                                                   "! done^
   "review the contain matchers                                                                                         "! done

  val preview =
  "Preview features                                                                                                     "^
  "High priority                                                                                                        "^
    "A Console reporter                                                                                                 "^
      "with statistics                                                                                                  "! done^
      "with stacktraces                                                                                                 "! done^
      "with output configuration                                                                                        "^
        "stacktraces on demand                                                                                          "! done^
        "fail only                                                                                                      "! done^
    "A specs file runner                                                                                                "^
      "reporter all classes on the path                                                                                 "! done^
      "according to a regular expression                                                                                "! done^
      "integrated to sbt                                                                                                "! done^
    "A JUnit4 reporter                                                                                                  "^
      "with nested suites                                                                                               "! done^
      "with proper display when the tests have the same name                                                            "! done^
      "with ComparisonFailure                                                                                           "! done^
    "Matchers                                                                                                           " ^
      "with logical combinators                                                                                         "! done^
      "with nice be, have, not syntax                                                                                   "! done^
      "with adapters                                                                                                    "! done^
      "for strings                                                                                                      "! done^
      "for iterables                                                                                                    "! done^
      "for maps                                                                                                         "! done^
      "for classes                                                                                                      "! done^
      "for Either                                                                                                       "! done^
      "for Options/Patterns                                                                                             "! done^
      "for Files                                                                                                        "! done^
      "for Numerics                                                                                                     "! done^
      "for Results                                                                                                      "! done^
      "for xml                                                                                                          "! done^
      "for json                                                                                                         "! done^
    "Spec for before/after/around                                                                                       "^
      "before/after                                                                                                     "! done^
      "example isolation                                                                                                "! done^
      "around                                                                                                           "! done^
      "first/last                                                                                                       "! done^
      "beforeSpec/afterSpec                                                                                             "! done^
    "A literate specs environment                                                                                       "^
      "with an html reporter                                                                                            "! done^
      "supporting Markdown                                                                                              "! done^
    "A sbt reporter                                                                                                     "^
      "based on the console reporter                                                                                    "! done^
    "Data tables                                                                                                        "^
      "with ! and | as separators                                                                                       "! done^
    "Mockito                                                                                                            "^
      "verification of the calls                                                                                        "! done^
      "returning stub values                                                                                            "! done^
      "order of method calls                                                                                            "! done^
      "with argument capture                                                                                            "! done

 val low =
  "Low priority                                                                                                         "^
    "A Console reporter                                                                                                 "^
      "with a timer                                                                                                     "! done^
      "with colored output                                                                                              "! done^
    "StackTraces                                                                                                        "^
      "with source code location                                                                                        "! done^
      "sanitized                                                                                                        "! done^
    "Forms                                                                                                              "^
      "with a text display                                                                                              "! done^
    "Auto examples                                                                                                      "^
      "with the description taken from the source file                                                                  "! done^
    "Timer                                                                                                              "^
      "specs-like Timer                                                                                                 "! done^
    "ScalaCheck                                                                                                         "^
      "with all previous features                                                                                       "! done^
      "with the reporting of expectations                                                                               "! done^
    "Command line arguments                                                                                             "^
      "implement an easy to maintain system coherent with the config                                                    "! done ^
      "can be overriden locally in a specification                                                                      "! done ^
    "Examples / User guide                                                                                              "^
      "Stack                                                                                                            "! done^
      "Specifications layouts                                                                                           "! done^
      "Given/When/Then                                                                                                  "! done^
      "Arguments                                                                                                        "! done^
      "Runners                                                                                                          "! done^
      "Expectations: boolean, matchers, properties                                                                      "! done
}