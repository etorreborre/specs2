package org.specs2
import specification._
import execute._

class FeaturesSpec extends Specification with StandardResults {
  def is = 
                                                                                          """
 These are the list of features todevelop / port for specs2.

 The prioritization idea is to:

  * check the feasability of old features in a fully functional setting
  * re-evaluate the necessity of some existing features and provide the most important 
    ones first including the ones necessary for a proper development of specs2 itself
  * make sure that
    ** at each step that everything is well specified
    ** dependencies are well-controled
    ** implicits visibility is reduced
    ** specs2 API is kept private unless necessary
                                                                                          """^
"  High priority                                                                          "^
"    A Console reporter                                                                   "^
"      with statistics                                                                    "! done^
"      with stacktraces                                                                   "! done^
"      with output configuration                                                          "^
"        stacktraces on demand                                                            "! todo^
"        fail only                                                                        "! todo^
"        pending only                                                                     "! todo^
"        Fragments filter                                                                 "! todo^
"    A specs file runner                                                                  "^
"      reporter all classes on the path                                                   "! done^
"      according to a regular expression                                                  "! todo^
"      reporting the final statistics                                                     "! done^
"    A JUnit4 reporter                                                                    "^
"      with nested suites                                                                 "! done^
"      with proper display when the tests have the same name                              "! done^
"      with ComparisonFailure                                                             "! done^
"    Matchers                                                                             " ^
"      with logical combinators                                                           "! done^
"      with nice be, have, not syntax                                                     "! done^
"      with adapters                                                                      "! done^
"      for strings                                                                        "! done^
"      for iterables                                                                      "! todo^
"      for maps                                                                           "! todo^
"      for classes                                                                        "! todo^
"      for Either                                                                         "! todo^
"      for Options/Patterns                                                               "! todo^
"      for Files                                                                          "! todo^
"      for Numerics                                                                       "! todo^
"      for xml                                                                            "! todo^
"    Spec for before/after/around                                                         "^
"      before/after                                                                       "! done^
"      example isolation                                                                  "! done^
"      around                                                                             "! done^
"      first/last                                                                         "! done^
"      beforeSpec/afterSpec                                                               "! done^
"    A literate specs environment                                                         "^
"      with an html reporter                                                              "! todo^
"      with non mutable forms                                                             "! done^
"    A sbt reporter                                                                       "^
"      based on the console reporter                                                      "! done^
"    Data tables                                                                          "^
"      with ! and | as separators                                                         "! done^
"    Mockito                                                                              "^
"      verification of the calls                                                          "! done^
"      returning stub values                                                              "! done^
"      order of method calls                                                              "! done^
"      with argument capture                                                              "! done^
                                                                                          p^    
"  Low priority                                                                           "^
"    A Console reporter                                                                   "^
"      with a timer                                                                       "! todo^
"      with colored output                                                                "! todo^
"      with intermediary stats when required                                              "! todo^
"    StackTraces                                                                          "^
"      for Fragments, to help with IDE navigation                                         "! todo^
"      for all results, even success                                                      "! todo^
"      with source code location                                                          "! done^
"      sanitized                                                                          "! done^
"    Tags                                                                                 "^
"      for examples, groups, specs                                                        "! todo^
"      with dependencies between tags                                                     "! todo^
"    Forms                                                                                "^
"      with a text display                                                                "! done^
"    JMock                                                                                "^
"      with all features                                                                  "! todo^
"    EasyMock                                                                             "^
"      with all features                                                                  "! todo^
"    Detailed diffs                                                                       "^
"      non mutable version                                                                "! todo^
"    Auto examples                                                                        "^
"      with the description taken from the source file                                    "! done^
"    Timer                                                                                "^
"      see timer and WaitFor                                                              "! todo^
"    A ScalaInterpreter                                                                   "^
"      with appropriate matchers                                                          "! todo^
"    ScalaCheck                                                                           "^
"      with all previous features                                                         "! done^
"      with the reporting of expectations                                                 "! done^
"    A Step execution model?                                                              "! todo^
"    A Database facility?                                                                 "! todo^
"    Command line arguments                                                               "^
"      implement an easy to maintain system coherent with the config                      "! done ^
                                                                                          end
}