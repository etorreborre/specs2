package org.specs2
package matcher

import execute.*
import Typecheck.*
import fp.*
import ResultMatchers.*

class TypecheckSpec extends Specification:
  def is = s2"""

  An expression can be successfully typechecked $e1
  An expression can be typechecked and report errors $e2
  Additional errors are reported in details $e3

  """

  def e1 =
    val code =
      """
        Monoid[String].zero
        """
    typecheck(code) === TypecheckSuccess

  def e2 =
    // the string to typecheck is split in 2 to show that it is not
    // necessary to parse string literals
    def code1 = """val n: Int = 0;  """
    def code2 = """val s: Int = n + "hello" """

    val code = code1 + code2

    val result = AsResult(typecheck(code))
    result must beFailing(startWith("""|val n: Int = 0;  val s: Int = n + "hello"
                                       |                             ^
                                       |Found:    String
                                       |Required: Int""".stripMargin))

  def e3 =
    def code1 = """val n: Int = "x";  """
    def code2 = """val s: Int = "y"   """

    val code = code1 + code2

    val result = AsResult(typecheck(code))
    result must beFailing(startWith("""|val n: Int = "x";  val s: Int = "y"
                                       |            ^
                                       |Found:    ("x" : String)
                                       |Required: Int""".stripMargin))

    result match
      case Failure(_, _, _, FailureDetailsMessages(messages)) =>
        messages(0) must startWith("""|val n: Int = "x";  val s: Int = "y"
                                      |                               ^
                                      |Found:    ("y" : String)
                                      |Required: Int""".stripMargin)
      case other =>
        failure(s"unexpected $other")
