package org.specs2
package matcher

import execute.*, Typecheck.*
import fp.*

class TypecheckSpec extends Specification:
  def is = s2"""

  An expression can be successfully typechecked $e1
  An expression can be typechecked and report errors $e2

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
    def code1 = """ val n: Int = 0;  """
    def code2 = """ val s: Int = n + "hello" """

    val code = code1 + code2

    typecheck(code) match
      case TypecheckSuccess =>
        failure("expected some errors"): Result
      case TypecheckErrors(errors) =>
        errors.toString must =~("Found:    String")
