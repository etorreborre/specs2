package org.specs2.matcher.describe

import org.specs2.Spec
import org.specs2.matcher.TypecheckMatchers
import IsCaseClass.checkCaseClass
import org.specs2.execute.Typecheck._

class IsCaseClassMacroTest extends Spec with TypecheckMatchers { def is = s2"""
  macro is able to prove that class if of type case class ${
    checkCaseClass[SomeCaseClass] must beAnInstanceOf[IsCaseClass[SomeCaseClass]]
  }

  Macro will fail compilation in case class is not of case class ${
    tc"""checkCaseClass[NotCaseClass]""" must not(succeed)
  }

  type system is able to use macro ${
    CaseClassWrapper[SomeCaseClass]() must beAnInstanceOf[CaseClassWrapper[SomeCaseClass]]
  }

  type system is able to use macro and fails compilation if class is not a case class ${
    tc"""CaseClassWrapper[NotCaseClass]""" must not(succeed)
  }
"""

  case class SomeCaseClass()
  class NotCaseClass()

  case class CaseClassWrapper[T <: Product with Serializable: IsCaseClass]()
}
