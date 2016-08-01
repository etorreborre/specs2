package org.specs2.matcher.describe.constraints

import org.specs2.Spec
import org.specs2.matcher.describe.constraints.IsCaseClass._
import shapeless.test.illTyped


class IsCaseClassMacroTest extends Spec { def is = s2"""
    macro is able to prove that class if of type case class                               ${ checkCaseClass[SomeCaseClass] must beAnInstanceOf[IsCaseClass[SomeCaseClass]] }
    Macro will fail compilation in case class is not of case class                        ${ illTyped( """checkCaseClass[NotCaseClass]"""); ok }

    type system is able to use macro                                                      ${ CaseClassWrapper[SomeCaseClass]() must beAnInstanceOf[CaseClassWrapper[SomeCaseClass]] }
    type system is able to use macro and fails compilation if class is not a case class   ${ illTyped( """CaseClassWrapper[NotCaseClass]""" ); ok }
  """

  case class SomeCaseClass()
  class NotCaseClass()

  case class CaseClassWrapper[T <: Product with Serializable: IsCaseClass]()
}
