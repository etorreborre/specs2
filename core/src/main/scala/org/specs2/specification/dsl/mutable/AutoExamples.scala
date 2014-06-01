package org.specs2
package specification
package dsl
package mutable

import execute.AsResult
import specification.core.Fragment

trait AutoExamples extends create.AutoExamples with FragmentBuilder {
  override def createExample[T](expression: String, code: =>T, asResult: AsResult[T]): Fragment =
    addFragment(super.createExample(expression, code, asResult))

}
