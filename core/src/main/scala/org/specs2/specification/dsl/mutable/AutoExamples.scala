package org.specs2
package specification
package dsl
package mutable

import execute.AsResult
import org.specs2.specification.core.{Fragments, Fragment}

/**
 * Auto-example creation for mutable specifications
 */
trait AutoExamples extends create.AutoExamples with FragmentBuilder {
  override def createExample[T](expression: String, code: =>T, asResult: AsResult[T]): Fragments =
    Fragments(super.createExample(expression, code, asResult).append(fragmentFactory.break).fragments.map(addFragment):_*)

}
