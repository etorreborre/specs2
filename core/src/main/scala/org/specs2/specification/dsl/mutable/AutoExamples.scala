package org.specs2
package specification
package dsl
package mutable

import execute.AsResult
import core.{Fragments}
import create.{FragmentsFactory}

/**
 * Auto-example creation for mutable specifications
 */
trait AutoExamples extends create.AutoExamples with FragmentBuilder with FragmentsFactory:

  override def postProcessAutoExample(fs: Fragments): Fragments =
    addFragments(fs)
    addFragment(fragmentFactory.break)
    Fragments.empty

