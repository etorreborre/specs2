package org.specs2
package specification

import reflect.ClassName
/**
 * A Base specification contains the minimum elements for a Specification
 * 
 * * a Seq of Fragments, available through the SpecificationStructure trait
 * * methods for creating Fragments from the FragmentsBuilder trait
 *
 */
trait BaseSpecification extends SpecificationStructure 
   with FragmentsBuilder {
  
  def include(s: BaseSpecification) = fragmentGroup(s.content)
}

trait SpecificationStructure { 
  /** declaration of Fragments from the user */
  def is: Fragments
  
  /** 
   * this "cached" version of the Fragments is kept hidden from the user to avoid polluting
   * the Specification namespace.
   * SpecStart and SpecEnd fragments are added if the user haven't inserted any
   */
  private[specs2] lazy val content: Fragments = {
    val userFragments = is
    val withStartFragments = userFragments.fragments.headOption match {
      case Some(SpecStart(n)) => userFragments.fragments
      case other => SpecStart(name(this)) +: userFragments.fragments
    }
    val withStartAndEndFragments = withStartFragments.lastOption match {
      case Some(SpecEnd(n)) => withStartFragments
      case other => withStartFragments :+ SpecEnd(name(this))
    }
    Fragments(withStartAndEndFragments)(userFragments.arguments)
  }
  private[specs2] def name(spec: AnyRef) = ClassName.className(spec)
} 
