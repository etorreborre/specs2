package org.specs2
package specification

import reflect.ClassName
import main.Arguments
/**
 * A Base specification contains the minimum elements for a Specification
 * 
 * * a Seq of Fragments, available through the SpecificationStructure trait
 * * methods for creating Fragments from the FragmentsBuilder trait
 *
 */
trait BaseSpecification extends SpecificationStructure 
   with FragmentsBuilder {
  
  def include(s: SpecificationStructure): Group = fragmentGroup(s.content)
  def include(f: Fragments): Group = fragmentGroup(f)
  def include(args: Arguments, s: SpecificationStructure): Group = {
    fragmentGroup(Fragments.withSpecStartEnd(s.is.copy(arguments = args), name(s)))
  }
}

trait SpecificationStructure { 
  /** declaration of Fragments from the user */
  def is: Fragments
  
  /** 
   * this "cached" version of the Fragments is kept hidden from the user to avoid polluting
   * the Specification namespace.
   * SpecStart and SpecEnd fragments are added if the user haven't inserted any
   */
  private[specs2] lazy val content: Fragments = Fragments.withSpecStartEnd(is, name(this))
  private[specs2] def name(spec: SpecificationStructure = this) = SpecName(spec)
} 
trait SpecName {
  def name: String
  def url: String
  def matches(p: String) = name matches p
  override def toString = name
}
object SpecName {
  def apply(s: SpecificationStructure): SpecificationName = SpecificationName(s)
  def apply(s: String): SpecificationTitle = SpecificationTitle(s)
}
case class SpecificationName(s: SpecificationStructure) extends SpecName {
  def name =  ClassName.className(s)
  def url = s.getClass.getName + ".html"
}
case class SpecificationTitle(s: String) extends SpecName {
  def name = s
  def url = s + ".html"
}