package org.specs2
package specification

import reflect._

/**
 * Name declaration for a specification
 */
private[specs2]
trait SpecName {
  def name: String
  def url: String
  def matches(p: String) = name matches p
  override def toString = name
  override def equals(o: Any) = {
    o match {
      case s: SpecName => s.name == this.name
      case _ => false
    }
  }
}
private[specs2]
object SpecName {
  def apply(s: SpecificationStructure): SpecificationName = SpecificationName(s)
  def apply(s: String): SpecificationTitle = SpecificationTitle(s)
}
private[specs2]
case class SpecificationName(s: SpecificationStructure) extends SpecName {
  def name =  ClassName.className(s)
  def url = s.getClass.getName + ".html"
}
private[specs2]
case class SpecificationTitle(s: String) extends SpecName {
  def name = s
  def url = s + ".html"
}