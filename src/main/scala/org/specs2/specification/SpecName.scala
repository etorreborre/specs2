package org.specs2
package specification

import reflect.ClassName._

/**
 * Name declaration for a specification
 */
private[specs2]
sealed trait SpecName {
  def name: String
  def url: String
  def matches(p: String) = name matches p
  def show = name+"("+id+")"

  override def toString = name
  override def equals(o: Any) = {
    o match {
      case s: SpecName => s.id == this.id
      case _ => false
    }
  }
  def id = System.identityHashCode(this)
  def overrideWith(n: SpecName): SpecName
}
private[specs2]
object SpecName {
  def apply(s: SpecificationStructure): SpecName = SpecificationName(s)
  def apply(s: String): SpecificationTitle = SpecificationTitle(s)
}
private[specs2]
case class SpecificationName(s: SpecificationStructure) extends SpecName {
  def name =  simpleClassName(s)
  def url = s.getClass.getName + ".html"

  def overrideWith(n: SpecName) = n match {
    case SpecificationName(s)  => n
    case SpecificationTitle(t) => new SpecificationName(s) {
      override def name =  t
    }
  }
}
private[specs2]
case class SpecificationTitle(t: String) extends SpecName {
  def name = t
  def url = t + ".html"

  def overrideWith(n: SpecName) = n match {
    case SpecificationTitle(t)  => n
    case SpecificationName(s) => new SpecificationName(s) {
      override def id = n.id
      override def name = if (t.isEmpty) n.name else t
    }
  }

}