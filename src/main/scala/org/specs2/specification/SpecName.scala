package org.specs2
package specification

import reflect.ClassName._

/**
 * Name declaration for a specification
 */
private[specs2]
sealed trait SpecName {
  /** the human readable name of the specification */
  def title: String
  /** the formal name of the specification */
  def name: String
  /** a unique url for the specification */
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
  def title = name
  def name =  simpleClassName(s)
  def url = s.getClass.getName + ".html"

  def overrideWith(n: SpecName) = n match {
    case SpecificationName(s)  => n
    case SpecificationTitle(t) => new SpecificationName(s) {
      override def id = n.id
      override def title =  t
    }
  }
  override def equals(a: Any) = a match {
    case s: SpecificationName => s.name == this.name
    case other                => false
  }
}
private[specs2]
case class SpecificationTitle(t: String) extends SpecName {
  def title = t
  def name = title
  def url = t + ".html"

  def overrideWith(n: SpecName) = n match {
    case SpecificationTitle(t)  => n
    case SpecificationName(s) => new SpecificationName(s) {
      override def id = n.id
      override def title = t
      override def name = n.name
    }
  }

}