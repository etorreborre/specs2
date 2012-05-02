package org.specs2
package specification

import reflect.ClassName._
import internal.scalaz.Monoid

/**
 * Name declaration for a specification
 */
private[specs2]
sealed trait SpecName {
  /** the human readable name of the specification */
  def title: String
  /** the formal name of the specification */
  def name: String
  /** the formal name of the specification, including its package */
  def fullName: String
  /** the full class name of the specification without embellishment */
  def javaClassName: String
  /** a unique url for the specification */
  def url: String
  def matches(p: String) = name matches p
  def show = name+"("+id+")"

  override def toString = title
  def is(s: SpecName) = s.id == this.id

  def id = System.identityHashCode(this)

  override def equals(o: Any) = o match {
    case s: SpecName => s.name == this.name
    case other       => false
  }
  
  def overrideWith(n: SpecName): SpecName
}
private[specs2]
object SpecName {
  def apply(s: SpecificationStructure): SpecName = SpecificationName(s)
  def apply(s: String): SpecificationTitle = SpecificationTitle(s)
  def apply(s: String, filePath: String): SpecificationTitle = new SpecificationTitle(s) {
    override def url = filePath
  }

  implicit def SpecNameMonoid: Monoid[SpecName] = new Monoid[SpecName] {
    def append(a1: SpecName, a2: =>SpecName) = if (a2.name.isEmpty) a1 else a2
    val zero = SpecName("")
  }
}

private[specs2]
case class SpecificationName(s: SpecificationStructure) extends SpecName {
  def title = name
  def name =  simpleClassName(s)
  def fullName = className(s)
  def javaClassName = s.getClass.getName

  def url = className(s) + ".html"

  def overrideWith(n: SpecName) = n match {
    case SpecificationName(s)  => this
    case SpecificationTitle(t) => new SpecificationName(s) {
      override def id = n.id
      override def title = t
      override def url = n.url
    }
  }
  override def equals(a: Any) = a match {
    case s: SpecificationName => s.name == this.name
    case other                => false
  }
}
private[specs2]
case class SpecificationTitle(t: String) extends SpecName { outer =>
  def title = t
  def name = title
  def fullName = name
  def javaClassName = fullName
  def url = t + ".html"

  def overrideWith(n: SpecName) = n match {
    case SpecificationTitle(t)  => this
    case SpecificationName(s) => new SpecificationName(s) {
      override def id = n.id
      override def title = t
      override def name = n.name
      override def fullName = n.fullName
      override def url = outer.url
    }
  }

}