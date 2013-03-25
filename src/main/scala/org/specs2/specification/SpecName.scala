package org.specs2
package specification

import reflect.ClassName._
import internal.scalaz.Monoid
import io.Paths._
import html.MarkdownLink

/**
 * Identification information for a specification
 */
trait SpecIdentification {
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
  /** a markdown link for the specification url */
  def markdownLink: MarkdownLink
  /** a markdown link for the specification url, with a specific name */
  def markdownLink(name: String): MarkdownLink
}

/**
 * Name declaration for a specification
 */
sealed trait SpecName extends SpecIdentification {
  /** the human readable name of the specification */
  def title: String
  /** the formal name of the specification */
  def name: String
  /** the formal name of the specification, including its package */
  def fullName: String
  /** the full class name of the specification without embellishment */
  def javaClassName: String
  /** a unique url for the specification */
  def url = className(fullName) + ".html"
  /** a markdown link for the specification url */
  def markdownLink = markdownLink(title)
  /** a markdown link for the specification url, with a specific name */
  def markdownLink(name: String) = MarkdownLink(name, url)
  /** @return true if name matches p */
  def matches(p: String) = name matches p
  override def toString = title

  def show = name+"("+id+")"
  def id = System.identityHashCode(this)

  override def equals(o: Any) = o match {
    case s: SpecName => s.name == this.name
    case other       => false
  }

  def is(s: SpecName) = s.id == this.id
  def overrideWith(n: SpecName): SpecName
  def urlIs(u: String): SpecName
  def baseDirIs(dir: String): SpecName
}

private[specs2]
object SpecName {
  def apply(s: SpecificationStructure): SpecName = SpecificationName(s)
  def apply(s: String): SpecificationTitle       = SpecificationTitle(s)

  implicit def SpecNameMonoid: Monoid[SpecName] = new Monoid[SpecName] {
    def append(a1: SpecName, a2: =>SpecName) = if (a2.name.isEmpty) a1 else a2
    val zero = SpecName("")
  }
}

private[specs2]
case class SpecificationName(s: SpecificationStructure) extends SpecName { outer =>
  def title = name
  def name =  simpleClassName(s)
  def fullName = className(s)
  def javaClassName = s.getClass.getName

  def overrideWith(n: SpecName) = n match {
    case SpecificationName(_)  => this
    case SpecificationTitle(t) => new SpecificationName(s) {
      override def id = n.id
      override def title = t
      override def name = outer.name
      override def fullName = outer.fullName
      override def javaClassName = outer.javaClassName
      override def url = n.url
    }
  }
  override def equals(a: Any) = a match {
    case s: SpecificationName => s.name == this.name
    case other                => false
  }

  def urlIs(u: String) = new SpecificationName(s) {
    override def id = outer.id
    override def title = outer.title
    override def name  = outer.name
    override def fullName = outer.fullName
    override def javaClassName = outer.javaClassName
    override def url = u
  }
  def baseDirIs(dir: String) = new SpecificationName(s) {
    override def id = outer.id
    override def title = outer.title
    override def name  = outer.name
    override def fullName = outer.fullName
    override def javaClassName = outer.javaClassName
    override def url = outer.url.rebase(dir)
  }
}
private[specs2]
case class SpecificationTitle(t: String) extends SpecName { outer =>
  def title = t
  def name = title
  def fullName = name
  def javaClassName = fullName

  def overrideWith(n: SpecName) = n match {
    case SpecificationTitle(_) => this
    case SpecificationName(s)  => new SpecificationName(s) {
      override def id = n.id
      override def title = t
      override def name = n.name
      override def fullName = n.fullName
      override def javaClassName = n.javaClassName
    }
  }
  def urlIs(u: String) = new SpecificationTitle(t) {
    override def id = outer.id
    override def title = outer.title
    override def name  = outer.name
    override def fullName = outer.fullName
    override def javaClassName = outer.javaClassName
  }
  def baseDirIs(dir: String) = new SpecificationTitle(t) {
    override def id = outer.id
    override def title = outer.title
    override def name  = outer.name
    override def fullName = outer.fullName
    override def javaClassName = outer.javaClassName
    override def url = outer.url.rebase(dir)
  }
}