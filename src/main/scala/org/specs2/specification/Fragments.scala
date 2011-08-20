package org.specs2
package specification

import execute.Executable
import control.LazyParameters._
import control.LazyParameter
import main.Arguments
import StandardFragments._
import org.specs2.internal.scalaz.Monoid
import Fragments._
/**
 * A Fragments object is a list of fragments with a SpecStart and a SpecEnd
 */
case class Fragments(private val title: Option[SpecName] = None, middle: Seq[Fragment] = Nil, arguments: Arguments = Arguments(), link: Option[HtmlLink] = None, seeOnly: Boolean = false) {
  def fragments: Seq[Fragment] = if (middle.isEmpty && !link.isDefined) Seq() else (start +: middle :+ end)

  private def append(e: Fragment) = copy(middle = middle :+ e)
  def specTitleIs(name: SpecName): Fragments = copy(title = title.map(_.overrideWith(name)).orElse(Some(name)))
  def add(e: Fragment): Fragments = append(e)
  def add(fs: Seq[Fragment]): Fragments = copy(middle = middle ++ fs)
  def add(fs: Fragments): Fragments = add(fs.fragments)
  def add(a: Arguments): Fragments = copy(arguments = arguments.overrideWith(a))
  
  def linkIs(htmlLink: HtmlLink) = copy(link = Some(htmlLink))
  def seeIs(htmlLink: HtmlLink) = copy(middle = Nil, link = Some(htmlLink), seeOnly = true)

  def executables: Seq[Executable] = fragments.collect { case e: Executable => e }
  def examples: Seq[Example] = fragments.collect(isAnExample)

  def overrideArgs(args: Arguments) = copy(arguments = arguments.overrideWith(args))
  def map(function: Fragment => Fragment) = copy(middle = middle.map(function))
  import StandardFragments._
  override def toString = fragments.mkString("\n")

  def specName = start.specName
  def name = start.name
  
  lazy val start: SpecStart = SpecStart(title.getOrElse(SpecName("")), arguments, link, seeOnly)
  lazy val end: SpecEnd = SpecEnd(start.specName)

}

/**
 * Utility methods for fragments
 */
object Fragments {

  def apply(t: SpecName) = new Fragments(title = Some(t))
  /**
   * @return a Fragments object containing only a seq of Fragments.
   */
  def createList(fs: Fragment*) = Fragments(middle = fs)
  /**
   * @return a Fragments object, where the SpecStart might be provided by the passed fragments
   */
  def create(fs: Fragment*) = {
    fs.toList match {
      case (s @ SpecStart(n, a, l, so)) :: rest => Fragments(Some(n), rest, a, l, so)
      case _                                    => createList(fs:_*)
    }
  }

  /** @return true if the Fragment is a Text */
  def isText: Function[Fragment, Boolean] = { case Text(_) => true; case _ => false }
  /** @return the text if the Fragment is a Text */
  def isSomeText: PartialFunction[Fragment, Text] = { case t @ Text(_) => t }
  /** @return true if the Fragment is an Example */
  def isExample: Function[Fragment, Boolean] = { case Example(_, _) => true; case _ => false }
  /** @return the example if the Fragment is an Example */
  def isAnExample: PartialFunction[Fragment, Example] = { case e @ Example(_,_) => e }
  /** @return true if the Fragment is a step */
  def isStep: Function[Fragment, Boolean] = { case Step(_) => true; case _ => false }
  /** @return true if the Fragment is an Example or a Step */
  def isExampleOrStep: Function[Fragment, Boolean] = (f: Fragment) => isExample(f) || isStep(f)
  /** @return the step if the Fragment is a Step*/
  def isAStep: PartialFunction[Fragment, Step] = { case s @ Step(_) => s }

  /** @return a Fragments object with the appropriate name set on the SpecStart fragment */
  def withSpecName(fragments: Fragments, name: SpecName): Fragments = fragments.specTitleIs(name)
  
  /**
   * @return a Fragments object with the appropriate name set on the SpecStart fragment
   *
   * That name is derived from the specification structure name
   */
  def withSpecName(fragments: Fragments, s: SpecificationStructure): Fragments = withSpecName(fragments, SpecName(s))

  /**
   * Fragments can be added as a monoid
   */
  implicit def fragmentsIsMonoid = new Monoid[Fragments] {
    val zero = new Fragments()
    def append(s1: Fragments, s2: => Fragments) = s1 add s2
  }

  /**
   * @return true if 2 fragments are the corresponding spec start and spec end
   */
  def startMatchEnd = (s: Fragment, e: Fragment) => (s, e) match {
    case (SpecStart(ns,_,_,_), SpecEnd(ne)) => ns == ne
    case other => false
  }

}

