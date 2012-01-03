package org.specs2
package specification

import execute._
import main._

/**
 * This trait provides function to create specification Fragments:
 *  - a SpecStart fragment with title
 *  - a Text fragment with text
 *  - an Example fragment with a body
 *  - a group of fragments (when including another specification for example)
 * 
 */
private[specs2]
trait FragmentsBuilder extends RegexSteps with ExamplesFactory { outer =>

  /**
   * Methods for chaining fragments
   */

  /** @return a Fragments object from a single Fragment */
  implicit def fragments(f: =>Fragment): Fragments = Fragments.createList(f)
  implicit def fragmentFragments(f: =>Fragment): FragmentsFragment = new FragmentsFragment(fragments(f))
  implicit def fragmentsFragments(fs: =>Fragments): FragmentsFragment = new FragmentsFragment(fs)
  /** reverse conversion from a Fragment containing a Fragments object to the Fragments object*/
  implicit def fragmentsFragmentToFragments(fs: FragmentsFragment): Fragments = fs.fragments

  /**
   * Methods for creating fragments
   */
  /** @return a Fragments object from a string */
  implicit def textStart(s: String): Fragments = Fragments.createList(Text(s))
  /** @return create a Text Fragment from a string and allow it to be chained to other fragments */
  implicit def textFragment(s: String): FragmentsFragment = textStart(s)
  /**
   * This method allows to add a title to the Specification. It can be used as an operation on a String:
   * `"spec title".title`
   *
   * @return a Fragments object from a string, with the given title 
   */
  implicit def title(s: String): SpecTitle = new SpecTitle(s)
  class SpecTitle(name: String) {
    def title = Fragments(SpecName(name))
  }
  /**
   * Example creation
   * @return an Example description from a string, to create a full Example once the body is defined
   */
  implicit def forExample(s: String): ExampleDesc = new ExampleDesc(s)
  /** transient class to hold an example description before creating a full Example */
  class ExampleDesc(s: String) {
    /** @return an Example, using anything that can be translated to a Result, e.g. a Boolean */
	  def ![T <% Result](t: =>T): Example = exampleFactory.newExample(s, t)
    /** @return an Example, using the example description */
	  def ![T <% Result](f: String => T): Example = exampleFactory.newExample(s, f(s))
    /** @return an Example which a function using values extracted from the text */
	  def !(gt: GivenThen): Example = exampleFactory.newExample(s, gt)
  }

  /**
   * Arguments creation
   * @return a Fragments object which can be chained with other fragments
   */
  implicit def argumentsFragment(a: Arguments): FragmentsFragment = new Fragments().overrideArgs(a)

  /**
   * Links to other specifications creation
   * 
   * @see org.specs2.UserGuide
   */
  implicit def stringToHtmlLinkFragments(s: String): HtmlLinkFragments = new HtmlLinkFragments(HtmlLink(SpecName(""), s, "", "", ""))
  class HtmlLinkFragments(link: HtmlLink) {
    def ~(s: SpecificationStructure) = outer.link(HtmlLink(s.content.specName, "", link.beforeText), s)
    def ~(p: (String, SpecificationStructure)) = outer.link(HtmlLink(p._2.content.specName, link.beforeText, p._1), p._2)
    def ~(p: (String, SpecificationStructure, String)) = outer.link(HtmlLink(p._2.content.specName, link.beforeText, p._1, p._3), p._2)
    def ~(p: (String, SpecificationStructure, String, String)) = outer.link(HtmlLink(p._2.content.specName, link.beforeText, p._1, p._3, p._4), p._2)
    def ~/(s: SpecificationStructure) = outer.see(HtmlLink(s.content.specName, "", link.beforeText), s)
    def ~/(p: (String, SpecificationStructure)) = outer.see(HtmlLink(p._2.content.specName, link.beforeText, p._1), p._2)
    def ~/(p: (String, SpecificationStructure, String)) = outer.see(HtmlLink(p._2.content.specName, link.beforeText, p._1, p._3), p._2)
    def ~/(p: (String, SpecificationStructure, String, String)) = outer.see(HtmlLink(p._2.content.specName, link.beforeText, p._1, p._3, p._4), p._2)
  }

  implicit def stringToHtmlLinkFragments2(s: String): HtmlLinkFragments2 = new HtmlLinkFragments2(HtmlLink(SpecName(""), s, "", "", ""))
  class HtmlLinkFragments2(link: HtmlLink) {
    def ~(p: (SpecificationStructure, String)) = outer.link(HtmlLink(p._1.content.specName, "", link.beforeText, p._2), p._1)
    def ~(p: (SpecificationStructure, String, String)) = outer.link(HtmlLink(p._1.content.specName, "", link.beforeText, p._2, p._3), p._1)
    def ~/(p: (SpecificationStructure, String)) = outer.see(HtmlLink(p._1.content.specName, "", link.beforeText, p._2), p._1)
    def ~/(p: (SpecificationStructure, String, String)) = outer.see(HtmlLink(p._1.content.specName, "", link.beforeText, p._2, p._3), p._1)
  }

  /** create a link directly on a specification, with a given link */
  def link(s: SpecificationStructure): Fragments = link(HtmlLink(s), s)
  /** create a link directly on a specification, with a given link */
  def link(htmlLink: HtmlLink, s: SpecificationStructure): Fragments = s.content.linkIs(htmlLink)

  /** create a html link without including the other specification fragments */
  def see(s: SpecificationStructure): Fragments = see(HtmlLink(s),  s)
  /** create a html link without including the other specification fragments, and passing a specific link */
  def see(htmlLink: HtmlLink, s: SpecificationStructure): Fragments = s.content.seeIs(htmlLink)

  /** transform a scope to a success to be able to create traits containing any variables and usable in any Examples */
  implicit def inScope(s: Scope): Success = Success()
}

/**
 * This trait can be used to deactivate implicits for building fragments
 */
trait NoFragmentsBuilder extends FragmentsBuilder {
  override def textStart(s: String): Fragments = super.textStart(s)
  override def textFragment(s: String): FragmentsFragment = super.textFragment(s)
  override def title(s: String): SpecTitle = super.title(s)
  override def stringToHtmlLinkFragments(s: String): HtmlLinkFragments = super.stringToHtmlLinkFragments(s)
  override def stringToHtmlLinkFragments2(s: String): HtmlLinkFragments2 = super.stringToHtmlLinkFragments2(s)
}

/**
 * Fragments can be chained with the ^ method
 */
private[specs2]
class FragmentsFragment(fs: =>Fragments) {
  def fragments = fs
  def ^(t: String) = fs add Text(t)
  def ^(f: Fragment) = f match {
    case s @ SpecStart(_,_,_,_) => (fs specTitleIs s.specName).overrideArgs(s.arguments)
    case _                      => fs add f
  }
  def ^(other: Seq[Fragment]) = fs add other

  def ^(other: Fragments) = {
    other match {
      case Fragments(t, m, a, Some(l), so)    => fs add other.fragments
      case Fragments(Some(t), m, a, None, so) => (fs add other.middle).specTitleIs(t).overrideArgs(a)
      case Fragments(None, m, a, None, so)    => (fs add other.middle).overrideArgs(a)
      case _                                  => fs add other.middle
    }
  }

  def ^(other: FragmentsFragment) = fs add other.fragments
  def ^(a: Arguments) = fs add a

  /** start a given-when-then block */
  def ^[T](step: Given[T]): PreStep[T] = {
    val text = fs.fragments.collect { case t: Text => t.t }.lastOption.getOrElse("A Text must precede a Given object!")
    lazy val extracted = step.extractContext(text)
    def strip(fragments: Fragments) = fragments.map(step.strip)
    new PreStep(() => extracted, new FragmentsFragment(strip(fs)) ^ Step.fromEither(extracted))
  }
}


object FragmentsBuilder extends FragmentsBuilder

import org.specs2.internal.scalaz._
/**
 * Implementation of the Show trait to display Fragments
 */
private[specs2]
trait FragmentsShow {
  implicit object showFragments extends Show[Fragment] {
	  def show(f: Fragment) = f.toString.toList
  }
}

private[specs2]
object FragmentsShow extends FragmentsShow
