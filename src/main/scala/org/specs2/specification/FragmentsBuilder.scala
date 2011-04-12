package org.specs2
package specification

import control.LazyParameters._
import execute._
import main._
import matcher._
import FormattingFragments._

/**
 * This trait provides function to create specification Fragments:
 *  * a SpecStart fragment with title
 *  * a Text fragment with text
 *  * an Example fragment with a body
 *  * a group of fragments (when including another specification for example)
 * 
 */
private[specs2]
trait FragmentsBuilder extends RegexSteps { outer =>

  /**
   * Methods for chaining fragments
   */

  /** @return a Fragments object from a single Fragment */
  implicit def fragments(f: =>Fragment): Fragments = Fragments.createList(f)
  implicit def fragmentFragments(f: =>Fragment): FragmentsFragment = new FragmentsFragment(fragments(f))
  implicit def fragmentsFragments(fs: =>Fragments): FragmentsFragment = new FragmentsFragment(fs)
  /**
   * Fragments can be chained with the ^ method
   */
  class FragmentsFragment(fs: =>Fragments) {
    private[specs2] def fragments = fs
    def ^(t: String) = fs add Text(t)
    def ^(f: Fragment) = f match {
      case s @ SpecStart(n, a) => fs specTitleIs s
      case _ => fs add f
    }
    def ^(other: Seq[Fragment]) = fs add other
    def ^(other: Fragments) = {
      other.specStart match {
        case Some(s) => (fs add other.middle).specTitleIs(s)
        case _       => fs add other.middle
      }
    }
    def ^(other: FragmentsFragment) = fs add other.fragments
    def ^(a: Arguments) = fs add a

    /** start a given-when-then block */
    def ^[T](step: Given[T]): PreStep[T] = {
      val text = fs.fragments.collect { case t: Text => t.t }.lastOption.getOrElse("A Text must precede a Given object!")
      lazy val extracted = step.extractContext(text)
      def strip(fragments: Fragments) = fragments.map(step.strip)
      new PreStep(() => extracted, fragmentsFragments(strip(fs) ^ Arguments("noindent")) ^ Step.fromEither(extracted))
    }
  }

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
   * @return a SpecStart object from a string 
   */
  implicit def title(s: String): SpecTitle = new SpecTitle(s)
  class SpecTitle(name: String) {
    def title = new Fragments().specTitleIs(SpecStart(SpecName(name)))
  }

  /**
   * Example creation
   * @return an Example description from a string, to create a full Example once the body is defined
   */
  implicit def forExample(s: String): ExampleDesc = new ExampleDesc(s)
  /** transient class to hold an example description before creating a full Example */
  class ExampleDesc(s: String) {
    /** @return an Example, using a function taking the example description as an input */
    def ![T <% Result](function: String => T): Example = exampleFactory.newExample(s, function)
    /** @return an Example, using a match */
	  def ![T](t: =>MatchResult[T]): Example = exampleFactory.newExample(s, t)
    /** @return an Example, using anything that can be translated to a Result, e.g. a Boolean */
	  def ![T <% Result](t: =>T): Example = exampleFactory.newExample(s, t)
  }

  private[specs2] def exampleFactory: ExampleFactory = new DefaultExampleFactory

  /**
   * Arguments creation
   * @return a Fragments object which can be chained with other fragments
   */
  implicit def argumentsFragment(a: Arguments): FragmentsFragment = new FragmentsFragment(new Fragments().overrideArgs(a))

  /**
   * Links to other specifications creation
   * 
   * @see org.specs2.UserGuide
   */
  implicit def stringToHtmlLinkFragments(s: String): HtmlLinkFragments = new HtmlLinkFragments(HtmlLink(SpecName(""), s, "", "", "", Success()))
  class HtmlLinkFragments(link: HtmlLink) {
    def ~(s: SpecificationStructure) =
      See(HtmlLink(s.content.start.name, "", link.beforeText)) ^ s.content.fragments

    def ~(p: (String, SpecificationStructure)) =
      See(HtmlLink(p._2.content.start.name, link.beforeText, p._1)) ^ p._2.content.fragments

    def ~(p: (String, SpecificationStructure, String)) =
      See(HtmlLink(p._2.content.start.name, link.beforeText, p._1, p._3)) ^ p._2.content.fragments

    def ~(p: (String, SpecificationStructure, String, String)) =
      See(HtmlLink(p._2.content.start.name, link.beforeText, p._1, p._3, p._4)) ^ p._2.content.fragments
  }

  implicit def stringToHtmlLinkFragments2(s: String): HtmlLinkFragments2 = new HtmlLinkFragments2(HtmlLink(SpecName(""), s, "", "", "", Success()))
  class HtmlLinkFragments2(link: HtmlLink) {
    def ~(p: (SpecificationStructure, String)) =
      See(HtmlLink(p._1.content.start.name, "", link.beforeText, p._2)) ^ p._1.content.fragments

    def ~(p: (SpecificationStructure, String, String)) =
      See(HtmlLink(p._1.content.start.name, "", link.beforeText, p._2, p._3)) ^ p._1.content.fragments
  }
  /** transform a scope to a success to be able to create traits containing any variables and usable in any Examples */
  implicit def inScope(s: Scope): Success = Success()
}
object FragmentsBuilder extends FragmentsBuilder
/**
 * Implementation of the Show trait to display Fragments
 */
private[specs2]
trait FragmentsShow {
  implicit object showFragments extends scalaz.Show[Fragment] {
	  def show(f: Fragment) = f.toString.toList
  }
}

private[specs2]
object FragmentsShow extends FragmentsShow
