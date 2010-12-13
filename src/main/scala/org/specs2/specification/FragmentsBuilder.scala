package org.specs2
package specification

import control.LazyParameters._
import execute._
import main._
import matcher._
import PredefinedFragments._

/**
 * This trait provides function to create specification Fragments:
 *  * a SpecStart fragment with title
 *  * a Text fragment with text
 *  * an Example fragment with a body
 *  * a group of fragments (when including another specification for example)
 * 
 */
private[specs2]
trait FragmentsBuilder {
  /** 
   * This method allows to add a title to the Specification
   * It can be used as an operation on a String: `"spec title".title`
   * @return a SpecStart object from a string 
   */
  implicit def title(s: String): SpecTitle = SpecTitle(s)
  case class SpecTitle(name: String) {
    def title = SpecStart(name)
  }
  
  /** @return a Text Fragment from a string */
  implicit def textFragment(s: String): Text = Text(s)
  /** @return a Group of Fragments from an existing Fragments object */
  implicit def fragmentGroup(Fragments: Fragments) = Group(Fragments.fragments)
  /** @return a Group of Fragments from an existing Fragments Seq */
  implicit def fragmentGroup(fragments: Seq[Fragment]) = Group(fragments)

  /** 
   * @return an Example description from a string, to create a full Example once the
   *         body is defined 
   */
  implicit def forExample(s: String): ExampleDesc = new ExampleDesc(s)
  /** transient class to hold an example description before creating a full Example */
  class ExampleDesc(s: String) {
    def ![T](function: String => MatchResult[T]) = Example(s, function(s).toResult)
	  def ![T](t: =>MatchResult[T]) = Example(s, t.toResult)
	  def ![T <% Result](t: =>T) = Example(s, t)
  }

  /** 
   * @return an Arguments Fragment which allows to insert Arguments before all other
   *         fragments
   */
  implicit def argumentsFragment(a: Arguments) = new ArgumentsFragment(a)
  /** this class allows to start a Fragment list with an Arguments object */
  class ArgumentsFragment(a: Arguments) {
    def ^(f: Fragment) = Fragments(List(f))(a)
    def ^(f: Fragments) = f ^ a
  }
  
  /** 
   * Additional implicits to create a Fragments object from a simple String
   * or an existing Fragment. This allows further chaining with the ^ method 
   */
  /** @return a Fragments object from a single Fragment */
  implicit def fragments(f: Fragment): Fragments = Fragments(f)
  /** @return a Fragments object from a string */
  implicit def textStart(s: String): Fragments = Fragments(Text(s))

  /**
   * This implicits allows to create links to other specifications
   * @see org.specs2.UserGuide
   */
  implicit def stringToHtmlLinkFragments(s: String): HtmlLinkFragments = new HtmlLinkFragments(HtmlLink("", s, "", "", "", Success()))
  class HtmlLinkFragments(link: HtmlLink) {
    def ~(p: (String, SpecificationStructure)) =
      See(HtmlLink(p._2, link.beforeText, p._1)) ^ fragmentGroup(p._2.content)

    def ~(p: (String, SpecificationStructure, String)) =
      See(HtmlLink(p._2, link.beforeText, p._1, p._3)) ^ fragmentGroup(p._2.content)

    def ~(p: (String, SpecificationStructure, String, String)) =
      See(HtmlLink(p._2, link.beforeText, p._1, p._3, p._4)) ^ fragmentGroup(p._2.content)
  }
}

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
