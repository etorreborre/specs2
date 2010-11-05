package org.specs2
package specification

import control.LazyParameters._
import execute._
import main._
import matcher._

private[specs2]
trait FragmentsBuilder {
  /** 
   * This method allows to add a title to the Specification
   * @return a SpecStart object from a string 
   */
  def title(s: String): SpecStart = SpecStart(s)
  
  /** @return a Fragments object from a single Fragment */
  implicit def fragments(f: Fragment): Fragments = Fragments(f)
  /** @return a Fragments object from a string */
  implicit def textStart(s: String): Fragments = Fragments(Text(s))
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
  
  class ArgumentsFragment(a: Arguments) {
    def ^(f: Fragment) = Fragments(List(f))(a)
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
