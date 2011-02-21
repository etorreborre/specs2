package org.specs2
package mutable
import execute._
import main._
import specification._
import FormattingFragments._
import matcher.MatchResult

/**
 * Adding new implicits to support specs-like naming: "the system" should "do this" in { ... }
 *
 * This trait belongs to the mutable package because it works by mutating a local variable each time a new Fragment
 * is created.
 *
 */
trait FragmentsBuilder extends specification.FragmentsBuilder {
  /** local mutable contents of the specification */
  protected[specs2] var specFragments: Fragments = new Fragments()

  override implicit def title(s: String): MutableSpecTitle = new MutableSpecTitle(s)
  class MutableSpecTitle(s: String) extends SpecTitle(s) {
    override def title = addFragments(super.title)
  }

  implicit def described(s: String): Described = new Described(s)
  class Described(s: String) {
    def should(fs: =>Example) = addFragments(s, fs, "should")
    def can(fs: =>Example) = addFragments(s, fs, "can")
  }
  /**
   * add a new example using 'in' or '>>' or '!'
   */
  implicit def inExample(s: String): InExample = new InExample(s)
  /** transient class to hold an example description before creating a full Example */
  class InExample(s: String) {
    def in[T <% Result](r: =>T): Example = addExample(s, r)
    def >>[T <% Result](r: =>T): Example = in(r)
    def >>(e: =>Example)       : Example = addExample(e)
    def in(e: =>Example)       : Example = addExample(e)
  }

  override implicit def forExample(s: String): ExampleDesc = new ExampleDesc(s, mutableExampleFactory)
  
  private val mutableExampleFactory = new ExampleFactory {
    def newExample[T <% Result](s: String, function: String => T): Example = addExample(s, function(s))
	  def newExample[T](s: String, t: =>MatchResult[T]): Example             = addExample(s, t.toResult)
	  def newExample[T <% Result](s: String, t: =>T): Example                = addExample(s, t)
  }

  /**
   *  add a new action to the Fragments
   */
  def action(a: =>Any) = {
    val newAction = Action(a)
    addFragments(newAction)
    newAction
  }
  /**
   * add a new step to the Fragments
   */
  def step(a: =>Any) = {
    val newStep = Step(a)
    addFragments(newStep)
    newStep
  }
  /**
   * add a new link to the Fragments
   */
  def link(f: Fragments) = addFragments(f)

  protected def addFragments[T](s: String, fs: =>T, word: String): Unit = {
    addFragments(s + " " + word)
    fs
    addFragments(p)
  }
  protected def addFragments(fs: Fragments): Fragments = {
    specFragments = specFragments ^ fs
    fs
  }
  protected def addArguments(a: Arguments): Arguments = {
    specFragments = specFragments ^ a
    a
  }
  protected def addExample[T <% Result](s: String, r: =>T): Example = addExample(Example(s, r))

  protected def addExample[T <% Result](ex: Example): Example = {
    specFragments = specFragments ^ ex
    ex
  }

}

