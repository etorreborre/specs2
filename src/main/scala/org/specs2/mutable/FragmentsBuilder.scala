package org.specs2
package mutable
import execute._
import main._
import specification.{Action, Step, SpecificationStructure, FormattingFragments, Fragments, FragmentsFragment, Example, GivenThen}
import FormattingFragments._

/**
 * Adding new implicits to support specs-like naming: "the system" should "do this" in { ... }
 *
 * This trait belongs to the mutable package because it works by mutating a local variable each time a new Fragment
 * is created.
 *
 */
trait FragmentsBuilder extends specification.FragmentsBuilder with ExamplesFactory {
  /** local mutable contents of the specification */
  protected[specs2] var specFragments: Fragments = new Fragments()

  /** @return a Fragments object from a single piece of text */
  override implicit def textFragment(s: String): FragmentsFragment = {
    val t = textStart(s)
    addFragments(t)
    t
  }

  implicit def text(s: String): MutableSpecText = new MutableSpecText(s)
  class MutableSpecText(s: String) {
    /** md stands for Markdown */
    def md = textFragment(s)
  }

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
    def in[T <% Result](r: =>T): Example = exampleFactory.newExample(s, r)
    def in[T <% Result](f: String => T): Example = exampleFactory.newExample(s, f(s))
    def >>[T <% Result](r: =>T): Example = in(r)
    def >>[T <% Result](f: String => T): Example = in(f)
    def in(gt: GivenThen): Example = exampleFactory.newExample(s, gt)
    def >>(gt: GivenThen): Example = exampleFactory.newExample(s, gt)

    def >>(e: =>Example)       : Example = in(e)
    def >>(block: =>Unit)      : Unit    = in(block)

    def in(e: =>Example)       : Example = {
      addFragments(s)
      val ex = e
      addFragments(p)
      ex
    }
    def in(block: =>Unit)       : Unit = {
      addFragments(s)
      val b = block
      addFragments(p)
      b
    }
    def in(fs: =>Fragments): Fragments = fs
    def >>(fs: =>Fragments): Fragments = fs
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
  override def link(s: SpecificationStructure) = addFragments(super.link(s))
  override def see(s: SpecificationStructure) = addFragments(super.see(s))

  protected def addFragments[T](s: String, fs: =>T, word: String): Fragments = {
    addFragments(s + " " + word)
    fs
    addFragments(p)
  }
  protected def >>(fs: Fragments) = addFragments(fs)
  protected def addFragments(fs: Fragments): Fragments = {
    specFragments = new FragmentsFragment(specFragments) ^ fs
    fs
  }
  protected def addArguments(a: Arguments): Arguments = {
    specFragments = new FragmentsFragment(specFragments) ^ a
    a
  }

  protected def addExample[T <% Result](ex: =>Example): Example = {
    val example = ex
    specFragments = new FragmentsFragment(specFragments) ^ example
    example
  }

}

/**
 * This trait can be used to deactivate implicits for building fragments
 */
trait NoFragmentsBuilder extends FragmentsBuilder {
  override def described(s: String): Described = super.described(s)
  override def inExample(s: String): InExample = super.inExample(s)
  override def title(s: String)                = super.title(s)
  override def text(s: String)                  = super.text(s)
}
