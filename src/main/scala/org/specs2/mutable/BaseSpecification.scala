package org.specs2
package mutable
import execute._
import main._
import specification._
import FormattingFragments._

/**
 * Adding new implicits to support specs-like naming: "the system" should "do this" in { ... }
 *
 * This trait belongs to the mutable package because it works by mutating a local variable each time a new Fragment
 * is created.
 *
 */
trait BaseSpecification extends specification.BaseSpecification {
  /** local mutable contents of the specification */
  private var specFragments: Fragments = new Fragments()

  def is = specFragments

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
   * add a new example using the 'in' or the '>>' method
   */
  implicit def inExample(s: String): InExample = new InExample(s)
  class InExample(s: String) {
    def in[T <% Result](r: =>T) = addExample(s, r)
    def >>[T <% Result](r: =>T) = in(r)
    def >>(e: =>Example) = addFragments(s, e, "")
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

  /**
   * include other specifications
   */
  override def include(s: SpecificationStructure): FragmentsFragment = {
    val fs = s.content
    addFragments(fs)
    super.include(fs)
  }

  override def include(args: Arguments, s: SpecificationStructure): FragmentsFragment = include(s.content.overrideArgs(args))

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
  protected def addExample[T <% Result](s: String, r: =>T): Example = {
    val ex = s ! r
    specFragments = specFragments ^ ex
    ex
  }

}