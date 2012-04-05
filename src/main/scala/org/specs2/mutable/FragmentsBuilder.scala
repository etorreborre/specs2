package org.specs2
package mutable
import execute._
import main._
import specification.{Action, Step, SpecificationStructure, FormattingFragments => FF, Fragments, FragmentsFragment, Example, GivenThen}
import specification.RegexStep._

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
    def txt = textFragment(s)
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
      addFragments(FF.p)
      ex
    }
    def in(block: =>Unit)       : Unit = {
      addFragments(s)
      val b = block
      addFragments(FF.p)
      b
    }
    def in(fs: =>Fragments): Fragments = fs
    def >>(fs: =>Fragments): Fragments = fs
  }

  /**
   * adding a conflicting implicit to warn the user when a `>>` was forgotten
   */
  implicit def `***If you see this message this means that you've forgotten an operator after the description string: you should write "example" >> result ***`(s: String): WarningForgottenOperator = new WarningForgottenOperator(s)
  class WarningForgottenOperator(s: String) {
    def apply[T <% Result](r: =>T): Example = sys.error("there should be a compilation error!")
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

  /**
   * Create GWT fragments with the << syntax for a mutable specification
   */
  implicit def gwtToFragment(s: String): GWTToFragment = new GWTToFragment(s)
  class GWTToFragment(s: String) {
    def <<(u: =>Unit): Step = createStep(s, u)
    def <<(f: Function[String, Unit]): Fragments = createStep(s, f(extract1(s)))
    def <<(f: Function2[String, String, Unit]): Fragments = createStep(s, f.tupled(extract2(s)))
    def <<(f: Function3[String, String, String, Unit]): Fragments = createStep(s, f.tupled(extract3(s)))
    def <<(f: Function4[String, String, String, String, Unit]): Fragments = createStep(s, f.tupled(extract4(s)))
    def <<(f: Function5[String, String, String, String, String, Unit]): Fragments = createStep(s, f.tupled(extract5(s)))
    def <<(f: Function6[String, String, String, String, String, String, Unit]): Fragments = createStep(s, f.tupled(extract6(s)))
    def <<(f: Function7[String, String, String, String, String, String, String, Unit]): Fragments = createStep(s, f.tupled(extract7(s)))
    def <<(f: Function8[String, String, String, String, String, String, String, String, Unit]): Fragments = createStep(s, f.tupled(extract8(s)))
    def <<(f: Function9[String, String, String, String, String, String, String, String, String, Unit]): Fragments = createStep(s, f.tupled(extract9(s)))
    def <<(f: Function10[String, String, String, String, String, String, String, String, String, String, Unit]): Fragments = createStep(s, f.tupled(extract10(s)))

    def <<[R <% Result](r: =>R): Example = createExample(s, r)
    def <<[R <% Result](f: Function[String, R]): Example = createExample(s, f(extract1(s)))
    def <<[R <% Result](f: Function2[String, String, R]): Example = createExample(s, f.tupled(extract2(s)))
    def <<[R <% Result](f: Function3[String, String, String, R]): Example = createExample(s, f.tupled(extract3(s)))
    def <<[R <% Result](f: Function4[String, String, String, String, R]): Example = createExample(s, f.tupled(extract4(s)))
    def <<[R <% Result](f: Function5[String, String, String, String, String, R]): Example = createExample(s, f.tupled(extract5(s)))
    def <<[R <% Result](f: Function6[String, String, String, String, String, String, R]): Example = createExample(s, f.tupled(extract6(s)))
    def <<[R <% Result](f: Function7[String, String, String, String, String, String, String, R]): Example = createExample(s, f.tupled(extract7(s)))
    def <<[R <% Result](f: Function8[String, String, String, String, String, String, String, String, R]): Example = createExample(s, f.tupled(extract8(s)))
    def <<[R <% Result](f: Function9[String, String, String, String, String, String, String, String, String, R]): Example = createExample(s, f.tupled(extract9(s)))
    def <<[R <% Result](f: Function10[String, String, String, String, String, String, String, String, String, String, R]): Example = createExample(s, f.tupled(extract10(s)))

    private def createStep(s: String, u: =>Unit) = {
      strip(s).txt
      step(u)
    }
    private def createExample[R <% Result](s: String, r: =>R) = {
      forExample(strip(s)) ! r
    }
  }


  protected def addFragments[T](s: String, fs: =>T, word: String): Fragments = {
    addFragments(s + " " + word)
    fs
    addFragments(FF.p)
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
  override def text(s: String)                 = super.text(s)
}
