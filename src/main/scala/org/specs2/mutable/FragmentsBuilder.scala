package org.specs2
package mutable
import execute._
import main._
import specification.RegexStep._
import specification.{FormattingFragments => FF, _}

/**
 * Adding new implicits to support specs-like naming: "the system" should "do this" in { ... }
 *
 * This trait belongs to the mutable package because it works by mutating a local variable each time a new Fragment
 * is created.
 *
 */
trait FragmentsBuilder extends specification.FragmentsBuilder with ExamplesFactory with SideEffectingBlocks {
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
    def should(fs: =>Fragment) = addFragments(s, fs, "should")
    def can(fs: =>Fragment) = addFragments(s, fs, "can")
  }
  /**
   * add a new example using 'in' or '>>' or '!'
   */
  implicit def inExample(s: String): InExample = new InExample(s)
  /** transient class to hold an example description before creating a full Example */
  class InExample(s: String) {
    def in[T <% Result](r: =>T): Example         = exampleFactory.newExample(s, r)
    def in[T <% Result](f: String => T): Example = exampleFactory.newExample(s, f(s))
    def >>[T <% Result](r: =>T): Example         = in(r)
    def >>[T <% Result](f: String => T): Example = in(f)
    def in(gt: GivenThen): Example               = exampleFactory.newExample(s, gt)
    def >>(gt: GivenThen): Example               = exampleFactory.newExample(s, gt)

    def >>[T <: Fragment](e: =>T): T         = in(e)
    def >>(block: =>Unit)        : Unit      = in(block)
    def >>(block: =>NameSpace)   : NameSpace = in(block)

    def in[T <: Fragment](block: =>T): T  = addSideEffectingBlock(block)
    def in(block: =>NameSpace): NameSpace = addSideEffectingBlock(block)
    def in(block: =>Unit): Unit           = addSideEffectingBlock(block)

    private def addSideEffectingBlock[T](block: =>T): T = {
      addFragments(s)
      startBlock
      val b = block
      endBlock
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
   * add a new stopOnFail step to the Fragments
   */
  def step(stopOnFail: Boolean = false) = {
    val newStep = Step(stopOnFail = stopOnFail)
    addFragments(newStep)
    newStep
  }
  /**
   * add a new link to the Fragments
   */
  override def link(fss: Seq[Fragments]): Fragments               = addFragments(super.link(fss))
  override def link(htmlLink: HtmlLink, fs: Fragments): Fragments = addFragments(super.link(htmlLink, fs))
  override def see(fss: Seq[Fragments]): Fragments                = addFragments(super.see(fss))
  override def see(htmlLink: HtmlLink, fs: Fragments): Fragments  = addFragments(super.see(htmlLink, fs))

  /**
   * Create GWT fragments with the << syntax for a mutable specification
   */
  implicit def gwtToFragment(s: String): GWTToFragment = new GWTToFragment(s)
  class GWTToFragment(s: String) {
    def <<(given: Given[Unit]): Fragments = createStep(s, given.extract(s))
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
    def <<(f: Seq[String] => Unit)(implicit p: ImplicitParam): Fragments = createStep(s, f(extractAll(s)))

    def <<(then: Then[Unit]): Example = createExample(s, then.extract((), s))
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
    def <<[R](f: Seq[String] => R)(implicit r: R => Result, p: ImplicitParam): Example = createExample(s, f(extractAll(s)))

  }

  private def createStep(s: String, u: =>Unit) = {
    strip(s).txt
    addFragments(FF.bt)
    step(u)
  }
  private def createExample[R <% Result](s: String, r: =>R) = {
    addFragments(FF.t)
    val e = forExample(strip(s)) ! r
    addFragments(FF.bt)
    e
  }

  protected def addFragments[T](s: String, fs: =>T, word: String): Fragments = {
    addFragments(s + " " + word)
    startBlock
    fs
    endBlock
    addFragments(FF.p)
  }

  protected def addFragments(fs: Fragments): Fragments = {
    val element = fs.middle.lastOption.getOrElse((Text("root")))
    addBlockElement(element)
    element match {
      case e @ Example(_,_) => specFragments = new FragmentsFragment(specFragments) ^ e.creationPathIs(creationPath)
      case other            => specFragments = new FragmentsFragment(specFragments) ^ fs
    }
    fs
  }
  protected def addFragments(fs: Seq[Fragment]): Fragments = {
    addFragments(Fragments.createList(fs:_*))
  }
  protected def addArguments(a: Arguments): Arguments = {
    specFragments = new FragmentsFragment(specFragments) ^ a
    a
  }

  protected def addExample[T <% Result](ex: =>Example): Example = {
    val example = ex
    addFragments(Fragments.createList(example))
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

import internal.scalaz.{TreeLoc, Scalaz}
import Scalaz._
import data.Trees._

trait SideEffectingBlocks {

 /**
  * This tree loc contains the "path" of Examples and Actions when they are created in a block creating another fragment
  * For example:
  *
  * "this" should {
  *   "create an example" >> ok
  * }
  *
  * The execution of the block above creates a Text fragment followed by an example. The blocksTree tree tracks the order of creation
  * so that we can attach a "block creation path" to the Example showing which fragment creation precedes him. This knowledge is used to
  * run a specification in the "isolation" mode were the changes in local variables belonging to blocks are not seen by
  * other examples
  */
 private[specs2] var blocksTree: TreeLoc[(Int, Any)] = leaf((0, Text("root"))).loc

  /** @return the Tree of creation paths */
  private[specs2] def blocksCreationTree = blocksTree.toTree.map(_._1)

  /** @return the current path to root */
  private[specs2] def creationPath = blocksTree.lastChild.getOrElse(blocksTree).map(_._1).path.reverse.toIndexedSeq

  private def nextNodeNumber = blocksTree.lastChild.map(_.getLabel._1 + 1).getOrElse(0)

  def startBlock {
    blocksTree = blocksTree.lastChild.getOrElse(blocksTree)
  }
  def endBlock {
    blocksTree = blocksTree.getParent
  }

  def addBlockElement(e: Any) {
    blocksTree = blocksTree.addChild((nextNodeNumber, e))
  }

}