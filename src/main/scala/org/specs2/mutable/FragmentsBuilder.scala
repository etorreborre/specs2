package org.specs2
package mutable

import execute._
import main._
import text.RegexExtractor
import RegexExtractor._
import specification.{FormattingFragments => FF, _}
import StandardResults._
import control.ImplicitParameters
import control.Functions._

/**
 * Adding new implicits to support specs-like naming: "the system" should "do this" in { ... }
 *
 * This trait belongs to the mutable package because it works by mutating a local variable each time a new Fragment
 * is created.
 *
 */
trait FragmentsBuilder extends specification.FragmentsBuilder
  with ExamplesFactory
  with SideEffectingCreationPaths
  with ImplicitParameters {

  /** local mutable contents of the specification */
  protected[mutable] var specFragments: Fragments = Fragments.createList()
  protected[specs2] def fragments: Fragments = { replay; specFragments }

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
    def can(fs: =>Fragment)    = addFragments(s, fs, "can")

    def should(fs: =>Fragments)(implicit p: ImplicitParam) = addFragments(s, fs, "should")
    def can(fs: =>Fragments)(implicit p: ImplicitParam)    = addFragments(s, fs, "can")
  }
  /**
   * add a new example using 'in' or '>>' or '!'
   */
  implicit def inExample(s: String): InExample = new InExample(s)
  /** transient class to hold an example description before creating a full Example */
  class InExample(s: String) {
    def in[T : AsResult](r: =>T): Example = {
      val example = exampleFactory.newExample(s, r)
      example
    }
    def >>[T : AsResult](r: =>T): Example = in(r)

    def in[T : AsResult](f: String => T): Example = exampleFactory.newExample(s, f(s))
    def >>[T : AsResult](f: String => T): Example = in(f)

    def in(block: =>NameSpace)(implicit p1: ImplicitParam1,
                                        p2: ImplicitParam2): Fragments  = addSideEffectingBlock(block)
    def >>(block: =>NameSpace)(implicit p1: ImplicitParam1,
                                        p2: ImplicitParam2): Fragments = in(block)(p1, p2)

    def in[T <: Fragment](block: =>T)(implicit p: ImplicitParam): Fragments = addSideEffectingBlock(block)
    def >>[T <: Fragment](block: =>T)(implicit p: ImplicitParam): Fragments = in(block)(p)

    def in(fs: =>Fragments): Fragments = addSideEffectingBlock(fs)
    def >>(fs: =>Fragments): Fragments = addSideEffectingBlock(fs)

    private def addSideEffectingBlock[T](block: =>T) = {
      addFragments(s)
      executeBlock(block)
      addFragments(FF.bt)
    }
  }

  /** add a block of examples */
  def examplesBlock(u: =>Unit) = {
    executeBlock(u)
    addFragments(FF.t(0))
  }
  /**
   * adding a conflicting implicit to warn the user when a `>>` was forgotten
   */
  implicit def `***If you see this message this means that you've forgotten an operator after the description string: you should write "example" >> result ***`(s: String): WarningForgottenOperator = new WarningForgottenOperator(s)
  class WarningForgottenOperator(s: String) {
    def apply[T : AsResult](r: =>T): Example = sys.error("there should be a compilation error!")
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
  def step(a: =>Any, global: Boolean = false) = {
    val newStep = Step(a).copy(isolable = !global)
    addFragments(newStep)
    newStep
  }
  /**
   * add a new stopOnFail step to the Fragments
   */
  def step(stopOnFail: Boolean) = {
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

  protected def addFragments[T](s: String, fs: =>T, word: String): Fragments = {
    addFragments(s + " " + word)
    executeBlock(fs)
    addFragments(FF.p)
  }

  protected def addFragments(fs: Fragments): Fragments = {
    val element = fs.middle.lastOption.getOrElse((Text("root")))
    addBlockElement(element)
    element match {
      case e: Example => updateSpecFragments(fragments => new FragmentsFragment(fragments) ^ e.creationPathIs(creationPath))
      case a: Action  => updateSpecFragments(fragments => new FragmentsFragment(fragments) ^ a.creationPathIs(creationPath))
      case other      => updateSpecFragments(fragments => new FragmentsFragment(fragments) ^ fs)
    }
    fs
  }
  protected def addFragments(fs: Seq[Fragment]): Fragments = {
    addFragments(Fragments.createList(fs:_*))
  }
  protected def addArguments(a: Arguments): Arguments = {
    updateSpecFragments(fragments => new FragmentsFragment(fragments) ^ a)
    a
  }

  protected def updateSpecFragments(f: Fragments => Fragments) = {
    effect(specFragments = f(specFragments))
  }

  protected def addExample(ex: =>Example): Example = {
    val example = ex
    addFragments(Fragments.createList(example))
    example
  }

}

/**
 * allow to write "this example has several expectations" in { Seq(1, 2, 3).foreach(i => i must be_>(0)) }
 */
trait ExpectationsBlock { this: FragmentsBuilder =>
  /**
   * add a new example using 'in' but ending with a Unit block
   */
  implicit class expectationsUnit(s: String) {
    def in(block: =>Unit): Example = exampleFactory.newExample(s, {block; success})
    def >>(block: =>Unit): Example = in(block)
  }
}
/**
 * allow to write "this block has several examples" in { Seq(1, 2, 3).foreach(i => "example"+i >> ok) }
 */
trait ExamplesBlock { this: FragmentsBuilder =>
  /**
   * add a new example using 'in' but ending with a Unit block
   */
  implicit class examplesUnit(s: String) {
    def in(block: =>Unit) : Unit = { lazy val b = block; >>(new NameSpace { b }); b }
    def >>(block: =>Unit) : Unit = in(block)
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

import scalaz.{TreeLoc, Scalaz, Tree}
import Scalaz._
import Tree._
import data.Trees._
trait SideEffectingCreationPaths extends SpecificationNavigation {

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
  private[mutable] var blocksTree: TreeLoc[(Int, Any)] = leaf((0, Text("root"): Any)).loc

  /** when a target path is specified we might limit the creation of fragments to only the fragments on the desired path */
  private[mutable] var targetPath: Option[CreationPath] = None

  /** list of actions to create fragments */
  private[mutable] var effects: scala.collection.mutable.ListBuffer[() => Unit] = new scala.collection.mutable.ListBuffer()

  /**
   * play all the effects. After each executed effect, new effects might have been created.
   * Push them first at the beginning of the effects list so that they can be played first
   */
  private[mutable] def replay = {
    def targetReached = targetPath.map(_ == creationPath).getOrElse(false)

    while (!effects.isEmpty) {
      val effect = effects.remove(0)
      val rest = effects.take(effects.size)
      effects = new scala.collection.mutable.ListBuffer()
      effect.apply()
      effects.append(rest:_*)
      if (targetReached)
        effects = effects.take(1)
    }
  }

  /** @return the Tree of creation paths */
  private[mutable] def blocksCreationTree = blocksTree.toTree.map(_._1)

  /** @return the current path to root */
  private[mutable] def creationPath = MutableCreationPath(blocksTree.lastChild.getOrElse(blocksTree).map(_._1).path.reverse.toIndexedSeq)

  private def nextNodeNumber = blocksTree.lastChild.map(_.getLabel._1 + 1).getOrElse(0)

  private[mutable] def startBlock {
    effect(blocksTree = blocksTree.lastChild.getOrElse(blocksTree))
  }

  private[mutable] def endBlock {
    effect(blocksTree = blocksTree.getParent)
  }

  private[mutable] def executeBlock[T](block: =>T) = {
    startBlock
    effect {
      targetPath match {
        case Some(path) => if (path.startsWith(creationPath)) block
        case None       => block
      }
    }
    endBlock
  }

  private[mutable] def effect(a: =>Unit) {
    effects.append(() => a)
  }

  private[mutable] def addBlockElement(e: Any) {
    effect(blocksTree = blocksTree.addChild((nextNodeNumber, e)))
  }

  /**
  * @return the list of fragments which have been created before a given one
  */
  private[specs2]
  override def fragmentsTo(f: Fragment): Seq[Fragment] = {
    // set the target path
    targetPath = f match {
      case e: Example => e.creationPath
      case a: Action  => a.creationPath
      case other      => None
    }
    // return the fragments created till all path nodes have been created
    content.fragments
  }
}
