package org.specs2
package specification

import control._
import Exceptions._
import LazyParameters._
import main.Arguments
import execute._
import text._
import text.Trim._

/**
 * A Fragment is a piece of a specification. It can be a piece of text, an action or
 * an Example
 */
sealed trait Fragment {
  val linkedTo: Option[SpecificationStructure] = None
  def matches(s: String) = true
}

/**
 * Start of a specification.
 *
 * This fragment keeps 2 important pieces of information:
 *
 *  * the name of the specification (which is derived from the specification class or from a user-defined title)
 *    That name stores a unique id for the specification
 *  * the arguments for that specification
 */
case class SpecStart(name: SpecName, arguments: Arguments = Arguments()) extends Fragment {
  override def matches(s: String) = name matches s
  override def toString = "SpecStart("+name.name+")"
  def withArgs(args: Arguments) = SpecStart(name, args)

  /**
   * The name of the specification can be overriden with a user defined title
   */
  def withName(n: SpecName) = copy(name = name.overrideWith(n))
  def overrideArgs(args: Arguments) = SpecStart(name, arguments.overrideWith(args))
}
object SpecStart {
  def apply(name: String): SpecStart  = new SpecStart(SpecName(name))
  def apply(name: String, args: Arguments): SpecStart = new SpecStart(SpecName(name), args)
}

/**
 * End of a specification.
 *
 * This marks the end of the Specification and must have the same name as the corresponding SpecStart.
 */
case class SpecEnd(name: SpecName) extends Fragment {
  override def matches(s: String) = name matches s
  def withName(n: SpecName) = SpecEnd(n)
}
object SpecEnd {
  def apply(name: String): SpecEnd = new SpecEnd(SpecName(name))
}

/**
 * Free text, describing the system to specify
 */
case class Text(t: String) extends Fragment {
  override def matches(s: String) = t.matches(s)
}

/**
 * A Example is:
 *
 * * a description: some text, with possibly some markup annotations for rendering code fragments (used in AutoExamples)
 * * a body: some executable code returning a Result
 */
case class Example private[specification] (desc: MarkupString = NoMarkup(""), body: () => Result) extends Fragment with Executable {
  def execute = body()
  override def matches(s: String) = desc.toString.removeAll("\n").matches(s)
  override def toString = "Example("+desc+")"

  override def equals(a: Any) = {
    a match {
      case e: Example => desc == e.desc
      case _          => false
    }
  }
}
case object Example {
  def apply[T <% Result](desc: String, body: =>T) = new Example(NoMarkup(desc), () => body)
  def apply[T <% Result](markup: MarkupString, body: =>T) = new Example(markup, () => body)
}

/**
 * An Step creates a fragment that will either return an
 * Error Result if there is an exception or a Success.
 *
 * It is usually used to do some initialisation or cleanup before or after all
 * the Fragments.
 *
 * Note that a Step fragment will not be reported in the output.
 *
 * @see the ContextSpec specification
 *
 */
case class Step (step: LazyParameter[Result] = lazyfy(Success())) extends Fragment with Executable {
  def execute = step.value
  override def toString = "Step"
}
case object Step {
  def apply(r: =>Any) = new Step(lazyfy(trye(r)(Error(_)).left.getOrElse(Success())))
}
/**
 * An Action is similar to a Step but can be executed concurrently with other examples.
 *
 * It is only reported in case of a failure
 */
case class Action (action: LazyParameter[Result] = lazyfy(Success())) extends Fragment with Executable {
  def execute = action.value
  override def toString = "Action"
}
case object Action {
  def apply(r: =>Any) = new Action(lazyfy(trye(r)(Error(_)).left.getOrElse(Success())))
}

/**
 * A link to another specification
 */
case class See(link: HtmlLink) extends Fragment

/**
 * Those standard Fragments are used to format the specification text:
 *  * End() can be used to "reset" the indentation of text
 *  * Br() can be used to insert a newline
 *  * Tab() can be used to increment the indentation level
 *  * Backtab() can be used to decrement the indentation level
 */
private[specs2]
object StandardFragments {
  case class End() extends Fragment
  case class Br() extends Fragment
  case class Tab(n: Int = 1) extends Fragment
  case class Backtab(n: Int = 1) extends Fragment
}

/**
 * Those fragments are used to tag other fragments in a specification\
 */
object TagsFragments {
  trait TaggingFragment extends Fragment {
    val names: Seq[String]
    def keep(args: Arguments): Boolean = isIncluded(args) && !isExcluded(args)
    def isIncluded(args: Arguments) = args.include.isEmpty || !args.include.split(",").intersect(names).isEmpty
    def isExcluded(args: Arguments) = !args.exclude.isEmpty && !args.exclude.split(",").intersect(names).isEmpty
  }
  /** tags the next fragment */
  case class Tag(names: String*) extends TaggingFragment {
    override def toString = names.mkString("Tag(", ",", ")")
  }
  /** tags the previous fragment */
  case class TaggedAs(names: String*) extends TaggingFragment {
    override def toString = names.mkString("TaggedAs(", ",", ")")
  }
  /** the previous fragment starts a section */
  case class AsSection(names: String*) extends TaggingFragment {
    override def toString = names.mkString("AsSection(", ",", ")")
  }
  /** the next fragment starts a section */
  case class Section(names: String*) extends TaggingFragment {
    override def toString = names.mkString("Section(", ",", ")")
  }
}
