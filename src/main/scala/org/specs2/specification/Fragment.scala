package org.specs2
package specification

import control._
import Exceptions._
import LazyParameters._
import main.Arguments
import execute._
import text._
import Regexes._
import scalaz.Monoid
import io.Location
import scala.Either
import data.SeparatedTags
import TagsFragments._
import scala.xml.NodeSeq

/**
 * A Fragment is a piece of a specification. It can be a piece of text, an action or
 * an Example
 */
sealed trait Fragment {
  def location: Location
  def matches(s: String) = true
}

/**
 * Start of a specification.
 *
 * This fragment keeps 2 important pieces of information:
 *
 *  - the name of the specification (which is derived from the specification class or from a user-defined title)
 *    That name stores a unique id for the specification
 *  - the arguments for that specification
 */
case class SpecStart(specName: SpecName, arguments: Arguments = Arguments(), linked: Linked = Linked(), location: Location = new Location) extends Fragment {

  def name = specName.name
  def title = specName.title
  override def matches(s: String) = name matches s

  override def toString = "SpecStart("+title+linkToString+")"
  def linkToString = linked.linkToString

  /** the new arguments take precedence over the old ones */
  def withArgs(args: Arguments) = copy(arguments = args)
  /** the new arguments take override the old ones where defined */
  def overrideArgs(args: Arguments) = copy(arguments = arguments.overrideWith(args))
  
  /** @return true if this spec starts only contains a link referencing another specification */
  def isSeeOnlyLink = linked.isSeeOnlyLink
  /** @return true if this spec starts only contains a link including another specification */
  def isIncludeLink = linked.isIncludeLink
  /** @return true if this spec starts only contains a link to another specification */
  def isLink        = linked.isLink
  /** @return true if this spec must not be displayed */
  def hidden        = linked.hidden
  /** @return the html link if any */
  def link          = linked.link
  /** The name of the specification can be overriden with a user defined title */
  def withName(n: SpecName) = copy(specName = specName.overrideWith(n))
  /** @return a non-linked start*/
  def unlink = SpecStart(specName, arguments)
  /** set the url for the generated documentation */
  def urlIs(url: String) = copy(specName = specName.urlIs(url), linked = linked.urlIs(url))
  /** set the base directory for the generated documentation */
  def baseDirIs(dir: String) = copy(specName = specName.baseDirIs(dir), linked = linked.baseDirIs(dir))
}

/**
 * End of a specification.
 *
 * This marks the end of the Specification and must have the same name as the corresponding SpecStart.
 *
 * There is a Boolean flag on a SpecEnd indicating if the whole specification was just executed as a link (for an index page for example)
 * In this case we must not store statistics for this specification (see Storing.scala)
 */
case class SpecEnd(specName: SpecName, isSeeOnlyLink: Boolean = false, location: Location = new Location) extends Fragment {
  def name = specName.name
  def title = specName.title
  def seeOnlyLinkIs(s: Boolean) = copy(isSeeOnlyLink = s)

  override def matches(s: String) = name.matchesSafely(s, ".*")
  override def toString = "SpecEnd("+title+")"
}

/**
 * Free text, describing the system to specify
 */
case class Text(text: SimpleFormattedString, location: Location = new Location) extends Fragment {
  def t = text.raw
  override def matches(s: String) = t.matches(s)
  def flow = text.flow
  def add(other: Text) = copy(text.append(other.t))

  override def toString = s"Text($text)"

  override def equals(a: Any) = a match {
    case t: Text => text == t.text
    case _       => false
  }
  override def hashCode = text.hashCode
}
object Text {
  def apply(s: String): Text = new Text(FormattedString(s))
}
trait FormattedString {
  type F <: FormattedString

  def raw: String
  def formatting: Formatting
  def isEmpty: Boolean
  def map(f: String => String): F
  def append(s: String) = map(t => t+s)
  def prepend(s: String) = map(t => s+t)

  def withMarkdown: FormattedString
  def withFlow: FormattedString

  def formatWithTagNames(names: Seq[String]): F
  def flow = formatting.flow
  def toXml: NodeSeq

  override def toString = raw
}
case class SimpleFormattedString(t: String = "", formatting: Formatting = Formatting(), isEmpty: Boolean = false) extends FormattedString {
  type F = SimpleFormattedString

  def raw: String = t
  def map(f: String => String) = copy(t = f(t))

  def withMarkdown = copy(formatting = formatting.copy(markdown = true))
  def withoutMarkdown = copy(formatting = formatting.copy(markdown = false))
  def withFlow = copy(formatting = formatting.copy(flow = true))
  def withoutFlow = copy(formatting = formatting.copy(flow = false))

  def toXml = if (formatting.markdown) <code class="prettyprint">{raw}</code> else if (isEmpty) <t></t> else <t>{raw}</t>

  def formatWithTagNames(names: Seq[String]) = copy(formatting = formatting.fromTagNames(names: Seq[String]))
  override def toString = raw
}
object FormattedString {
  def apply(t: String) = SimpleFormattedString(t)
  def code(t: String) = FormattedString(t).withMarkdown
  def empty = SimpleFormattedString(isEmpty = true)
}
/** Formatting for Text fragments */
case class Formatting(flow: Boolean = false, markdown: Boolean = true, verbatim: Boolean = true) {
 def fromTagNames(names: Seq[String]) = copy(flow = tagValue(names, "flow", flow), markdown = tagValue(names, "markdown", markdown), verbatim = tagValue(names, "verbatim", verbatim))

  private def tagValue(names: Seq[String], name: String, defaultValue: Boolean) = {
    val nameFound         = names.exists(_ == FormattingTags.internal+name)
    val negatedNameFound  = names.exists(_ == "!"+FormattingTags.internal+name)

    if (nameFound && !negatedNameFound) true
    else if (negatedNameFound)          false
    else                                defaultValue
  }

}

/**
 * A Example is:
 *
 * - a description: some text, with possibly some markdown annotations for rendering code fragments (used in AutoExamples)
 * - a body: some executable code returning a Result
 */
case class Example private[specification] (desc: FormattedString = FormattedString(""), body: () => Result,
                                           location: Location = new Location,
                                           isolable: Boolean = true, private[specs2] val creationPath: Option[CreationPath] = None) extends Fragment with Executable with Isolable { outer =>
  def execute = body()

  /**
   * match the description of an example with a regular expression.
   *
   * If the regexp doesn't compile, it is used literally by quoting it. However this regexp is usually passed as the
   * Arguments.ex value, it is enclosed with '.*' characters, so they are removed and added back before quotation
   */
  override def matches(s: String) = desc.toString.matchesSafely(s, enclosing = ".*")

  override def toString = "Example("+desc+")"
  override def map(f: Result => Result) = Example(desc, f(body()))

  override def equals(a: Any) = {
    a match {
      case e: Example => desc == e.desc
      case _          => false
    }
  }

  /** this fragment can not be executed in a separate specification */
  def global = copy(isolable = false)

  /** replace the current formatted string with another one */
  def formatWith(formatted: FormattedString) = copy(desc = formatted)

  /** set a creation path, if not already set, on this example to possibly isolate it during its execution */
  private[specs2]
  def creationPathIs(path: CreationPath) = copy(creationPath = if (outer.creationPath.isDefined) outer.creationPath else Some(path))

}

case object Example {
  def apply[T : AsResult](desc: String, body: =>T) = new Example(FormattedString(desc), () => AsResult(body))
  def apply[T : AsResult](fs: FormattedString, body: =>T) = new Example(fs, () => AsResult(body))
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
case class Step (step: LazyParameter[Result], stopOnFail: Boolean = false, location: Location = new Location, isolable: Boolean = true) extends Fragment with Executable with Isolable {

  def execute = step.value
  override def toString = "Step"

  override def map(f: Result => Result) = Step(step map f)

  /** this fragment can not be executed in a separate specification */
  def global = copy(isolable = false)

  // we must override the case class equality to avoid evaluating the step
  override def equals(any: Any) = any match {
    case s: Step => s eq this
    case _       => false
  }
  // we must override the case class hashCode to avoid evaluating the step
  override def hashCode = super.hashCode
}

case object Step extends ImplicitParameters {
  /** create a Step object from either a previous result, or a value to evaluate */
  def fromEither[T](r: =>Either[Result, T]) = new Step(either(r))

  private[specs2]
  def either[T](r: =>Either[Result, T]): LazyParameter[Result] = lazyfy {
    r match {
      case Left(l)               => l
      case Right(result: Result) => result
      case Right(other)          => Success()
    }
  }

  /** create a Step object from any value */
  def apply[T](r: =>T) = fromEither(catchAll {
    r match {
      case r1: Result => r1
      case other      => DecoratedResult(other, Success())
    }
  }((e: Throwable) => Error(e)))

  /** create a Step object from a stopOnFail value. Make sure that the boolean evaluation doesn't fail */
  def apply(stopOnFail: =>Boolean)(implicit p: ImplicitParam) = {
    val stop = catchAll(stopOnFail)(Error(_))
    fromEither(stop).copy(stopOnFail = stop.fold(_ => false, b => b))
  }
}
/**
 * An Action is similar to a Step but can be executed concurrently with other examples.
 *
 * It is only reported in case of a failure
 */
case class Action (action: LazyParameter[Result] = lazyfy(Success()),
                   location: Location = new Location,
                   isolable: Boolean = true, private[specs2] val creationPath: Option[CreationPath] = None) extends Fragment with Executable with Isolable { outer =>

  def execute = action.value
  override def toString = "Action"

  override def map(f: Result => Result) = Action(action map f)

  /** this fragment can not be executed in a separate specification */
  def global = copy(isolable = false)

  /** set a creation path, if not already set, on this action to possibly isolate it during its execution */
  private[specs2] def creationPathIs(path: CreationPath) = copy(creationPath = if (outer.creationPath.isDefined) outer.creationPath else Some(path))

  // we must override the case class equality to avoid evaluating the action
  override def equals(any: Any) = any match {
    case a: Action => a eq this
    case _         => false
  }

  // we must override the case class hashCode to avoid evaluating the action
  override def hashCode = super.hashCode
}

case object Action {
  /** create an Action object from any value */
  def apply[T](r: =>T) = fromEither(trye(r)(Error(_)))
  /** create an Action object from either a previous result, or a value to evaluate */
  def fromEither[T](r: =>Either[Result, T]) = new Action(Step.either(r))
}

/**
 * Those standard Fragments are used to format the specification text:
 *  - End() can be used to "reset" the indentation of text
 *  - Br() can be used to insert a newline
 *  - Tab() can be used to increment the indentation level
 *  - Backtab() can be used to decrement the indentation level
 */
object StandardFragments {
  case class End() extends Fragment               { val location = new Location() }
  case class Br() extends Fragment                { val location = new Location() }
  case class Tab(n: Int = 1) extends Fragment     { val location = new Location() }
  case class Backtab(n: Int = 1) extends Fragment { val location = new Location() }
}

/**
 * Those fragments are used to tag other fragments in a specification\
 */
object TagsFragments {
  trait TaggingFragment extends Fragment {
    val location = new Location()
    /** tagging names */
    def names: Seq[String]
    /** @return true if the fragment tagged with this must be kept */
    def keep(args: Arguments): Boolean = SeparatedTags(args.include, args.exclude).keep(names)
    /** @return true if this tagging fragment is a section */
    def isSection: Boolean
    /** @return true if this fragment has no names */
    def isEmpty = names.isEmpty
  }
  /** tags the next fragment */
  case class Tag(names: String*) extends TaggingFragment {
    def isSection = false
    override def toString = names.mkString("Tag(", ",", ")")
    override def equals(o: Any) = {
      o match {
        case t @ Tag(_*)      => names == t.names
        case t @ TaggedAs(_*) => names == t.names
        case _ => false
      }
    }
  }
  /** tags the previous fragment */
  case class TaggedAs(names: String*) extends TaggingFragment {
    def isSection = false
    override def toString = names.mkString("TaggedAs(", ",", ")")
    override def equals(o: Any) = {
      o match {
        case t @ Tag(_*)      => names == t.names
        case t @ TaggedAs(_*) => names == t.names
        case _ => false
      }
    }
  }
  /** the previous fragment starts a section */
  case class AsSection(names: String*) extends TaggingFragment {
    def isSection = true
    override def toString = names.mkString("AsSection(", ",", ")")
    override def equals(o: Any) = {
      o match {
        case s @ AsSection(_*) => names == s.names
        case s @ Section(_*)   => names == s.names
        case _ => false
      }
    }
  }
  /** the next fragment starts a section */
  case class Section(names: String*) extends TaggingFragment {
    def isSection = true
    override def toString = names.mkString("Section(", ",", ")")
    override def equals(o: Any) = {
      o match {
        case s @ AsSection(_*) => names == s.names
        case s @ Section(_*)   => names == s.names
        case _ => false
      }
    }
  }

  /**
   * define a very coarse Monoid for TaggingFragments where appending 2 TaggingFragments returns a Tag object
   * with both list of tags
   */
  implicit def TaggingFragmentsAreMonoid = new Monoid[TaggingFragment] {
    val zero = Tag()
    def append(t1: TaggingFragment, t2: =>TaggingFragment) = Tag((t1.names ++ t2.names):_*)
  }

  /** @return true if the object is a TaggingFragment */
  def isTag(f: Fragment) = f match {
    case t: TaggingFragment => true
    case other              => false
  }
}
