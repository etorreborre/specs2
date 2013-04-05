package org.specs2
package specification

import collection.Iterablex._
import execute.Executable
import main.Arguments
import org.specs2.internal.scalaz.Monoid
import Fragments._
import specification.StandardFragments.{End, Br}

/**
 * A Fragments object is a list of fragments with a SpecStart and a SpecEnd
 */
case class Fragments(specTitle: Option[SpecName] = None, middle: Seq[Fragment] = Vector().view,
                     arguments: Arguments = Arguments(), linked: Linked = Linked()) {

  def fragments: Seq[Fragment] = if (isZero) Vector().view else (specStart +: middle :+ specEnd)
  def isZero = this == Fragments()

  def specTitleIs(name: SpecName): Fragments = copy(specTitle = specTitle.filterNot(_.title.isEmpty).map(_.overrideWith(name)).orElse(Some(name)))

  def add(e: Fragment): Fragments = append(e)
  def add(fs: Seq[Fragment]): Fragments = copy(middle = middle ++ fs)
  def add(fs: Fragments): Fragments = add(fs.fragments)
  def add(a: Arguments): Fragments = copy(arguments = arguments.overrideWith(a))

  /** append the fragments from fs, appending the text fragments if this object ends with Text and fs starts with Text */
  def append(fs: Fragments)(implicit exampleFactory: ExampleFactory) =
    (middle, fs.middle) match {
      case (begin :+ Text(t1), Text(t2) +: rest) => ((new FragmentsFragment(this)) ^ fs).copy(middle = begin ++ (Text(t1+t2) +: rest))
      case _                                     => (new FragmentsFragment(this)) ^ fs
    }

  def middleDrop(n: Int) = copy(middle = Vector(middle:_*).drop(n).view)
  def middleDropRight(n: Int) = copy(middle = Vector(middle:_*).dropRight(n).view)
  def middleDropWhile(p: Fragment => Boolean) = copy(middle = Vector(middle:_*).dropWhile(p).view)

  def insert(e: Fragment): Fragments = prepend(e)
  def insert(fs: Seq[Fragment]): Fragments = copy(middle = fs ++ middle)
  def insert(fs: Fragments): Fragments = insert(fs.fragments)

  private def prepend(e: Fragment) = copy(middle = e +: middle)
  private def append(e: Fragment) = copy(middle = middle :+ e)

  def urlIs(url: String)         = copy(specTitle = specTitle.map(_.urlIs(url)),     linked = linked.urlIs(url))
  def baseDirIs(dir: String)     = copy(specTitle = specTitle.map(_.baseDirIs(dir)), linked = linked.baseDirIs(dir))

  def linkIs(htmlLink: HtmlLink) = copy(linked = linked.linkIs(htmlLink))
  def seeIs(htmlLink: HtmlLink)  = copy(middle = Vector().view, linked = linked.seeIs(htmlLink))
  def hide                       = copy(linked = linked.linkIs(HtmlLink(this)).hide)

  def executables: Seq[Executable] = fragments.collect { case e: Executable => e }
  def examples: Seq[Example] = fragments.collect(isAnExample)
  def texts: Seq[Text] = fragments.collect(isSomeText)

  def overrideArgs(args: Arguments) = copy(arguments = arguments.overrideWith(args))
  def map(function: Fragment => Fragment) = Fragments.create(fragments.map(function):_*)
  def flatMap(function: Fragment => Seq[Fragment]) = Fragments.create(fragments.flatMap(function):_*)

  override def toString = fragments.mkString("\n")

  def specName = specStart.specName
  def name     = specStart.name

  lazy val specStart: SpecStart = SpecStart(specTitle.getOrElse(SpecName("")), arguments, linked)
  lazy val specEnd:   SpecEnd   = SpecEnd(specName)
}

/**
 * Utility methods for fragments
 */
object Fragments {

  def apply(t: SpecName): Fragments = Fragments().specTitleIs(t)
  /**
   * @return a Fragments object containing only a seq of Fragments.
   */
  def createList(fs: Fragment*) = Fragments(middle = fs)
  /**
   * @return a Fragments object, where the SpecStart might be provided by the passed fragments
   */
  def create(fs: Fragment*) = {
    fs match {
      case (s @ SpecStart(n, a, l)) +: rest :+ SpecEnd(_,_) => Fragments(Some(n), rest, a, l)
      case (s @ SpecStart(n, a, l)) +: rest                 => Fragments(Some(n), rest, a, l)
      case _                                                => createList(fs:_*)
    }
  }

  /** @return true if the Fragment is a Text */
  def isText: Function[Fragment, Boolean] = { case Text(_) => true; case _ => false }
  /** @return the text if the Fragment is a Text */
  def isSomeText: PartialFunction[Fragment, Text] = { case t @ Text(_) => t }
  /** @return true if the Fragment is an Example */
  def isExample: Function[Fragment, Boolean] = { case Example(_, _) => true; case _ => false }
  /** @return the example if the Fragment is an Example */
  def isAnExample: PartialFunction[Fragment, Example] = { case e @ Example(_,_) => e }
  /** @return true if the Fragment is a step */
  def isStep: Function[Fragment, Boolean] = { case Step(_,_) => true; case _ => false }
  /** @return true if the Fragment is a SpecStart or a SpecEnd */
  def isSpecStartOrEnd: Function[Fragment, Boolean] = { case SpecStart(_,_,_) | SpecEnd(_,_) => true; case _ => false }
  /** @return the spec start if the Fragment is a SpecStart */
  def isASpecStart: PartialFunction[Fragment, Fragment] = { case s @ SpecStart(_,_,_) => s }
  /** @return the spec end if the Fragment is a SpecEnd */
  def isASpecEnd: PartialFunction[Fragment, Fragment] = { case s @ SpecEnd(_,_) => s }
  /** @return true if the Fragment is a SpecStart */
  def isSpecStart: Function[Fragment, Boolean] = { case s @ SpecStart(_,_,_) => true; case _ => false }
  /** @return true if the Fragment is a SpecEnd */
  def isSpecEnd: Function[Fragment, Boolean] = { case s @ SpecEnd(_,_) => true; case _ => false }
  /** @return true if the Fragment is an Example or a Step */
  def isExampleOrStep: Function[Fragment, Boolean] = (f: Fragment) => isExample(f) || isStep(f)
  /** @return the step if the Fragment is a Step */
  def isAStep: PartialFunction[Fragment, Step] = { case s @ Step(_,_) => s }
  /** @return the action if the Fragment is an Actino */
  def isAnAction: PartialFunction[Fragment, Action] = { case a @ Action(_) => a }
  /** @return the step if the Fragment is a Br fragment */
  def isABr: PartialFunction[Fragment, Fragment] = { case br @ Br() => br }
  /** @return the step if the Fragment is an End fragment */
  def isAnEnd: PartialFunction[Fragment, Fragment] = { case e @ End() => e }

  /** @return a Fragments object with the appropriate name set on the SpecStart fragment */
  def withSpecName(fragments: Fragments, name: SpecName): Fragments = fragments.specTitleIs(name)
  
  /**
   * @return a Fragments object with the appropriate name set on the SpecStart fragment
   *
   * That name is derived from the specification structure name
   */
  def withSpecName(fragments: Fragments, s: SpecificationStructure): Fragments = withSpecName(fragments, SpecName(s))
  /**
   * @return a Fragments object with creation paths set on Examples and Actions
   *
   * The path of a Fragment is either:
   *
   *  - set at construction time when it comes from a mutable specification
   *  - its index in the sequence of fragments for an acceptance specification
   */
  def withCreationPaths(fragments: Fragments): Fragments = fragments.copy(middle = fragments.middle.zipWithIndex.map {
    case (e @ Example(_,_), i) => e.creationPathIs(AcceptanceCreationPath(Seq(i+1)))
    case (a @ Action(_), i)    => a.creationPathIs(AcceptanceCreationPath(Seq(i+1)))
    case (other, i)            => other
  })

  /**
   * Fragments can be added as a monoid
   */
  implicit def fragmentsIsMonoid = new Monoid[Fragments] {
    val zero = new Fragments()
    def append(s1: Fragments, s2: => Fragments) = {
      if (s1.isZero)      s2
      else if (s2.isZero) s1
      else                Fragments.createList((s1.fragments ++ s2.fragments):_*)
    }
  }
}

/** encapsulation of the linking behaviour */
case class Linked(link: Option[HtmlLink] = None, seeOnly: Boolean = false, hidden: Boolean = false) {
  def isSeeOnlyLink = isLink && seeOnly
  def isIncludeLink = isLink && !seeOnly
  def isLink        = link.isDefined

  def urlIs(url: String)     = copy(link = link.map(_.urlIs(url)))
  def baseDirIs(dir: String) = copy(link = link.map(_.baseDirIs(dir)))

  def linkIs(htmlLink: HtmlLink) = copy(link = Some(htmlLink))
  def seeIs(htmlLink: HtmlLink)  = copy(link = Some(htmlLink), seeOnly = true)
  def hide                       = copy(hidden = true)

  def linkToString = link.map(l => ", link:"+l.toString+", seeOnly:"+seeOnly).getOrElse("")
}

