package org.specs2
package reporter

import scalaz._
import Scalaz._
import main.Arguments
import specification._
import TagsFragments._
/**
 * The SpecsArguments trait allows to fold a list of Fragments into the list of applicable arguments for each fragment
 */
private[specs2]
case class SpecsArguments[T](argumentsFragments: List[ApplicableArguments[T]] = Nil) {
  def append(s2: SpecsArguments[T]) = SpecsArguments(argumentsFragments ++ s2.argumentsFragments)

  def toList: List[Arguments] = {
    import NestedBlocks._
    def toBlock(a: ApplicableArguments[T]) = a match {
      case StartOfArguments(_, _, args) => BlockStart(args)
      case NoStartOfArguments(_)        => BlockBit(Arguments())
      case EndOfArguments(_, _)         => BlockEnd(Arguments())
    }
    import Arguments._
    overrideContext(argumentsFragments.map(toBlock _)).toList
  }

  /**
   * @return a list of pair (fragment, argument) where argument is the applicable arguments for the current fragment)
   */
  def fragmentAndApplicableArguments: Seq[(T, Arguments)] =
    argumentsFragments.zip(toList).collect { case (ApplicableArguments(value), args) => (value, args) }
  /**
   * @return a list of fragments without their corresponding arguments
   */
  def fragments: Seq[T] =
    argumentsFragments.collect { case (ApplicableArguments(value)) => value }
  /**
   * @return filter the values according to the current arguments
   */
  def filter(f: (T, Arguments) => Boolean)(implicit commandLineArgs: Arguments=Arguments()): Seq[T] =
    fragmentAndApplicableArguments |> filterTags |> filterSections |> filterFragments(f)

  /**
   * @return filter fragments according to tags
   */
  def filterTags(implicit commandLineArgs: Arguments=Arguments()) = (fragmentsAndArguments: Seq[(T, Arguments)]) =>
    fragmentsAndArguments.zip(fragments.drop(1)) collect {
      case ((f, args), t @ TaggedAs(_)) if t.keep(args.overrideWith(commandLineArgs)) => (f, args)
      case ((f1, args), f2)             if !f1.isInstanceOf[TaggingFragment]          => (f1, args)
    }
  /**
   * @return filter fragments according to sections
   */
  def filterSections(implicit commandLineArgs: Arguments=Arguments()) = (fragmentsAndArguments: Seq[(T, Arguments)]) =>
    fragmentsAndArguments

  /**
   * @return filter fragments according to fragments
   */
  def filterFragments(f: (T, Arguments) => Boolean)(implicit commandLineArgs: Arguments=Arguments()) = (fragmentsAndArguments: Seq[(T, Arguments)]) =>
     fragmentsAndArguments.collect {
      case (value, args) if f(value, commandLineArgs.overrideWith(args)) => value
    }
}

private[specs2]
case object SpecsArguments {
  def apply[T](s: ApplicableArguments[T]) = new SpecsArguments(List(s))

  def filter[T](ts: Seq[T])(f: (T, Arguments) => Boolean)(commandLineArgs: Arguments)(implicit r: Reducer[T, SpecsArguments[T]]): Seq[T] =
    foldAll(ts).filter(f)(commandLineArgs).view

  implicit def SpecsArgumentsMonoid[T] = new Monoid[SpecsArguments[T]] {
    def append(a1: SpecsArguments[T], a2: =>SpecsArguments[T]) = a1 append a2
    val zero = new SpecsArguments[T]()
  }
  def foldAll[T](fs: Seq[T])(implicit reducer: Reducer[T, SpecsArguments[T]]): SpecsArguments[T] = { 
    fs.foldMap(reducer.unit)
  }
  implicit object FragmentSpecsArgumentsReducer extends Reducer[Fragment, SpecsArguments[Fragment]] {
    implicit override def unit(f: Fragment) = f match {
      case SpecStart(name, args) => SpecsArguments(StartOfArguments(f, name, args))
      case SpecEnd(name)         => SpecsArguments(EndOfArguments(f, name))
      case _                     => SpecsArguments(NoStartOfArguments(f))
    }
  }
  implicit object SpecsArgumentsReducer extends Reducer[ExecutedFragment, SpecsArguments[ExecutedFragment]] {
    implicit override def unit(f: ExecutedFragment) = f match {
      case ExecutedSpecStart(name, args)    => SpecsArguments(StartOfArguments(f, name, args))
      case ExecutedSpecEnd(name)            => SpecsArguments(EndOfArguments(f, name))
      case _                                => SpecsArguments(NoStartOfArguments(f))
    }
  }

}
private[specs2]
sealed trait ApplicableArguments[T] {
  val value: T
}
object ApplicableArguments {
  def unapply[T](a: ApplicableArguments[T]): Option[T] = Some(a.value)
}
case class StartOfArguments[T](value: T, name: SpecName, args: Arguments) extends ApplicableArguments[T]
case class EndOfArguments[T](value: T, name: SpecName) extends ApplicableArguments[T]
case class NoStartOfArguments[T](value: T) extends ApplicableArguments[T]
