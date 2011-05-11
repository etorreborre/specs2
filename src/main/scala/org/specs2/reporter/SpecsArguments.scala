package org.specs2
package reporter

import org.specs2.internal.scalaz._
import Scalaz._
import main.Arguments
import specification._
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
   * filter the fragments with 2 functions:
   *  * one working on the whole fragment list
   *  * one working on each individual fragment
   */
  def filter(fs: Seq[(T, Arguments)] => Seq[T]): Seq[T] = fragmentAndApplicableArguments |> fs

  /**
   * @return a list of pair (fragment, argument) where argument is the applicable arguments for the current fragment)
   */
  def fragmentAndApplicableArguments: Seq[(T, Arguments)] =
    argumentsFragments.zip(toList).view.collect { case (ApplicableArguments(value), args) => (value, args) }
  /**
   * @return a list of fragments without their corresponding arguments
   */
  def fragments: Seq[T] =
    argumentsFragments.collect { case (ApplicableArguments(value)) => value }
}

private[specs2]
case object SpecsArguments {
  def apply[T](s: ApplicableArguments[T]) = new SpecsArguments(List(s))

  /**
   * filter the fragments with 2 functions:
   *  * one working on the whole fragment list
   *  * one working on each individual fragment
   */
  def filter[T](ts: Seq[T])(fs: Seq[(T, Arguments)] => Seq[T])(implicit r: Reducer[T, SpecsArguments[T]]): Seq[T] =
    foldAll(ts).filter(fs)

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
      case ExecutedSpecStart(name, args, _)    => SpecsArguments(StartOfArguments(f, name, args))
      case ExecutedSpecEnd(name, _)            => SpecsArguments(EndOfArguments(f, name))
      case _                                   => SpecsArguments(NoStartOfArguments(f))
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
