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
case class SpecsArguments[T](argumentsFragments: Seq[ApplicableArguments[T]] = Vector()) {
  def append(s2: SpecsArguments[T]) = SpecsArguments(argumentsFragments ++ s2.argumentsFragments)

  lazy val nestedArguments: Seq[Arguments] = {
    import NestedBlocks._
    def toBlock(a: ApplicableArguments[T]) = a match {
      case StartOfArguments(_, _, args) => BlockStart(args)
      case NoStartOfArguments(_)        => BlockBit(Arguments())
      case EndOfArguments(_, _)         => BlockEnd(Arguments())
    }
    import Arguments._
    overrideContext(argumentsFragments.map(toBlock _)).toSeq
  }

  lazy val last = nestedArguments.lastOption.getOrElse(Arguments())
  /**
   * @return the list of all applicable spec names
  */
  lazy val nestedSpecNames: Seq[SpecName] = {
    import NestedBlocks._
    def toBlock(a: ApplicableArguments[T]) = a match {
      case StartOfArguments(_, name, _) => BlockStart(name)
      case NoStartOfArguments(_)        => BlockBit[SpecName](SpecName(""))
      case EndOfArguments(_, name)      => BlockEnd(name)
    }
    import SpecName._
    overrideContext(argumentsFragments.map(toBlock _)).toSeq
  }
  /**
   * filter the fragments with 2 functions:
   *  - one working on the whole fragment list
   *  - one working on each individual fragment
   */
  def filter(fs: Seq[(T, Arguments, SpecName)] => Seq[T]): Seq[T] = fragmentAndApplicableArgumentsAndSpecNames |> fs

  /**
   * @return a list of pairs (fragment, argument) where argument is the applicable arguments for the current fragment)
   */
  lazy val fragmentAndApplicableArguments: Seq[(T, Arguments)] =
    argumentsFragments.view.zip(nestedArguments).collect { case (ApplicableArguments(value), args) => (value, args) }
  /**
   * @return a list of pairs (fragment, specName) where specName is the parent specification
   */
  lazy val fragmentAndSpecNames: Seq[(T, SpecName)] = argumentsFragments.view.zip(nestedSpecNames).collect { case (ApplicableArguments(value), name) => (value, name) }
  /**
   * @return a list of triplets (fragment, argument, specName) where
   * argument is the applicable arguments for the current fragment and specName is the parent specification
   */
  lazy val fragmentAndApplicableArgumentsAndSpecNames: Seq[(T, Arguments, SpecName)] =
    argumentsFragments.view.zip(nestedArguments).zip(nestedSpecNames).collect { case ((ApplicableArguments(value), args), name) => (value, args, name) }
  /**
   * @return a list of fragments without their corresponding arguments
   */
  lazy val fragments: Seq[T] = argumentsFragments.collect { case (ApplicableArguments(value)) => value }
}

private[specs2]
case object SpecsArguments {
  def apply[T](s: ApplicableArguments[T]) = new SpecsArguments(Vector(s))

  /**
   * filter the fragments with 2 functions:
   *  - one working on the whole fragment list
   *  - one working on each individual fragment
   */
  def filter[T](ts: Seq[T])(fs: Seq[(T, Arguments, SpecName)] => Seq[T])(implicit r: Reducer[T, SpecsArguments[T]]): Seq[T] =
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
      case s @ SpecStart(_,_,_,_,_) => SpecsArguments(StartOfArguments(f, s.specName, s.arguments))
      case e @ SpecEnd(_)           => SpecsArguments(EndOfArguments(f, e.specName))
      case _                        => SpecsArguments(NoStartOfArguments(f))
    }
  }
  implicit object SpecsArgumentsReducer extends Reducer[ExecutedFragment, SpecsArguments[ExecutedFragment]] {
    implicit override def unit(f: ExecutedFragment) = f match {
      case s @ ExecutedSpecStart(_,_,_) => SpecsArguments(StartOfArguments(f, s.specName, s.args))
      case e @ ExecutedSpecEnd(_,_,_)   => SpecsArguments(EndOfArguments(f, e.specName))
      case _                            => SpecsArguments(NoStartOfArguments(f))
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
