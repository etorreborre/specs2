package org.specs2
package reporter

import scalaz._
import Scalaz._
import main.Arguments
import specification._

/**
 * The ScopedArguments trait allows to fold a list of Fragments and keep the 
 * arguments held by the last SpecStart fragment.
 */
private[specs2]
case class SpecsArguments[T](argumentsFragments: List[ArgumentsStart[T]] = Nil) {
  def append(s2: SpecsArguments[T]) = SpecsArguments(argumentsFragments ++ s2.argumentsFragments)
  def arguments = argumentsFragments.zip(toList)
  def toList: List[Arguments] = {
    argumentsFragments.foldLeft((Nil: List[Arguments], Nil: List[Arguments])) { (res, cur) =>
      val (fragmentsArgs, currentArgs) = res
      cur match {
        case StartOfArguments(f, name, args) => (fragmentsArgs :+ args, args :: currentArgs)
        case NoStartOfArguments(f)           => (fragmentsArgs ++ currentArgs.headOption.toList, currentArgs)
        case EndOfArguments(f, name)         => (fragmentsArgs ++ currentArgs.headOption.toList, currentArgs.drop(1))
      }
    }._1
  }
  def filter(f: (T, Arguments) => Boolean): Seq[T] = {
    arguments.filter { case (fragment, args) => f(fragment.value, args) }.map(_._1.value)
  }
}

private[specs2]
case object SpecsArguments {
  def apply[T](s: ArgumentsStart[T]) = new SpecsArguments(List(s))

  def filter[T](ts: Seq[T])(f: (T, Arguments) => Boolean)(implicit r: Reducer[T, SpecsArguments[T]]): Seq[T] =
    foldAll(ts).filter(f).view

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
      case ExecutedSpecStart(name, _, args) => SpecsArguments(StartOfArguments(f, name, args))
      case ExecutedSpecEnd(name)            => SpecsArguments(EndOfArguments(f, name))
      case _                                => SpecsArguments(NoStartOfArguments(f))
    }
  }

}
private[specs2]
sealed trait ArgumentsStart[T] {
  val value: T
}
object ArgumentsStart {
  def unapply[T](a: ArgumentsStart[T]): Option[T] = Some(a.value)
}
case class StartOfArguments[T](value: T, name: SpecName, args: Arguments) extends ArgumentsStart[T]
case class EndOfArguments[T](value: T, name: SpecName) extends ArgumentsStart[T]
case class NoStartOfArguments[T](value: T) extends ArgumentsStart[T]
