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
case class SpecsArguments[T](arguments: List[(ArgumentsStart[T], Arguments)] = Nil) {
  def update(args: Arguments) = SpecsArguments {
    arguments.takeWhile(!_._1.isStart).map(a => (a._1, args)) ++ 
    arguments.dropWhile(!_._1.isStart)
  }
  def append(args: SpecsArguments[T]) = SpecsArguments(arguments ++ args.arguments)
  def args = arguments.last._2
  def headOption = arguments.headOption
  def lastOption = arguments.lastOption
  def toList = arguments.map(_._2)
  def fragments: List[T] = arguments.map(_._1.value)
  def filter(f: (T, Arguments) => Boolean) = {
    SpecsArguments(arguments.filter { case (fragment, args) => f(fragment.value, args) }).fragments
  }
}

private[specs2]
case object SpecsArguments {
  
  def filter[T](ts: Seq[T])(f: (T, Arguments) => Boolean)(implicit r: Reducer[T, SpecsArguments[T]]) = 
    foldAll(ts).filter(f).view

  def create[T](a: ArgumentsStart[T], args: Arguments = Arguments()) = new SpecsArguments(List((a, args)))
  
  implicit def SpecsArgumentsMonoid[T] = new Monoid[SpecsArguments[T]] {
    def append(a1: SpecsArguments[T], a2: =>SpecsArguments[T]) = {
      a1.lastOption match {
        case Some((s1, ar1)) => a1 append a2.update(ar1)
        case None => a2
      }
    }
    val zero = new SpecsArguments[T]()
  }
  def foldAll[T](fs: Seq[T])(implicit reducer: Reducer[T, SpecsArguments[T]]): SpecsArguments[T] = { 
    fs.foldMap(reducer.unit)
  }
  implicit object FragmentSpecsArgumentsReducer extends Reducer[Fragment, SpecsArguments[Fragment]] {
    implicit override def unit(f: Fragment) = f match {
      case SpecStart(_, args) => create(StartOfArguments(f), args)     
      case _                  => create(NoStartOfArguments(f))        
    }
  }
  implicit object SpecsArgumentsReducer extends Reducer[ExecutedFragment, SpecsArguments[ExecutedFragment]] {
    implicit override def unit(f: ExecutedFragment) = f match {
      case ExecutedSpecStart(_, _, args) => create(StartOfArguments(f), args)     
      case _                             => create(NoStartOfArguments(f))        
    }
  }

}
private[specs2]
sealed trait ArgumentsStart[T] {
  val value: T
  def isStart: Boolean
}
object ArgumentsStart {
  def unapply[T](a: ArgumentsStart[T]): Option[T] = Some(a.value)
}
case class StartOfArguments[T](value: T) extends ArgumentsStart[T] {
  def isStart = true
}
case class NoStartOfArguments[T](value: T) extends ArgumentsStart[T] {
  def isStart = false
}
