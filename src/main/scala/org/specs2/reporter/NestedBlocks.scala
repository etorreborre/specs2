package org.specs2
package reporter

import scalaz._
import Scalaz._

/**
 * This trait takes a sequence of delimited blocks and uses that information to either:
 *
 * - define what is the "current" context and makes sure that the current context is overriding the "parent" context
 * - sum the "values" of the current context and cumulate the total of children contexts into the parent context
 *
 */
trait NestedBlocks {
  sealed trait SpecBlock[T] {
    def value: T
  }
  case class BlockStart[T](value: T) extends SpecBlock[T]
  case class BlockEnd[T](value: T) extends SpecBlock[T]
  case class BlockBit[T](value: T) extends SpecBlock[T]

  def overrideContext[T : Monoid](blocks: Seq[SpecBlock[T]]): Seq[T] = overrideContext(blocks, identity[T])
  def overrideContext[T : Monoid, S](blocks: Seq[SpecBlock[T]], f: T => S): Seq[S] = {
    blocks.foldLeft((Nil: List[S], Nil: List[T])) { (res, cur) =>
      val (result, stack) = res
      cur match {
        case BlockStart(value)       => (result :+ f(top(addToTop(stack, value))), addToTop(stack, value))
        case BlockBit(value)         => (result :+ f(top(stack)), stack)
        case BlockEnd(value)         => (result :+ f(top(stack)), pop(stack))
      }
    }._1
  }

  def addToTop[T : Monoid](stack: List[T], value: T) = (top(stack) |+| value) :: pop(stack)
  def totalContext[T : Monoid](blocks: Seq[SpecBlock[T]]): Seq[T] = totalContext(blocks, identity[T])
  def totalContext[T : Monoid, S](blocks: Seq[SpecBlock[T]], f: T => S): Seq[S] = {
    blocks.foldLeft((Nil: List[S], Nil: List[T])) { (res, cur) =>
      val (result, stack) = res
      cur match {
        case BlockStart(value)       => (result :+ f(value), value :: stack)
        case BlockBit(value)         => (result :+ f(value), addToTop(stack, value))
        case BlockEnd(value)         => (result :+ f(top(stack)), addToTop(pop(stack), top(stack)))
      }
    }._1
  }

  def sumContext[T : Monoid](blocks: Seq[SpecBlock[T]]): Seq[T] = sumContext(blocks, identity[T])
  def sumContext[T : Monoid, S](blocks: Seq[SpecBlock[T]], f: T => S): Seq[S] = {
    blocks.foldLeft((Nil: List[S], Nil: List[T])) { (res, cur) =>
      val (result, stack) = res
      cur match {
        case BlockStart(value)       => (result :+ f(value), value :: stack)
        case BlockBit(value)         => (result :+ f(top(addToTop(stack, value))), addToTop(stack, value))
        case BlockEnd(value)         => (result :+ f(top(addToTop(stack, value))), pop(stack))
      }
    }._1
  }

  def top[T : Monoid](stack: List[T]): T = {
    val monoid = implicitly[Monoid[T]]
    stack.headOption.getOrElse(monoid.zero)
  }
  def pop[T](stack: List[T]) = stack.drop(1)
}
object NestedBlocks extends NestedBlocks