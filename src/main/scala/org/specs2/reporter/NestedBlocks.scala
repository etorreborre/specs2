package org.specs2
package reporter

import org.specs2.internal.scalaz._
import  Scalaz._
import data.Trees._
import specification.{SpecEnd, SpecStart, Fragment, ExecutedSpecEnd, ExecutedSpecStart, ExecutedFragment}

/**
* This trait takes a sequence of delimited blocks and uses that information to either:
*
* - define what is the "current" context and makes sure that the current context is overriding the "parent" context
* - sum the "values" of the current context and cumulate the total of children contexts into the parent context
*
*/
private[specs2]
trait NestedBlocks {
  sealed trait SpecBlock[T] {
    def value: T
    def update(value: T): SpecBlock[T]
  }
  case class BlockStart[T](value: T) extends SpecBlock[T] {
    def update(value: T) = BlockStart(value)
  }
  case class BlockEnd[T](value: T) extends SpecBlock[T]  {
    def update(value: T) = BlockEnd(value)
  }
  case class BlockBit[T](value: T) extends SpecBlock[T] {
    def update(value: T) = BlockBit(value)
  }
  def isBlockStart[T](b: SpecBlock[T]) = b match { case BlockStart(_) => true; case _ => false }
  def isBlockEnd[T](b: SpecBlock[T])   = b match { case BlockEnd(_) => true; case _ => false }
  def isBlockBit[T](b: SpecBlock[T])   = b match { case BlockBit(_) => true; case _ => false }

  /**
   * The context is overriden when we enter a Nested Block.
   * This is used for Arguments management, where arguments of an included specification must override those of the parent
   * one.
   */
  def overrideContext[T : Monoid](blocks: Seq[SpecBlock[T]]): Seq[T] = overrideContext(blocks, identity[T])
  def overrideContext[T : Monoid, S](blocks: Seq[SpecBlock[T]], f: T => S): Seq[S] = {
    blocks.foldLeft((Nil: List[S], Nil: List[T])) { (res, cur) =>
      val (result, stack) = res
      cur match {
        case BlockStart(value)       => (result :+ f(top(stack) |+| value), (top(stack) |+| value) :: stack)
        case BlockBit(value)         => (result :+ f(top(stack)), stack)
        case BlockEnd(value)         => (result :+ f(top(stack)), pop(stack))
      }
    }._1
  }

  /**
   * The context is reset when we enter a Nested Block and totaled when we leave it.
   * This is used for Statistics management where an included specification must start new Statistics then add its
   * total to the parent ones.
   */
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

  /**
   * The context is added when we enter a Nested Block and reset when we leave it.
   * This is used for Level management where an included specification must go on with indenting text, then reset
   * to the previous indentation level when finished.
   */
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

  trait TreeNode[T] {
    def addStart(b: SpecBlock[T])
  }

  def associateStartEnd[T](blocks: Seq[SpecBlock[T]], f: (T, T) => (T, T)): Seq[T] = {
    blocks.headOption match {
      case None         => Seq()
      case Some(start)  => {
        blocks.drop(1).foldLeft(leaf(start).loc) { (res, cur) =>
          cur match {
            case BlockStart(value)       => res.insertDownLast(leaf(cur))
            case BlockBit(value)         => res.addChild(cur)
            case BlockEnd(value)         => {
              val (st, en) = f(res.getLabel.value, value)
              res.updateLabel(_.update(st)).addChild(cur.update(en)).getParent
            }
          }
        }.root.tree.flatten.map(_.value).toSeq
      }
    }
  }


  def addToTop[T : Monoid](stack: List[T], value: T) = (top(stack) |+| value) :: pop(stack)
  def top[T : Monoid](stack: List[T]): T = {
    val monoid = implicitly[Monoid[T]]
    stack.headOption.getOrElse(monoid.zero)
  }
  def pop[T](stack: List[T]) = stack.drop(1)
  
  private def lift[T](f: (T, T) => (T, T)): (SpecBlock[T], SpecBlock[T]) => (SpecBlock[T], SpecBlock[T]) = { (b1, b2) =>
    val (updated1, updated2) = f(b1.value, b2.value)
    (b1.update(updated1), b2.update(updated2)) 
  }
  
  def fragmentsToSpecBlock = (f: Fragment) => f match {
    case SpecStart(_,_,_,_)   => BlockStart(f)
    case SpecEnd(_)           => BlockEnd(f)
    case other                => BlockBit(f)
  }

  def executedFragmentsToSpecBlock = (f: ExecutedFragment) => f match {
    case ExecutedSpecStart(_,_,_) => BlockStart(f)
    case ExecutedSpecEnd(_,_,_)   => BlockEnd(f)
    case other                    => BlockBit(f)
  }

}
private[specs2]
object NestedBlocks extends NestedBlocks
