package examples

import org.specs2._
import specification.core._

/**
 * This specification shows how to reuse examples and data.
 *
 * For example `nonEmptyStack` is a list of examples which are used to specify
 * - the case of a `normal` (non-empty) stack
 * - the case of a `full` stack
 *
 * Those examples are reused with different data in each case
 */
class StackSpec extends Specification { def is =  "Specification for a Stack with a limited capacity".title ^ s2"""

 A Stack with limited capacity can either be:                             $p
   1. Empty                                                               $anEmptyStack
   2. Normal (i.e. not empty but not full)                                $aNormalStack
   3. Full                                                                $aFullStack
                                                                          """

  /** examples for an empty stack */
  def anEmptyStack =                                                       s2"""
    An empty stack should
      have a size == 0                                                     ${empty().e1}
      throw an exception when sent #top                                    ${empty().e2}
      throw an exception when sent #pop                                    ${empty().e3}
                                                                           """

  /** examples for a normal stack */
  def aNormalStack =                                                       p^s2"""
    A normal stack should
      behave like a non-empty stack                                        ${nonEmptyStack(newNormalStack)}
      add to the top when sent #push                                       ${nonFullStack().e1}
                                                                           """
  /** examples for a full stack */
  def aFullStack =                                                         p^s2"""
    A full stack should
      behave like a non-empty stack                                        ${nonEmptyStack(newFullStack)}
      throw an exception when sent #push                                   ${fullStack().e1}
                                                                           """
  /** examples for a non-empty stack */
  def nonEmptyStack(stack: =>SizedStack): Fragments =                      s2"""
    have a size > 0                                                        ${nonEmpty(stack).size}
    return the top item when sent #top                                     ${nonEmpty(stack).top1}
    not remove the top item when sent #top                                 ${nonEmpty(stack).top2}
    return the top item when sent #pop                                     ${nonEmpty(stack).pop1}
    remove the top item when sent #pop                                     ${nonEmpty(stack).pop2}
                                                                           """
  /** stacks creation */
  def newEmptyStack  = SizedStack(maxCapacity = 10, size = 0)
  def newNormalStack = SizedStack(maxCapacity = 10, size = 2)
  def newFullStack   = SizedStack(maxCapacity = 10, size = 10)

  /** stacks examples */
  case class empty() {
    val stack = newEmptyStack

    def e1 = stack.size must_== 0
    def e2 = stack.top must throwA[NoSuchElementException]
    def e3 = stack.pop must throwA[NoSuchElementException]
  }

  def nonEmpty(createStack: =>SizedStack) = new {
    val stack = createStack

    def size = stack.size > 0

    def top1 = stack.top must_== stack.size
    def top2 = {
      stack.top
      stack.top must_== stack.size
    }

    def pop1 = {
      val topElement = stack.size
      stack.pop must_== topElement
    }

    def pop2 = {
      stack.pop
      stack.top must_== stack.size
    }
  }

  case class nonFullStack() {
    val stack = newNormalStack

    def e1 = {
      stack push (stack.size + 1)
      stack.top must_== stack.size
    }
  }
  case class fullStack() {
    val stack = newFullStack

    def e1 = stack push (stack.size + 1) must throwAn[Error]
  }
}

/**
 * SizedStack definition
 */
object SizedStack {
  def apply(maxCapacity: Int, size: Int) = new SizedStack(maxCapacity).fill(1 to size)
}

class SizedStack(val capacity: Int) extends scala.collection.mutable.Stack[Int] {
  override def push(a: Int) = {
    if (size == capacity) throw new Error("full stack")
    super.push(a)
  }
  def fill(range: Range) = {
    range.foreach(push(_))
    this
  }
}