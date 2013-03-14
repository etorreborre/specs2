package examples

import org.specs2._
import specification._

/**
 * This is another way to write the StackSpec, this time using auto-examples and
 * the "isolated" argument in order to be able to use local variables safely for a group of examples
 *
 * Note the use of eg to avoid early evaluation of the stack variable by the interpolated String when a block contains
 * several statements
 */
class StackIsolatedSpec extends Specification with Groups { def is = isolated ^ s2"""
                                                                             
 A Stack with limited capacity can either be:
 1. Empty
   when the stack is empty $stackIsEmpty
   ${ stack must be empty }
   ${ stack.top must throwA[NoSuchElementException] }
   ${ stack.pop must throwA[NoSuchElementException] }

 2. Non-empty and not full
   when the stack is not empty and not full $stackIsNormal
   ${ stack must not be empty }
   ${ eg { stack.top === normalStack.top; stack === normalStack } }
   ${ eg { stack.pop === normalStack.top; stack !== normalStack } }
   ${ eg { stack push 1; stack.top === 1; stack !== normalStack } }

 3. Full
   when the stack is full $stackIsFull
   ${ (stack push 1) must throwAn[Error] }
                                                                      """

  /** stacks creation */
  def stackIsEmpty  = Step(stack = emptyStack)
  def stackIsNormal = Step(stack = normalStack)
  def stackIsFull   = Step(stack = fullStack)

  def emptyStack  = SizedStack(maxCapacity = 10, size = 0)
  def normalStack = SizedStack(maxCapacity = 10, size = 2)
  def fullStack   = SizedStack(maxCapacity = 10, size = 10)

  var stack: SizedStack = normalStack
}
