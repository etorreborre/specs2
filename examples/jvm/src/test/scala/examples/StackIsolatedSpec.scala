package examples

import org.specs2._

/**
 * This is another way to write the StackSpec, this time using auto-examples and
 * the "isolated" argument in order to be able to use local variables safely for a group of examples
 *
 * Note the use of eg to avoid early evaluation of the stack variable by the interpolated String when a block contains
 * several statements
 *
 * NOTE: this spec is failing non-deterministically, this is why it is commented out for now
 *
 *
 */
class StackIsolatedSpec extends Specification { def is = isolated ^ sequential ^ s2"""
                                                                             
 A Stack with limited capacity can either be:
 1. Empty
   when the stack is empty $stackIsEmpty
   $${ eg { stack must be empty }}
   $${ eg { stack.top must throwA[RuntimeException] }}  // 2.11: NoSuchElementException; 2.12/13: IndexOutOfBoundsException

   $${ eg { stack.pop must throwA[NoSuchElementException] }}

 2. Non-empty and not full
   when the stack is not empty and not full $stackIsNormal
   $${ eg { stack must not be empty } }
   $${ eg { stack.top === normalStack.top; stack === normalStack } }
   $${ eg { stack.pop === normalStack.top; stack !== normalStack } }
   $${ eg { stack push 1; stack.top === 1; stack !== normalStack } }

 3. Full
   when the stack is full $stackIsFull
   $${ eg { (stack push 1) must throwAn[Error] }}
                                                                       
"""

  /** stacks creation */
  def stackIsEmpty  = step { stack = emptyStack }
  def stackIsNormal = step { stack = normalStack }
  def stackIsFull   = step { stack = fullStack }

  def emptyStack  = SizedStack(maxCapacity = 10, size = 0)
  def normalStack = SizedStack(maxCapacity = 10, size = 2)
  def fullStack   = SizedStack(maxCapacity = 10, size = 10)

  var stack: SizedStack = normalStack
}
