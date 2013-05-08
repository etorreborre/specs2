package examples

import org.specs2.mutable._
import org.specs2.specification.AllExpectations

/**
 * This specification shows a way to re-write the StackSpec example, using:
 *
 * - a mutable specification
 * - the `isolated` argument in order to be able to use local variables safely for a group of examples
 * - auto-examples (annotated with .eg but ; have to be inserted to avoid type-inference issues
 * - formatting fragments operator to transform any string to a Text fragment followed by a formatting fragment
 */
class MutableStackIsolatedSpec extends Specification { isolated
  "A Stack with limited capacity can either be:".p

  "1. Empty".br
  "when the stack is empty".br
  step { stack = emptyStack }
  eg { stack must beEmpty }
  eg { stack.top must throwA[NoSuchElementException] }
  eg { stack.pop must throwA[NoSuchElementException] }; endp

  "2. Non-empty and not full".br
  "when the stack is not empty and not full".br
  step { stack = normalStack }
  eg { stack must not be empty }
  eg { stack.top === normalStack.top; stack === normalStack }
  eg { stack.pop === normalStack.top; stack !== normalStack }
  eg { stack push 1; stack.top === 1; stack !== normalStack }; endp

  "3. Full".br
  "when the stack is full".br
  step { stack = fullStack }
  eg { (stack push 1) must throwAn[Error] }

  def emptyStack  = SizedStack(maxCapacity = 10, size = 0)
  def normalStack = SizedStack(maxCapacity = 10, size = 2)
  def fullStack   = SizedStack(maxCapacity = 10, size = 10)

  var stack: SizedStack = emptyStack
}