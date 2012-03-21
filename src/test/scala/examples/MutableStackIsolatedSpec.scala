package examples

import org.specs2.mutable._
import org.specs2.specification.AllExpectations

/**
 * This is another way to write the StackSpec, using:
 *
 * - a mutable specification
 * - the `isolated` argument in order to be able to use local variables safely for a group of examples
 * - auto-examples (annotated with .eg but ; have to be inserted to avoid type-inference issues
 * - the `<<` operator to create some text followed by a step (see the MutableGivenWhenThenSpec)
 * - formatting fragments operator to transform any string to a Text fragment followed by a formatting fragment
 */
class MutableStackIsolatedSpec extends Specification { isolated
  "A Stack with limited capacity can either be:".txt

  "1. Empty".newp
  "when the stack is empty" <<
  { stack = emptyStack }
  { stack must beEmpty }.eg;
  { stack.top must throwA[NoSuchElementException] }.eg;
  { stack.pop must throwA[NoSuchElementException] }.eg

  "2. Non-empty and not full".newp
  "when the stack is not empty and not full" <<
  { stack = normalStack }
  { stack must not be empty }.eg;
  { stack.top === normalStack.top; stack === normalStack }.eg;
  { stack.pop === normalStack.top; stack !== normalStack }.eg;
  { stack push 1; stack.top === 1; stack !== normalStack }.eg

  "3. Full".newp
  "when the stack is full" <<
  { stack = fullStack }
  { (stack push 1) must throwAn[Error] }.eg

  def emptyStack  = SizedStack(maxCapacity = 10, size = 0)
  def normalStack = SizedStack(maxCapacity = 10, size = 2)
  def fullStack   = SizedStack(maxCapacity = 10, size = 10)

  var stack: SizedStack = emptyStack
}

/**
 * The StackSpec with a mutable specification, isolated variables, autoexamples but no postfix operations
 */
class MutableStackIsolated2Spec extends Specification { isolated
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

/**
 * The StackSpec with a mutable specification, isolated variables, no autoexamples and no postfix operations
 * The AllExpectations trait is mixed in so that all expectations are verified in an Example
 */
class MutableStackIsolated3Spec extends Specification with AllExpectations { isolated
  "A Stack with limited capacity can either be:".p

  "1. Empty".br
  "when the stack is empty" << {
    stack = emptyStack
  }
  "it has no elements" >> {
    stack must beEmpty
  }
  "it not possible to get the top element or to remove it" >> {
    stack.top must throwA[NoSuchElementException]
    stack.pop must throwA[NoSuchElementException]
  }; endp

  "2. Non-empty and not full".br
  "when the stack is not empty and not full" << {
    stack = normalStack
  }
  "it is possible to retrieve the top element, this doesn't modify the stack" >> {
    stack.top === normalStack.top
    stack === normalStack
  }
  "it is possible to remove the top element with 'pop', which modifies the stack" >> {
    stack.pop === normalStack.top
    stack !== normalStack
  }
  "it is possible to push an element on the top of the stack" >> {
    stack push 1
    stack.top === 1
    stack !== normalStack
  }; endp

  "3. Full".br
  "when the stack is full" << {
    stack = fullStack
  }
  "it is not possible to push an element on top of the stack" >> {
    (stack push 1) must throwAn[Error]
  }

  def emptyStack  = SizedStack(maxCapacity = 10, size = 0)
  def normalStack = SizedStack(maxCapacity = 10, size = 2)
  def fullStack   = SizedStack(maxCapacity = 10, size = 10)

  var stack: SizedStack = emptyStack
}
