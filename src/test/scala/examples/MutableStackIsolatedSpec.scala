package examples

import org.specs2.mutable._

/**
 * This is another way to write the StackSpec, using:
 *
 * - a mutable specification
 * - the `isolated` argument in order to be able to use local variables safely for a group of examples
 * - auto-examples (annotated with .eg but ; have to be inserted to avoid type-inference issues
 * - the `<<` operator to create some text followed by a step (see the MutableGivenWhenThenSpec)
 * - formatting fragments operator to transform any string to a Text fragment followed by a formatting fragment
 */
class MutableStackIsolatedSpec extends Specification { isolated; noindent
  "A Stack with limited capacity can either be:".br

  "1. Empty".br
  "when the stack is empty" <<
  { stack = emptyStack }
  { stack must be empty }.eg;
  { stack.top must throwA[NoSuchElementException] }.eg;
  { stack.pop must throwA[NoSuchElementException] }.eg.br

  "2. Non-empty and not full".br
  "when the stack is not empty and not full" <<
  { stack = normalStack }
  { stack must not be empty }.eg;
  { stack.top === normalStack.top; stack === normalStack }.eg;
  { stack.pop === normalStack.top; stack !== normalStack }.eg;
  { stack push 1; stack.top === 1; stack !== normalStack }.eg.br

  "3. Full".br
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
class MutableStackIsolated2Spec extends Specification { isolated; noindent
  "A Stack with limited capacity can either be:".br

  "1. Empty".br
  "when the stack is empty"
  step { stack = emptyStack }
  eg { stack must beEmpty }
  eg { stack.top must throwA[NoSuchElementException] }
  eg { stack.pop must throwA[NoSuchElementException] }; br

  "2. Non-empty and not full".br
  "when the stack is not empty and not full"
  step { stack = normalStack }
  eg { stack must not be empty }
  eg { stack.top === normalStack.top; stack === normalStack }
  eg { stack.pop === normalStack.top; stack !== normalStack }
  eg { stack push 1; stack.top === 1; stack !== normalStack }; br

  "3. Full".br
  "when the stack is full"
  step { stack = fullStack }
  eg { (stack push 1) must throwAn[Error] }

  def emptyStack  = SizedStack(maxCapacity = 10, size = 0)
  def normalStack = SizedStack(maxCapacity = 10, size = 2)
  def fullStack   = SizedStack(maxCapacity = 10, size = 10)

  var stack: SizedStack = emptyStack
}
