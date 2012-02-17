package org.specs2
package examples
import _root_.examples.SizedStack
import specification._

class Stack2Spec extends Specification { def is =                            isolated ^
                                                                             p^
    "A Stack with limited capacity can either be:"                           ^ endp^
    "1. Empty"                                                               ^
      "when the stack is empty"                                              ^
      stackIsEmpty                                                           ^
      { stack.size must_== 0 }                                               ^
      { stack.top must throwA[NoSuchElementException] }                      ^
      { stack.pop must throwA[NoSuchElementException] }                      ^
                                                                             endp^
    "2. Normal"                                                              ^
      "when the stack is not empty and not full"                             ^
      stackIsNormal                                                          ^
      { stack.size must be_>(0) }                                            ^
      { stack.top must_== stack.size }                                       ^
      { stack.top; stack.top must_== stack.size }                            ^
      { val top = stack.size; stack.pop must_== top }                        ^
      { stack.pop; stack.top must_== stack.size  }                           ^
      { stack.push(stack.size + 1);  stack.top must_== stack.size }          ^
                                                                             endp^
    "3. Full"                                                                ^
      "when the stack is full"                                               ^
      stackIsFull                                                            ^
      { stack.size must be_>(0) }                                            ^
      { stack.top must_== stack.size }                                       ^
      { stack.top; stack.top must_== stack.size }                            ^
      { val top = stack.size; stack.pop must_== top }                        ^
      { stack.pop; stack.top must_== stack.size  }                           ^
      { stack push (stack.size + 1) must throwAn[Error] }                    ^
                                                                             end

  /** stacks creation */
  def stackIsEmpty  = Step(stack = empty)
  def stackIsNormal = Step(stack = normal)
  def stackIsFull   = Step(stack = full)

  def empty  = SizedStack(maxCapacity = 10, size = 0)
  def normal = SizedStack(maxCapacity = 10, size = 2)
  def full   = SizedStack(maxCapacity = 10, size = 10)

  var stack: SizedStack = empty
}
