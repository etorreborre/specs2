package examples
import org.specs2._
import specification._

/**
 * This is another way to write the StackSpec, this time using auto-examples and
 * the "isolated" argument in order to be able to use local variables safely for a group of examples
 */
class StackIsolatedSpec extends Specification { def is =                     isolated ^
                                                                             p^
    "A Stack with limited capacity can either be:"                           ^ endp^
    "1. Empty"                                                               ^
      "when the stack is empty"                                              ^ stackIsEmpty^
      { stack must be empty }                                                ^
      { stack.top must throwA[NoSuchElementException] }                      ^
      { stack.pop must throwA[NoSuchElementException] }                      ^
                                                                             endp^
    "2. Non-empty and not full"                                              ^
      "when the stack is not empty and not full"                             ^ stackIsNormal^
      { stack must not be empty }                                            ^
      { stack.top === normalStack.top; stack === normalStack }               ^
      { stack.pop === normalStack.top; stack !== normalStack }               ^
      { stack push 1; stack.top === 1; stack !== normalStack }               ^
                                                                             endp^
    "3. Full"                                                                ^
      "when the stack is full"                                               ^ stackIsFull^
      { (stack push 1) must throwAn[Error] }                                 ^
                                                                             end

  /** stacks creation */
  def stackIsEmpty  = Step(stack = emptyStack)
  def stackIsNormal = Step(stack = normalStack)
  def stackIsFull   = Step(stack = fullStack)

  def emptyStack  = SizedStack(maxCapacity = 10, size = 0)
  def normalStack = SizedStack(maxCapacity = 10, size = 2)
  def fullStack   = SizedStack(maxCapacity = 10, size = 10)

  var stack: SizedStack = emptyStack
}
