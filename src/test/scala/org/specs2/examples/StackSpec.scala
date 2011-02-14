package org.specs2
package examples

/**
 * This specification shows how to use different data with different example so that both of them can be reused
 */
class StackSpec extends SpecificationWithJUnit { def is = 
  
  "Specification for a Stack with a limited capacity".title                                       ^
                                                                                                  p^
  "An empty stack should"                                                                         ^
    "behave like an empty stack"                                                                  ^ isEmpty^
                                                                                                  endp^
  "A non-empty stack should"                                                                      ^
    "behave like a non empty stack"                                                               ^ isNonEmpty(normal)^
                                                                                                  endp^
  "A stack below full capacity should"                                                            ^
    "behave like a non empty stack"                                                               ^ isNonEmpty(normal)^
    "behave like a stack below capacity"                                                          ^ isNotFull(normal)^
                                                                                                  endp^
  "A full stack should"                                                                           ^
    "behave like a non empty stack"                                                               ^ isNonEmpty(full)^
    "behave like a full stack"                                                                    ^ isFull(full)^
                                                                                                  end

  def normal = Stack(10, 2)
  def full = Stack(10, 10)

  def isEmpty =
    "throw an exception when sent #top"                                                           ! empty().e1^
    "throw an exception when sent #pop"                                                           ! empty().e2

  def isNonEmpty(s: =>SizedStack) =
    "not be empty"                                                                                ! nonempty(s).size^
    "return the top item when sent #top"                                                          ! nonempty(s).top1^
    "not remove the top item when sent #top"                                                      ! nonempty(s).top2^
    "return the top item when sent #pop"                                                          ! nonempty(s).pop1^
    "remove the top item when sent #pop"                                                          ! nonempty(s).pop2

  def isNotFull(s: =>SizedStack) =
    "add to the top when sent #push"                                                              ! notfull(s).e1

  def isFull(s: =>SizedStack) =
    "throw an exception when sent #push"                                                          ! fullStack(s).e1

  case class empty() {
    val stack = new SizedStack(10)
    def e1 = stack.top must throwA[NoSuchElementException]
    def e2 = stack.pop must throwA[NoSuchElementException]
  }
  case class nonempty(stack: SizedStack) {
    def size = !stack.isEmpty
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
  case class notfull(stack: SizedStack) {
    def e1 = {
      stack push (stack.size + 1)
      stack.top must_== stack.size
    }
  }
  case class fullStack(stack: SizedStack) {
    def e1 = stack push (stack.size + 1) must throwAn[Error]
  }
}
object Stack {
  def apply(capacity: Int, maxNumber: Int) = new SizedStack(capacity).fill(1 to maxNumber)
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

