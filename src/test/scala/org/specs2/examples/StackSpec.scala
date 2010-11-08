package org.specs2
package examples

class StackSpec extends SpecificationWithJUnit { def is =

  "Specification for a Stack with a limited capacity".title                               ^
                                                                                          p^
  "An empty stack should"                                                                 ^
  "  behave like an empty stack"                                                          ^ emptyStack^
                                                                                          end^
  "A non-empty stack should"                                                              ^
  "  behave like a non empty stack"                                                       ^ nonEmptyStack(Stack(10, 2)) ^                                          
                                                                                          end^
  "A stack below full capacity should"                                                    ^
  "  behave like a non empty stack"                                                       ^ nonEmptyStack(Stack(10, 2)) ^                                          
  "  behave like a stack below capacity"                                                  ^ belowCapacity(Stack(10, 2)) ^  
                                                                                          end^
  "A full stack should"                                                                   ^
  "  behave like a non empty stack"                                                       ^ nonEmptyStack(Stack(10, 10)) ^                                          
  "  behave like a full stack"                                                            ^ fullStack(Stack(10, 10)) ^
                                                                                          end
                                                                                          
  def emptyStack = 
  "  throw an exception when sent #top"                                                   ! empty().e1^ 
  "  throw an exception when sent #pop"                                                   ! empty().e2^p
  
  def nonEmptyStack(s: =>SizedStack) =                                                                                           
  "  not be empty"                                                                        ! nonempty(s).e1^ 
  "  return the top item when sent #top"                                                  ! nonempty(s).e2^ 
  "  not remove the top item when sent #top"                                              ! nonempty(s).e3^ 
  "  return the top item when sent #pop"                                                  ! nonempty(s).e4^ 
  "  remove the top item when sent #pop"                                                  ! nonempty(s).e5^p 

  def belowCapacity(s: =>SizedStack) = 
  "  add to the top when sent #push"                                                      ! notfull(s).e1^p  

  def fullStack(s: =>SizedStack) = 
  "  throw an exception when sent #push"                                                  ! full(s).e1^p

  case class empty() {
    val stack = new SizedStack(10)
    def e1 = stack.top must throwA[NoSuchElementException]
    def e2 = stack.pop must throwA[NoSuchElementException]
  }
  case class nonempty(stack: SizedStack) {
    def e1 = !stack.isEmpty
    def e2 = stack.top must_== stack.size
    def e3 = { 
      stack.top must_== stack.size
      stack.top must_== stack.size
    }
    def e4 = {
      val topElement = stack.size 
      stack.pop must_== topElement
    }
    def e5 = { 
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
  case class full(stack: SizedStack) {
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

