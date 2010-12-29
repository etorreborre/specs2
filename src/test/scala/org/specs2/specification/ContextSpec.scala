package org.specs2
package specification
import io._
import execute._
import reporter._

class ContextSpec extends SpecificationWithJUnit with FragmentExecution { def is =
                                                                                                                        """
  It is sometimes necessary to provide functions to "prepare" the specification before
  executing the Fragments and clean it up afterwards. This may be for example:

     * opening a database connection
     * inserting some data
     * executing the example
     * closing the connection after each example
     
  It may also be very convenient to have each example executed "inside" a specific
  context, like a  web application session. Finally, some setups or cleanups are very 
  expensive so one might want to add arbitrary actions that will be executed only once,
  at the beginning of the specification or the end.
  
  All of this can be achieved in specs2 by using case classes which extend the following
  traits:

     * Before
     * After
     * Around
     * BeforeAfter or BeforeAfterAround for combined functionality
                                                                                                  """                                                                                 ^
  "The Before trait can be used to execute methods before Fragments"                              ^
    "the before method is executed before a first example"                                        ! c().e1^
    "the before method is executed before the second example"                                     ! c().e2^
                                                                                                  p^
  "If the before method throws an exception"                                                      ^
    "the first example will not execute"                                                          ! c().e3^
    "and it will be reported as an error"                                                         ! c().e4^
                                                                                                  p^
  "The After trait can be used to execute methods after Fragments"                                ^
    "the after method is executed after a first example"                                          ! c().e5^
    "the after method is executed after the second example"                                       ! c().e6^
                                                                                                  p^
  "If the after method throws an exception"                                                       ^
    "the first example will execute"                                                              ! c().e7^
    "but it will be reported as an error"                                                         ! c().e8^
                                                                                                  p^
  "The Around trait can be used to"                                                               ^
    "execute the example inside a user provided function"                                         ! c().e9^
                                                                                                  p^
  "The BeforeAfter trait can be used to"                                                          ^
    "execute a method before and after each example"                                              ! c().e10^
                                                                                                  p^
  "The BeforeAfterAround trait can be used to"                                                    ^
    "execute a method before, around and after the first example"                                 ! c().e11^
                                                                                                  p^
  "The Before After Around traits can be composed to create more complex setups"                  ^
    "before compose before2 "                                                                     ! compose().e1^
    "before then before2 "                                                                        ! compose().e2^
    "after compose after2 "                                                                       ! compose().e3^
    "after then after2 "                                                                          ! compose().e4^
    "beforeAfter compose before2After2 "                                                          ! compose().e5^
    "beforeAfter then before2After2 "                                                             ! compose().e6^
    "around compose around2 "                                                                     ! compose().e7^
    "around then around2 "                                                                        ! compose().e8^
    "beforeAfterAround compose before2After2Around"                                               ! compose().e9^
    "beforeAfterAround then before2After2Around2"                                                 ! compose().e10^
                                                                                                  p^
  "An Action can be used to create Step fragments containing an action to execute:"               ^
    "val beforeSpec = new Action"                                                                 ^
    "def is = beforeSpec(c.println('beforeSpec')) ^ ex1"                                          ^
                                                                                                  p^
    "that action will execute and return a result"                                                ! c().e12^
    "if it executes ok, nothing is printed, it is a silent Success"                               ! c().e13^
    "otherwise, it is reported as an Error"                                                       ! c().e14^
                                                                                                  end
  implicit val args = main.Arguments()
  case class c() extends FragmentsExecution {
    def e1 = executing(ex1Before).prints("before", "e1")
    def e2 = executing(ex1_2Before).prints("before", "e1", "before", "e2")
    def e3 = executing(ex1_beforeFail).prints()
    def e4 = executeBodies(ex1_beforeFail).map(_.message) must_== List("error")
    
    def e5 = executing(ex1After).prints("e1", "after")
    def e6 = executing(ex1_2After).prints("e1", "after", "e2", "after")
    def e7 = executing(ex1_afterFail).prints("e1")
    def e8 = executeBodies(ex1_beforeFail).map(_.message) must_== List("error")
    def e9 = executing(ex1Around).prints("around", "e1")
    def e10 = executing(ex1BeforeAfter).prints("before", "e1", "after")
    def e11 = executing(ex1BeforeAfterAround).prints("before", "around", "e1", "after")
    def e12 = executing(firstThenEx1).prints("first", "e1")
    def e13 = executeBodies(silentFirstThenEx1).map(_.message) must_== List("success")
    def e14 = executeBodies(failingFirstThenEx1).map(_.message) must_== List("error", "success")
 
  }
  case class compose() extends FragmentsExecution {
    def e1 = executing(ex1BeforeComposeBefore2).prints("before2", "before", "e1")
    def e2 = executing(ex1BeforeThenBefore2).prints("before", "before2", "e1")
    def e3 = executing(ex1AfterComposeAfter2).prints("e1", "after2", "after")
    def e4 = executing(ex1AfterThenAfter2).prints("e1", "after", "after2")
    def e5 = executing(ex1BeforeAfterComposeBefore2After2).prints("before2", "before", "e1", "after2", "after")
    def e6 = executing(ex1BeforeAfterThenBefore2After2).prints("before", "before2", "e1", "after", "after2")
    def e7 = executing(ex1AroundComposeAround2).prints("around2", "around", "e1")
    def e8 = executing(ex1AroundThenAround2).prints("around", "around2", "e1")
    def e9 = executing(ex1BeforeAfterAroundComposeBefore2After2Around2).
             prints("before2", "before", "around2", "around", "e1", "after2", "after")
    def e10 = executing(ex1BeforeAfterAroundThenBefore2After2Around2).
                   prints("before", "before2", "around", "around2", "e1", "after", "after2")
  }
    
  class FragmentsExecution extends MockOutput with ContextData {
    def executing(exs: Fragments): Executed = Executed(executeBodies(exs))
    case class Executed(r: Seq[Result]) {
      def prints(ms: String*): Result = {
        messages must_== List(ms:_*)
      }  
    }
  }
}
trait ContextData extends StandardResults with FragmentsBuilder with ContextsForFragments {

  def ok(name: String) = { println(name); success }
  def ok1 = ok("e1")
  def ok2 = ok("e2")
  
  def ex1 = "ex1" ! ok1  
  def ex1Before = "ex1" ! before(ok1) 
  def ex1BeforeComposeBefore2 = "ex1" ! (before compose before2)(ok1)  
  def ex1BeforeThenBefore2 = "ex1" ! (before then before2)(ok1)  
  def ex1AfterComposeAfter2 = "ex1" ! (after compose after2)(ok1)  
  def ex1AfterThenAfter2 = "ex1" ! (after then after2)(ok1)  
  def ex1BeforeAfterComposeBefore2After2 = "ex1" ! (beforeAfter compose before2After2)(ok1)  
  def ex1BeforeAfterThenBefore2After2 = "ex1" ! (beforeAfter then before2After2)(ok1)  
  def ex1AroundComposeAround2 = "ex1" ! (around compose around2)(ok1)  
  def ex1AroundThenAround2 = "ex1" ! (around then around2)(ok1)  
  def ex1BeforeAfterAroundComposeBefore2After2Around2 = "ex1" ! (beforeAfterAround compose before2After2Around2)(ok1)  
  def ex1BeforeAfterAroundThenBefore2After2Around2 = "ex1" ! (beforeAfterAround then before2After2Around2)(ok1)  

  def ex1_beforeFail = "ex1" ! beforeWithError(ok1) 
  def ex1_2Before = ex1Before ^ "ex2" ! before(ok2)

  def ex1After = "ex1" ! after(ok1) 
  def ex1_afterFail = "ex1" ! afterWithError(ok1) 
  def ex1_2After = ex1After ^ "ex2" ! after(ok2)

  def ex1Around = "ex1" ! around(ok1) 
  def ex1BeforeAfter = "ex1" ! beforeAfter(ok1) 
  def ex1BeforeAfterAround = "ex1" ! beforeAfterAround(ok1)
  
  val first = new Action
  def firstThenEx1 = first(println("first")) ^ ex1
  def silentFirstThenEx1 = first("first") ^ ex1
  def failingFirstThenEx1 = first(Predef.error("error")) ^ ex1
}
trait ContextsForFragments extends MockOutput {
  object before extends Before {
	  def before = println("before")
  }
  object before2 extends Before {
    def before = println("before2")
  }
  object beforeWithError extends Before with MockOutput {
	  def before = Predef.error("error")
  }
  object after extends After {
	  def after = println("after")
  }
  object after2 extends After {
    def after = println("after2")
  }
  object afterWithError extends After {
	  def after = Predef.error("error")
  }
  object around extends Around {
	  def around[T <% Result](a: =>T) = { println("around"); a } 
  }
  object around2 extends Around {
    def around[T <% Result](a: =>T) = { println("around2"); a } 
  }
  object beforeAfter extends BeforeAfter {
	  def before = println("before")
	  def after = println("after")
  }
  object before2After2 extends BeforeAfter {
    def before = println("before2")
    def after = println("after2")
  }
  object beforeAfterAround extends BeforeAfterAround {
	  def before = println("before")
	  def after = println("after")
	  def around[T <% Result](a: =>T) = { println("around"); a } 
  }
  object before2After2Around2 extends BeforeAfterAround {
    def before = println("before2")
    def after = println("after2")
    def around[T <% Result](a: =>T) = { println("around2"); a } 
  }
}