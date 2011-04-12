package org.specs2
package specification

import io.MockOutput
import execute.Result

class BeforeAfterAroundSpec extends SpecificationWithJUnit { def is =

 "The `Before/After/Around Example` traits are used to automatically insert contexts around examples bodies"            ^
 "a spec can define a Before context that is used for each example"                                                     ^
   "in a mutable spec"                                                                                                  ! before ^
   "also in an acceptance spec"                                                                                         ! before2 ^
                                                                                                                        p^
 "a spec can define an After context that is used for each example"                                                     ! after ^
 "a spec can define an Around context that is used for each example"                                                    ! around ^
 "a spec can define a BeforeAfter context that is used for each example"                                                ! beforeAfter ^
 "a spec can define a BeforeAfterAround context that is used for each example"                                          ! beforeAfterAround ^
                                                                                                                        end

  def executeContains(s: SpecificationStructure with MockOutput, messages: String*) = {
    FragmentExecution.executeBodies(s.content)
    s.messages must contain(messages.map(lazyfy(_)):_*).inOrder
  }
  def before = executeContains(
    new mutable.Specification with BeforeExample with MockOutput {
      def before = println("before")
      "ex1" ! success
    }, "before")

  def before2 = executeContains(
    new Specification with BeforeExample with MockOutput {
      def before = println("before")
      def is = "ex1" ! success
    }, "before")

  def after = executeContains(
    new mutable.Specification with AfterExample with MockOutput {
      def after = println("after")
      "ex1" ! success
    },"after")

  def around = executeContains(
    new mutable.Specification with AroundExample with MockOutput {
      def around[R <% Result](r: =>R) = { println("around"); r }
      "ex1" ! success
    },"around")

  def beforeAfter = executeContains(
    new mutable.Specification with BeforeAfterExample with MockOutput {
      def before = println("before")
      def after = println("after")
      "ex1" ! success
    },"before", "after")


  def beforeAfterAround = executeContains(
    new mutable.Specification with BeforeAfterAroundExample with MockOutput {
      def before = println("before")
      def after = println("after")
      def around[R <% Result](r: =>R) = { println("around"); r }
      "ex1" ! success
    }, "before", "around", "after")
}