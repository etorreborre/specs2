package org.specs2
package reporter
import mock.Mockito
import specification._
import main.Arguments
import execute._

class NotifierSpec extends SpecificationWithJUnit with Mockito { def is =
                                                                                                                        """
A Notifier can be used to get stream of events for the execution of a Specification
                                                                                                                        """^
  "The SpecStart is notified"                                                                                           ! start1^
  "A Text is notified"                                                                                                  ! text1^
    "with its location"                                                                                                 ! text2^
  "Going up a level is notified"                                                                                        ! level1^
  "Going down a level is notified"                                                                                      ! level2^
                                                                                                                        p^
  "An example is notified"                                                                                              ^
    "when starting"                                                                                                     ^
      "with its description"                                                                                            ! ex1^
      "with its location"                                                                                               ! ex2^
    "when completing"                                                                                                   ^
      "with its description"                                                                                            ! ex3^
      "with its result"                                                                                                 ^
        "when Failure"                                                                                                  ! ex4^
        "when Error"                                                                                                    ! ex5^
        "when Skipped"                                                                                                  ! ex6^
        "when Pending"                                                                                                  ! ex7^
                                                                                                                        endp^
  "A step can be notified"                                                                                              ^
    "if it fails"                                                                                                       ! step1^
                                                                                                                        endp^
  "The SpecEnd is notified"                                                                                             ! end1^
                                                                                                                        end


  def start1 = there was one(notified).specStart(anyString, anyString)
  def text1  = there was atLeastOne(notified).text(equalTo("intro"), anyString)
  def text2  = there was atLeastOne(notified).text(anyString, matching(".*.scala.*"))
  def level1 = there was one(notified).contextStart(anyString, anyString)
  def level2 = there was one(notified).contextEnd(anyString, anyString)
  def ex1    = there was atLeastOne(notified).exampleStarted(equalTo("ex1"), anyString)
  def ex2    = there was atLeastOne(notified).exampleStarted(anyString, matching(".*NotifierSpecification.scala:11.*"))
  def ex3    = there was atLeastOne(notified).exampleSuccess(equalTo("ex1"), anyLong)
  def ex4    = there was atLeastOne(notified).exampleFailure(anyString, anyString, matching(".*NotifierSpecification.scala:12.*"), any[Throwable], anyLong)
  def ex5    = there was atLeastOne(notified).exampleError(anyString, anyString, matching(".*NotifierSpecification.scala:14.*"), any[Throwable], anyLong)
  def ex6    = there was atLeastOne(notified).exampleSkipped(anyString, anyString, anyLong)
  def ex7    = there was atLeastOne(notified).examplePending(anyString, anyString, anyLong)
  def step1  = there was atLeastOne(notified).exampleFailure(anyString, matching("clean failed"), anyString, any[Throwable], anyLong)
  def end1   = there was one(notified).specEnd(anyString, anyString)

  def notified: Notifier = {
    val r = reporter
    r.report(spec)(Arguments())
    r.notifier
  }
  def spec = new user.reporter.NotifierSpecification
  def reporter = new NotifierReporter {
    val notifier = mock[Notifier]
  }
}

