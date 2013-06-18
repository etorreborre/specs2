package org.specs2
package reporter

import mock.Mockito
import specification._
import execute._

class NotifierSpec extends Specification with Mockito with Tags with Grouped { def is = sequential ^ s2"""

A Notifier can be used to get a stream of events for the execution of a Specification

 The SpecStart is notified                                                                                  ${g1.e1}
 A Text is notified                                                                                         ${g2.e1}
   with its location                                                                                        ${g2.e2}
 Going up a level is notified                                                                               ${g3.e1}
 Going down a level is notified                                                                             ${g3.e2}
 If there are 2 'contexts' they are both notified                                                           ${g3.e3}

 An example is notified
   when starting
     with its description                                                                                   ${g4.e1}
     with its location                                                                                      ${g4.e2}

   when completing
     with its description                                                                                   ${g4.e3}
     with its result
       when Failure                                                                                         ${g4.e4}
       when Error                                                                                           ${g4.e5}
       when Skipped                                                                                         ${g4.e6}
       when Pending                                                                                         ${g4.e7}
       and a filtered stacktrace                                                                            ${g4.e8}
       when a DataTable                                                                                     ${g4.e9}

 A step is notified
   but only if it fails                                                                                     ${g5.e1}

 The SpecEnd is notified                                                                                    ${g6.e1}
 Fragments should be printed in the right order                                                             ${g7.e1}

 Arguments must be satisfied
   xonly only notifies of the failed and error examples                                                     ${g8.e1}
   showOnly only notifies of the examples with the correct statuses                                         ${g8.e2}
                                                                                                            """


  "start" - new g1 {
    e1 := { there was one(notified).specStart(anyString, anyString) }
  }
  "text" - new g2 {
    e1 := { there was atLeastOne(notified).text(===("intro"), anyString)           }
    e2 := { there was atLeastOne(notified).text(anyString, matching(".*.scala.*")) }
  }
  "levels" - new g3 {
    e1 := { there was one(notified).contextStart(anyString, anyString)      }
    e2 := { there was one(notified).contextEnd(anyString, anyString)        }
    e3 := { there was two(notified(spec2)).contextEnd(anyString, anyString) }
  }
  "examples" - new g4 {
    e1 := { there was atLeastOne(notified).exampleStarted(===("ex1"), anyString)                                                                                          }
    e2 := { there was atLeastOne(notified).exampleStarted(anyString, matching(".*NotifierSpecification.scala:11.*"))                                                      }
    e3 := { there was atLeastOne(notified).exampleSuccess(===("ex1"), anyLong)                                                                                            }
    e4 := { there was atLeastOne(notified).exampleFailure(anyString, anyString, matching(".*NotifierSpecification.scala:13.*"), any[Throwable], any[Details], anyLong)    }
    e5 := { there was atLeastOne(notified).exampleError(anyString, anyString, matching(".*NotifierSpecification.scala:15.*"), any[Throwable], anyLong)                    }
    e6 := { there was atLeastOne(notified).exampleSkipped(anyString, anyString, anyLong)                                                                                  }
    e7 := { there was atLeastOne(notified).examplePending(anyString, anyString, anyLong)                                                                                  }
    e8 := { there was atLeastOne(notified).
            exampleFailure(anyString, anyString, anyString, containMatch("specs2") ^^ ((t:Throwable) => t.getStackTrace().map(_.toString).toList), any[Details], anyLong) }
    e9 := { there was atLeastOne(notified).exampleSuccess(anyString, anyLong)                                                                                            }
  }
  "steps" - new g5 {
    e1 := { there was atLeastOne(notified).exampleFailure(anyString, matching("clean failed"), anyString, any[Throwable], any[Details], anyLong) }
  }
  "end" - new g6 {
    e1 := { there was one(notified).specEnd(anyString, anyString) }
  }
  "order" - new g7 {
    e1 := reportMessages(spec) === Seq(
    "specStart:  NotifierSpecification",
    "text:  intro",
    "contextStart:  first group",
    "exampleStarted:  ex1",
    "exampleSuccess:  ex1",
    "exampleStarted:  ex2",
    "exampleFailure:  ex2",
    "exampleStarted:  ex3",
    "exampleError:  ex3",
    "exampleStarted:  ex4",
    "exampleSkipped:  ex4",
    "exampleStarted:  ex5",
    "examplePending:  ex5",
    "exampleStarted:  ex6",
    "exampleSuccess:  ex6",
    "exampleStarted:  step failure",
    "exampleFailure:  step failure",
    "contextEnd:  first group",
    "specEnd:  NotifierSpecification")
  }
  "arguments" - new g8 {
    e1 := { there was no(notified(withXOnly)).exampleSuccess(anyString, anyLong) }
    e2 := { there was no(notified(withShowOnly)).exampleSuccess(anyString, anyLong) }
  }

  def notified: Notifier = notified(spec)
  def notified(s: Specification): Notifier = {
    val r = reporter
    r.report(s)(s.content.arguments)
    r.notifier
  }
  def spec = new user.reporter.NotifierSpecification
  def spec2 = new user.reporter.NotifierSpecification2
  def withXOnly = new Specification { def is  = xonly ^ "ex1" ! success ^ "ex2" ! failure }
  def withShowOnly = new Specification { def is  = showOnly("ox") ^ "ex1" ! success ^ "ex2" ! failure ^ "e3" ! skipped }

  def reporter = new NotifierReporter {
    val notifier = mock[Notifier]
  }

  def reportMessages(s: Specification) = {
    val messagesNotifier = MessagesNotifier
    new NotifierReporter {
      val notifier = messagesNotifier
    }.report(s)(s.content.arguments)
    messagesNotifier.messages.map(_.split("\\:").take(2).mkString(": "))
  }
}

