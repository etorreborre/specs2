package org.specs2
package matcher

class TerminationMatchersSpec extends Specification with TerminationMatchers { def is =

  "It is possible to check if a block of code terminates"                                          ^
    "with a default number of retries and default sleep time"                                      ^
      "if it succeeds"                                                                             ! e1^
      "if it fails"                                                                                ! e2^
                                                                                                   p^
    "with a specified number of retries and sleep time"                                            ^
      "if it succeeds"                                                                             ! e3^
      "if it fails"                                                                                ! e4^
                                                                                                   p^
    "We can write 'action must not terminate'"                                                     ! e5^
                                                                                                   end

  def e1 = { Thread.sleep(50) must terminate }
  def e2 = { (Thread.sleep(150) must terminate) returns "the action is blocking with retries=0 and sleep=100" }
  def e3 = { Thread.sleep(50) must terminate(retries=2, sleep=20.millis) }
  def e4 = { (Thread.sleep(100) must terminate(retries=3, sleep=20.millis)) returns "the action is blocking with retries=3 and sleep=20" }
  def e5 = Thread.sleep(150) must not terminate
}