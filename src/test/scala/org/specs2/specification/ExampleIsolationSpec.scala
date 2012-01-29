package org.specs2
package specification
import execute._
import matcher._

class ExampleIsolationSpec extends Specification with UserInteractions { def is =
                                                                                                                        """
This specification shows how to use case classes to ensure Fragments isolation.

The main idea is to create a inheritance tree of classes to model nested contexts where variables are renitialized
for each example by the sheer virtue of creating a fresh context object
                                                                                                                        """^p^
  "Those 2 first Fragments show that they are not sharing variables"                                                    ^
    "This one modifies a local variable"                                                                                ! c().e1^
    "This other one 'reuses' the same local variable, but the variable is reinitialized"                                ! c().e2^
                                                                                                                        p^
  "The next 2 Fragments show that it is possible to 'nest' context by inheriting them"                                  ^
    "This example uses new local variables + the one from the parent context"                                           ! c1().e3^
    "And isolation is still ok for another example"                                                                     ! c1().e4^
                                                                                                                        p^
  "Now these Fragments model a fictive customer interaction"                                                            ^
  "When the user logs in"                                                                                               ^
    "his past history must be shown"                                                                                    ! history().isShown^
    "if he selects tickets"                                                                                             ^
      "the list must be displayed"                                                                                      ! tickets().list^
      "the total amount must be displayed"                                                                              ! tickets().total^
      "if he buys tickets"                                                                                              ^
        "his favorite payment type is shown"                                                                            ! buy().favorite^
                                                                                                                        end

  trait Env {
	  var local = 0
	  var local2 = 0
  }
  case class c() extends Env {
	  def e1 = { local += 1; success }
	  def e2 = { local must_== 0 }
  }
  case class c1() extends Env {
	  def e3 = { local += 1; local2 +=1; success }
	  def e4 = { local must_== 0; local2 must_== 0 }
  }
}
trait UserInteractions extends StandardResults with MustMatchers  {
  trait Login {
    var loggedIn = false
    def login = loggedIn = true
    def logout = loggedIn = false
  }
  case class history() extends Login {
    login
    def isShown = loggedIn must beTrue
  }
  case class tickets() extends Login {
    login
    def list = success
    def total = success
  }
  case class buy() extends Login {
    tickets()
    def favorite = success
  }
}