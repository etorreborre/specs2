package org.specs2
package specification
import execute._
import matcher._

class ExampleIsolationSpec extends Specification with StandardResults with UserInteractions {
  val content = 
""" 
  This specification shows how to use case classes to ensure Fragments isolation.
  
  The main idea is to create a inheritance tree of classes to model nested contexts where
  variables are renitialized for each example by the sheer virtue of creating a fresh context 
  object
"""                                                                                      ^
"  Those 2 first Fragments show that they are not sharing variables"                     ^
"    This one modifies a local variable"                                                 ! c().e1^
"    This other one 'reuses' the same local variable, but the variable is "              +
"    reinitialized"                                                                      ! c().e2^
                                                                                         p^
"  The next 2 Fragments show that it is possible to 'nest' context by inheriting them"   ^
"    This example uses new local variables + the one from the parent context"            ! c1().e3^
"    And isolation is still ok for another example"                                      ! c1().e4^
                                                                                         p^
"  Now these Fragments model a fictive customer interaction"                             ^
"  The user logs in"                                                                     ^ 
"    if he selects a car, then his favorite must be displayed"                           ! select.favoriteCar^
"    if he selects an hotel, then his favorite must be displayed"                        ! select.favoriteHotel^
"  The user logs out"                                                                    ^ 
"    if he has selected a car, then it must be displayed"                                ! summary.carSelection^
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
trait UserInteractions extends StandardResults with MustExpectations with Matchers {
  class Login {
    var loggedIn = false
	  def login = loggedIn = true
	  def logout = loggedIn = false
  }
  def select = new Select
  class Select extends Login {
	  login
	  var selection = ""
	  def favoriteCar = { selection = "car"; loggedIn must_== true }
	  def favoriteHotel = { selection = "hotel"; loggedIn must_== true }
  }
  def summary = new Summary
  class Summary extends Select {
	  favoriteCar
	  logout
	  def carSelection = loggedIn must beFalse
  }
}