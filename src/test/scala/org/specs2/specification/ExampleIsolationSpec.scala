package org.specs2
package specification

class ExampleIsolationSpec extends Specification with FeaturesResults {
  val examples = 
""" 
This specification shows how to use case classes to ensure examples isolation.

The main idea is to create a inheritance tree of classes to model nested contexts where
variables are renitialized for each example by the sheer virtue of creating a fresh context 
object
"""^
"Those 2 first examples show that they are not sharing variables"^
  "This one modifies a local variable" ! c().e1^
  "This other one 'reuses' the same local variable, but the variable is " +
  "reinitialized"! c().e2^
par^
"The next 2 examples show that it is possible to 'nest' context by inheriting them"^
  "This example uses new local variables + the one from the parent context" ! c1().e3^
  "And isolation is still ok for another example" ! c1().e4
  
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