package org.specs2
package control

class ExceptionsSpec extends Specification with Exceptions {
  val examples =
"""  
The Exceptions trait provide functional ways to deal with exceptions
"""^
  "tryo executes an expression and return "^
    "Some(result) if the expression doesn't through an exception" ! e1^
    "None if the expression failed" ! e2^
p^
  "trye executes an expression and return "^
    "Right(result) if the expression doesn't through an exception" ! e3^
    "Left(f(e)) if the expression failed, where f is a function of an exception" ! e4
    
  def e1 = tryo("a") must_== Some("a")
  def e2 = tryo({error("boom");"a"}) must_== None
  def e3 = trye("a")((e:Exception) => e.getMessage) must_== Right("a")
  def e4 = trye({error("boom");"a"})((e:Exception) => e.getMessage) must_== Left("boom")
}