package org.specs2
package control

class ExceptionsSpec extends Specification with Exceptions {
  val examples =
"""  
  The Exceptions trait provide functional ways to catch exceptions
  and deal with them
"""^
"  tryo executes an expression and return "^
"    Some(result) if the expression doesn't throw an exception" ! tryo1^
"    None if the expression failed" ! tryo2^
p^
"  trye executes an expression and return "^
"    Right(result) if the expression doesn't throw an exception" ! trye1^
"    Left(f(e)) if the expression failed, where f is a function of an exception" ! trye2^
p^
"  tryOr executes an expression and return "^
"    the result if the expression doesn't throw an exception" ! tryOr1^
"    the result if the expression doesn't throw an exception" ! tryOr2^
end
    
  def tryo1 = tryo("a") must_== Some("a")
  def tryo2 = tryo({error("boom");"a"}) must_== None

  def trye1 = trye("a")((e:Exception) => e.getMessage) must_== Right("a")
  def trye2 = trye({error("boom");"a"})((e:Exception) => e.getMessage) must_== Left("boom")

  def tryOr1 = tryOr("a")((e:Exception) => e.getMessage) must_== "a"
  def tryOr2 = tryOr({error("boom");"a"})((e:Exception) => "bang") must_== "bang"
}