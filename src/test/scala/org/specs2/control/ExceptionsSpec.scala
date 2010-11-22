package org.specs2
package control

class ExceptionsSpec extends SpecificationWithJUnit with Exceptions {  def is =
  
                                                                                          """  
  The Exceptions trait provide functional ways to catch exceptions                        
  and deal with them                                                                      
                                                                                          """																					                                     ^
  "tryo executes an expression and returns"                                               ^br^
    "Some(result) if the expression doesn't throw an exception"                           ! tryo1^
    "None if the expression failed"                                                       ! tryo2^
                                                                                          p^
  "tryOr executes an expression and return "                                              ^
    "the result if the expression doesn't throw an exception"                             ! tryOr1^
    "a default value if the expression throws an exception"                               ! tryOr2^
                                                                                          p^
  "tryOrElse executes an expression and return "                                          ^
    "the result if the expression doesn't throw an exception"                             ! tryOrElse1^
    "another value if the expression throws an exception"                                 ! tryOrElse2^
                                                                                          p^
  "tryMap executes an expression and return "                                             ^
    "a 'ok' value if the expression doesn't throw an exception"                           ! tryMap1^
    "a 'ko' value if the expression throws an exception"                                  ! tryMap2^
                                                                                          p^
  "tryOk executes an expression and return "                                              ^
    "true if the expression doesn't throw an exception"                                   ! tryOk1^
    "false if the expression throws an exception"                                         ! tryOk2^
                                                                                          p^
  "trye executes an expression and returns"                                               ^
    "Left(f(e)) if the expression threw anything, even an error"                          ! catchAll2^
    "Right(result) if the expression doesn't throw an exception"                          ! trye1^
    "Left(f(e)) if the expression failed, where f is a function of an exception"          ! trye2^
                                                                                          p^
  "catchAll executes an expression and returns"                                           ^
    "Right(result) if the expression doesn't throw a Throwable"                           ! catchAll1^
                                                                                          end
    
  def tryo1 = tryo("a") must_== Some("a")
  def tryo2 = tryo({error("boom");"a"}) must_== None

  def tryOr1 = tryOr("a")((e:Exception) => e.getMessage) must_== "a"
  def tryOr2 = tryOr({error("boom");"a"})((e:Exception) => "bang") must_== "bang"

  def tryOrElse1 = tryOrElse("a")("b") must_== "a"
  def tryOrElse2 = tryOrElse({error("boom");"a"})("bang") must_== "bang"

  def tryMap1 = tryMap("a")(true)(false) must_== true
  def tryMap2 = tryMap({error("boom");"a"})(true)(false) must_== false

  def tryOk1 = tryOk("a") must_== true
  def tryOk2 = tryOk({error("boom");"a"}) must_== false

  def trye1 = trye("a")((e:Exception) => e.getMessage) must_== Right("a")
  def trye2 = trye({error("boom");"a"})((e:Exception) => e.getMessage) must_== Left("boom")

  def catchAll1 = catchAll("a")((e:Throwable) => e.getMessage) must_== Right("a")
  def catchAll2 = catchAll({throw new Error("boom");"a"})((e:Throwable) => e.getMessage) must_== Left("boom")
}