package org.specs2
package control

class ExceptionsSpec extends SpecificationWithJUnit with Exceptions {  def is =
                                                                                                                        """
The Exceptions trait provide functional ways to catch exceptions and deal with them:

 * tryo returns the result in an Option
 * trye returns the result in an Either with the Left value being a function of the exception
 * catchAll is similar to trye but even catches Throwables
 * tryOr returns the result or a value built from the thrown exception
 * catchAllOr is similar to tryOr but even catches Throwables
 * tryOrElse returns the result or a default value
 * tryMap returns different values depending on the success of the expression
 * tryOk returns true iff the expression doesn't throw an Exception
                                                                                                                        """^
                                                                                                                        p^
  "tryo executes an expression and returns an Option"                                                                   ^
    "Some(result) if the expression doesn't throw an exception"                                                         ! tryo1^
    "None if the expression failed"                                                                                     ! tryo2^
                                                                                                                        p^
  "tryOr executes an expression and returns the result or a default value. It returns"                                  ^
    "the result if the expression doesn't throw an exception"                                                           ! tryOr1^
    "a default value if the expression throws an exception"                                                             ! tryOr2^
                                                                                                                        p^
  "tryOrElse executes an expression and returns the result or another value. It returns"                                ^
    "the result if the expression doesn't throw an exception"                                                           ! tryOrElse1^
    "another value if the expression throws an exception"                                                               ! tryOrElse2^
                                                                                                                        p^
  "tryMap executes an expression and returns"                                                                           ^
    "a 'ok' value if the expression doesn't throw an exception"                                                         ! tryMap1^
    "a 'ko' value if the expression throws an exception"                                                                ! tryMap2^
                                                                                                                        p^
  "tryOk executes an expression and returns a Boolean"                                                                  ^
    "true if the expression doesn't throw an exception"                                                                 ! tryOk1^
    "false if the expression throws an exception"                                                                       ! tryOk2^
                                                                                                                        p^
  "trye executes an expression and returns an Either value"                                                             ^
    "Right(result) if the expression doesn't throw an exception"                                                        ! trye1^
    "Left(f(e)) if the expression failed, where f is a function of an exception"                                        ! trye2^
                                                                                                                        p^
  "catchAll executes an expression and returns an Either value"                                                         ^
    "Right(result) if the expression doesn't throw a Throwable"                                                         ! catchAll1^
    "Left(f(e)) if the expression threw anything, even an error"                                                        ! catchAll2^
                                                                                                                        p^
  "tryOr executes an expression and returns the result or a default value. It returns"                                  ^
    "the result if the expression doesn't throw an exception"                                                           ! catchAllOr1^
    "a default value if the expression throws an exception"                                                             ! catchAllOr2^
                                                                                                                        end
    
  def boom = { sys.error("boom"); "a" }

  def tryo1 = tryo("a") must_== Some("a")
  def tryo2 = tryo(boom) must_== None

  def tryOr1 = tryOr("a")((e:Exception) => e.getMessage) must_== "a"
  def tryOr2 = tryOr(boom)((e:Exception) => "bang") must_== "bang"

  def tryOrElse1 = tryOrElse("a")("b") must_== "a"
  def tryOrElse2 = tryOrElse(boom)("bang") must_== "bang"

  def tryMap1 = tryMap("a")(true)(false) must_== true
  def tryMap2 = tryMap(boom)(true)(false) must_== false

  def tryOk1 = tryOk("a") must_== true
  def tryOk2 = tryOk(boom) must_== false

  def trye1 = trye("a")((e:Exception) => e.getMessage) must_== Right("a")
  def trye2 = trye(boom)((e:Exception) => e.getMessage) must_== Left("boom")

  def catchAll1 = catchAll("a")((e:Throwable) => e.getMessage) must_== Right("a")
  def catchAll2 = catchAll({throw new Error("boom");"a"})((e:Throwable) => e.getMessage) must_== Left("boom")

  def catchAllOr1 = catchAllOr("a")((e:Throwable) => e.getMessage) must_== "a"
  def catchAllOr2 = catchAllOr({throw new Error("boom");"a"})((e:Throwable) => "bang") must_== "bang"
}