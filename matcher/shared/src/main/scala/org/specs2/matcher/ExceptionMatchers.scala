package org.specs2
package matcher

import control.Exceptions.*
import scala.reflect.ClassTag
import reflect.ClassName.*
import text.NotNullStrings.*
import BeMatching.{given, *}
import execute.*, Result.*
import ResultImplicits.*
import AnyMatchers.{haveClass}
import StringMatchers.{beMatching}
import org.specs2.control.Action.exception

/** These matchers can be used to check if exceptions are thrown or not
  */
trait ExceptionMatchers extends ExpectationsCreation:
  /** @return
    *   a matcher checking the type of an Exception
    */
  def throwA[E <: Throwable](using m: ClassTag[E]): ExceptionMatcher[E] =
    m.toString match
      case "Nothing" => new ExceptionMatcher[E](classOf[Throwable], None)
      case _         => new ExceptionMatcher[E](m.runtimeClass, None)

  /** @return
    *   a matcher checking the type of an Exception and its message (as a regexp)
    */
  def throwA[E <: Throwable](message: String = ".*")(using m: ClassTag[E]): ExceptionMatcher[E] =
    throwA[E].like { case e: Throwable => createExpectable(e.getMessage.notNull).applyMatcher(withPart(message)) }

  /** @return
    *   a matcher checking the value of an Exception
    */
  def throwA[E <: Throwable](e: E)(using m: ClassTag[E]): ExceptionMatcher[E] =
    throwA[E].like { case actual => createExpectable(actual.getMessage).applyMatcher(BeEqualTo(e.getMessage)) }

  /** alias for throwA
    */
  def throwAn[E <: Throwable](using m: ClassTag[E]): ExceptionMatcher[E] =
    throwA[E]

  /** alias for throwA
    */
  def throwAn[E <: Throwable](message: String = ".*")(using m: ClassTag[E]): ExceptionMatcher[E] =
    throwA(message)

  /** alias for throwA
    */
  def throwAn[E <: Throwable](e: E)(using m: ClassTag[E]): ExceptionMatcher[E] =
    throwA(e)

  /** check if a Throwable has a specific class and error message The message must be a regular expression, for example
    * (new IllegalArgumentException("incorrect arguments"): Throwable) must
    * beException[IllegalArgumentException](".*arguments.*")
    */
  def beException[T: ClassTag](message: String): Matcher[Throwable] =
    haveClass[T] and beMatching(message) ^^ ((t: Throwable) => t.getMessage)

  /** An exception matcher checks if an expression throws some specified exceptions:
    *   - by checking the type of exception
    *     - if no exception is thrown it returns a success
    *     - if an exception is thrown with with a subtype of java.lang.Error and we expect an Exception then it is
    *       re-thrown
    *     - if an exception is thrown with a different type it returns a failure
    *   - by checking the exception message
    *   - by checking a condition on the exception
    */
  class ExceptionMatcher[E <: Throwable](klass: Class[?], pf: Option[PartialFunction[E, Result]]) extends Matcher[Any]:
    outer =>

    def apply[S <: Any](value: Expectable[S]): Result =
      getException[E](value.value) match
        case Some(e) =>
          errorMustBeThrownIfExceptionIsExpected(e, klass)

          if klass.isAssignableFrom(e.getClass) then
            pf match {
              case None =>
                Success(
                  s"Caught an exception of type ${klass.getName}: ${e.getMessage}\n\nThe stacktrace is\n\n${e.getStackTrace
                      .mkString("\n")}"
                )
              case Some(f) =>
                val result = f(e.asInstanceOf[E])
                if result.isSuccess then
                  Success(
                    s"Caught an exception of type ${klass.getName}, verifying the partial function: ${e.getMessage}\n" +
                      s"The stacktrace is\n\n${e.getStackTrace.mkString("\n")}"
                  )
                else
                  Failure(
                    s"Caught an exception of type ${klass.getName}: ${e.getMessage}\n" +
                      s"However the exception does not satisfy the specified condition: ${result.message}\n" +
                      s"The stacktrace is\n\n${e.getStackTrace.mkString("\n")}"
                  )
            }
          else
            Failure(
              s"Caught an exception of type ${e.getClass.getName} which is not of type ${klass.getName}: ${e.getMessage}\nThe stacktrace is\n\n${e.getStackTrace
                  .mkString("\n")}"
            )
        case None =>
          Failure(s"No exception of type ${klass.getName} was thrown")

    /** Specify an additional condition to check on a thrown exception
      */
    def like[R: AsResult](pf: PartialFunction[E, R]): ExceptionMatcher[E] =
      ExceptionMatcher(klass, Some({ case e: E => AsResult(pf(e)) }))

    /** Negate the exception matcher to assert that an exception must *not* be thrown
      *   - if no exception is thrown this is a success
      *   - if an exception is thrown with the specified type, it is a failure
      *   - if an exception is thrown with a different type, the exception is re-thrown in order to be interpreted as a
      *     programming error by the library
      */
    override def not: Matcher[Any] =
      new Matcher[Any]:
        def apply[S <: Any](value: Expectable[S]): Result = getException[E](value.value) match
          case Some(e) =>
            if klass.isAssignableFrom(e.getClass) then
              pf match {
                case None =>
                  Failure(
                    s"Caught an exception of type ${klass.getName}: ${e.getMessage}\n\nThe stacktrace is\n\n${e.getStackTrace
                        .mkString("\n")}"
                  )
                case Some(f) =>
                  val result = f(e.asInstanceOf[E])
                  if result.isSuccess then
                    Failure(
                      s"Caught an exception of type ${klass.getName}: ${e.getMessage}\n" +
                        s"The exception satisfies the specified condition: ${result.message}\n" +
                        s"The stacktrace is\n\n${e.getStackTrace.mkString("\n")}"
                    )
                  else
                    Success(
                      s"Caught an exception of type ${klass.getName}: ${e.getMessage}\n" +
                        s"However the exception does not satisfy the specified condition: ${result}\n" +
                        s"The stacktrace is\n\n${e.getStackTrace.mkString("\n")}"
                    )
              }
            else
              Success(
                s"Caught an exception of type ${e.getClass.getName} which is not of type ${klass.getName}: ${e.getMessage}\n" +
                  s"The stacktrace is\n\n${e.getStackTrace.mkString("\n")}"
              )
          case None => Success(s"No exception of type ${klass.getName} was thrown")

  /** re-throw a java.lang.Error if an Exception was expected but a java.lang.Error was thrown */
  private def errorMustBeThrownIfExceptionIsExpected(e: Throwable, klass: Class[?]) =
    if classOf[Exception].isAssignableFrom(klass) && classOf[java.lang.Error].isAssignableFrom(e.getClass) then throw e

  /** Evaluates a value and return any exception that is thrown In the case of the use of the like method
    * (throwAn[Exception].like) the value will be an Expectable encapsulating the real value which needs to be evaluated
    */
  private def getException[E <: Throwable](value: =>Any): Option[Throwable] =
    catchAll {
      value.asInstanceOf[Matchable] match
        case e: Expectable[?] => e.value
        case _                => value
    }(identity).left.toOption

object ExceptionMatchers extends ExceptionMatchers
