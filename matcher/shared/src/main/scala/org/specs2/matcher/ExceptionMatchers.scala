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

/** These matchers can be used to check if exceptions are thrown or not
  */
trait ExceptionMatchers extends ExpectationsCreation:
  /** @return
    *   a matcher checking the type of an Exception
    */
  def throwA[E <: Throwable](using m: ClassTag[E]): ExceptionClassMatcher =
    m.toString match
      case "Nothing" => new ExceptionClassMatcher(classOf[Throwable])
      case _         => new ExceptionClassMatcher(m.runtimeClass)

  /** @return
    *   a matcher checking the type of an Exception and its message (as a regexp)
    */
  def throwA[E <: Throwable](message: String = ".*")(using m: ClassTag[E]): Matcher[Any] =
    throwA.like { case e: Throwable => createExpectable(e.getMessage.notNull).applyMatcher(withPart(message)) }

  /** @return
    *   a matcher checking the value of an Exception
    */
  def throwA[E <: Throwable](e: E): ExceptionMatcher[E] = new ExceptionMatcher(e)

  /** alias for throwA
    */
  def throwAn[E <: Throwable](using m: ClassTag[E]): ExceptionClassMatcher = throwA[E]

  /** alias for throwA
    */
  def throwAn[E <: Throwable](message: String = ".*")(using m: ClassTag[E]): Matcher[Any] = throwA(message)

  /** alias for throwA
    */
  def throwAn[E <: Throwable](e: E): ExceptionMatcher[E] = throwA(e)

  /** check if a Throwable has a specific class and error message The message must be a regular expression, for example
    * (new IllegalArgumentException("incorrect arguments"): Throwable) must
    * beException[IllegalArgumentException](".*arguments.*")
    */
  def beException[T: ClassTag](message: String): Matcher[Throwable] =
    haveClass[T] and beMatching(message) ^^ ((t: Throwable) => t.getMessage)

  /** Exception matcher checking the type of a thrown exception.
    */
  class ExceptionClassMatcher(klass: Class[?]) extends Matcher[Any]:
    outer =>

    def apply[S <: Any](value: Expectable[S]) =
      checkBoolean(value, checkClassType, andFinally = dropException)

    def like[T](f: PartialFunction[Throwable, Result]): Matcher[T] =
      new Matcher[T]:
        def apply[S <: T](value: Expectable[S]) =
          checkResult(value, checkClassType, f, dropException)

        override def not: Matcher[T] =
          new Matcher[T]:
            def apply[S <: Any](value: Expectable[S]) =
              checkResult(value, checkClassType, f, rethrowException).not

    private val checkClassType = (e: Throwable) => {
      errorMustBeThrownIfExceptionIsExpected(e, klass)
      klass.isAssignableFrom(e.getClass)
    }

    private def checkBoolean[T, R](expectable: Expectable[T], f: Throwable => Boolean, andFinally: Throwable => Unit) =
      checkExceptionValue(expectable, f, asString(klass), andFinally)

    private def checkResult[T](
        expectable: Expectable[T],
        e: Throwable => Boolean,
        f: PartialFunction[Throwable, Result],
        andFinally: Throwable => Unit
    ) =
      checkExceptionValueWithMatcher(expectable, e, f, asString(klass), andFinally)

    private def asString(exception: Any) =
      exception.asInstanceOf[Matchable] match
        case e: Class[?]   => e.getName
        case ex: Throwable => ex.getClass.getName + ": " + ex.getMessage.notNull
        case other         => exception.toString

    override def not: Matcher[Any] =
      new Matcher[Any]:
        def apply[S <: Any](value: Expectable[S]) =
          checkBoolean(value, checkClassType, rethrowException).not

  /** re-throw an Error if an Exception was expected */
  private def errorMustBeThrownIfExceptionIsExpected(e: Throwable, klass: Class[?]) =
    if classOf[Exception].isAssignableFrom(klass) && classOf[Error].isAssignableFrom(e.getClass) then throw e

  /** This matchers matches exception instances.
    * @see
    *   throwA
    */
  class ExceptionMatcher[E <: Throwable](exception: E) extends Matcher[Any]:
    outer =>

    def apply[S <: Any](value: Expectable[S]) =
      checkBoolean(value, checkClassTypeAndMessage, dropException)

    def like(f: PartialFunction[E, Result]): Matcher[Any] =
      new Matcher[Any]:
        def apply[S <: Any](value: Expectable[S]) =
          checkResult(value, checkClassTypeAndMessage, f, dropException)

        override def not: Matcher[Any] =
          new Matcher[Any]:
            def apply[S <: Any](value: Expectable[S]) =
              checkResult(value, checkClassTypeAndMessage, f, rethrowException).not

    private val checkClassTypeAndMessage = (e: Throwable) => {
      errorMustBeThrownIfExceptionIsExpected(e, exception.getClass)
      exception.getClass == e.getClass && exception.getMessage.notNull == e.getMessage.notNull
    }

    private def checkBoolean[T](expectable: Expectable[T], f: Throwable => Boolean, andFinally: Throwable => Unit) =
      checkExceptionValue(expectable, f, exception.toString, dropException)

    private def checkResult[T](
        expectable: Expectable[T],
        e: Throwable => Boolean,
        f: PartialFunction[E, Result],
        andFinally: Throwable => Unit
    ) =
      checkExceptionValueWithMatcher(expectable, e, f, exception.toString, dropException)

    override def not: Matcher[Any] =
      new Matcher[Any]:
        def apply[S <: Any](value: Expectable[S]) =
          checkBoolean(value, checkClassTypeAndMessage, rethrowException).not

  end ExceptionMatcher

  private val dropException = (e: Throwable) => ()

  private val rethrowException = (e: Throwable) => throw e

  /** use the andFinally continuation when a result is not successful in order to know what to do of unexpected
    * exceptions for the case expr must not(throw[ExceptionX])
    */
  private def rethrowFinally(e: Throwable, andFinally: Throwable => Unit)(r: Result): Result =
    if !r.isSuccess then andFinally(e)
    r

  private def checkExceptionValue[T](
      expectable: Expectable[T],
      f: Throwable => Boolean,
      expectedAsString: String,
      andFinally: Throwable => Unit
  ) =
    checkException(
      expectable,
      f,
      (e: Throwable) =>
        s"Expected: $expectedAsString. Got: $e instead \n\n The  ${e.getClass.simpleName} stacktrace is\n\n${e.getStackTrace
            .mkString("\n")}",
      "Expected: " + expectedAsString + ". Got nothing",
      andFinally
    )

  private def checkExceptionValueWithMatcher[T, E <: Throwable](
      expectable: Expectable[T],
      e: Throwable => Boolean,
      f: PartialFunction[E, Result],
      expectedAsString: String,
      andFinally: Throwable => Unit
  ) =
    checkExceptionWithMatcher(
      expectable,
      e,
      f,
      (e: Throwable) => s"Expected: $expectedAsString. Got: $e",
      "Expected: " + expectedAsString + ". Got nothing",
      (e: Throwable) => e.getStackTrace.toSeq,
      andFinally
    )

  private def checkException[T](
      expectable: Expectable[T],
      f: Throwable => Boolean,
      someKo: Throwable => String,
      noneKo: String,
      andFinally: Throwable => Unit
  ) =
    getException(expectable.value) match
      case Some(e) =>
        rethrowFinally(e, andFinally) {
          Result.result(f(e), someKo(e))
        }

      case _ =>
        Result.result(false, noneKo)

  private def checkExceptionWithMatcher[T, E <: Throwable](
      expectable: Expectable[T],
      ef: Throwable => Boolean,
      f: PartialFunction[E, Result],
      someKo: Throwable => String,
      noneKo: String,
      stacktrace: Throwable => Seq[StackTraceElement],
      andFinally: Throwable => Unit
  ) =
    getException(expectable.value) match
      case Some(e) =>
        rethrowFinally(e, andFinally) {
          if ef(e) then
            val likeResult = f(e.asInstanceOf[E])
            Result.result(
              ef(e) && likeResult.isSuccess,
              s"""${someKo(e)} and ${likeResult.message}\n\n The ${e.getClass.simpleName} stacktrace is\n\n${stacktrace(
                  e
                ).mkString("\n")}"""
            )
          else Result.result(false, someKo(e))
        }

      case _ =>
        Result.result(false, noneKo)

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
