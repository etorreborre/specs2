package org.specs2
package matcher

import control.Exceptions._
import org.specs2.execute.Result
import scala.reflect.ClassTag
import reflect.ClassName._
import text.NotNullStrings._

/**
 * These matchers can be used to check if exceptions are thrown or not
 */
trait ExceptionMatchers extends ExceptionBaseMatchers with ExceptionBeHaveMatchers
object ExceptionMatchers extends ExceptionMatchers

private[specs2]
trait ExceptionBaseMatchers extends ExpectationsCreation {
  /**
   * @return a matcher checking the type of an Exception
   */
  def throwA[E <: Throwable](implicit m: ClassTag[E]): ExceptionClassMatcher =
    m.toString match {
      case "Nothing" => new ExceptionClassMatcher(classOf[Throwable])
      case _         => new ExceptionClassMatcher(m.runtimeClass)
    }

  /**
   * @return a matcher checking the type of an Exception and its message (as a regexp)
   */
  def throwA[E <: Throwable](message: String = ".*")(implicit m: ClassTag[E]): Matcher[Any] = {
    throwA(m).like { case e: Throwable => createExpectable(e.getMessage.notNull).applyMatcher(BeMatching.withPart(message)) }
  }
  /**
   * @return a matcher checking the value of an Exception
   */
  def throwA[E <: Throwable](e: E): ExceptionMatcher[E] = new ExceptionMatcher(e)
  /**
   * alias for throwA
   */
  def throwAn[E <: Throwable](implicit m: ClassTag[E]) = throwA[E](m)
  /**
   * alias for throwA
   */
  def throwAn[E <: Throwable](message: String = ".*")(implicit m: ClassTag[E]): Matcher[Any] = throwA(message)(m)
  /**
   * alias for throwA
   */
  def throwAn[E <: Throwable](e: E) = throwA(e)

  /**
   * Exception matcher checking the type of a thrown exception.
   */
  class ExceptionClassMatcher(klass: Class[_]) extends Matcher[Any] { outer =>

    def apply[S <: Any](value: Expectable[S]) =
      checkBoolean(value, checkClassType, andFinally = dropException)

    def like[T](f: PartialFunction[Throwable, MatchResult[Any]]) = new Matcher[T] {
      def apply[S <: T](value: Expectable[S]) =
        checkMatchResult(value, checkClassType, f, dropException)

      override def not = new Matcher[T] {
        def apply[S <: Any](value: Expectable[S]) =
          checkMatchResult(value, checkClassType, f, rethrowException).negate
      }
    }

    private val checkClassType = (e: Throwable) => {
      errorMustBeThrownIfExceptionIsExpected(e, klass)
      klass.isAssignableFrom(e.getClass)
    }

    private def checkBoolean[T, R](expectable: Expectable[T], f: Throwable => Boolean, andFinally: Throwable => Unit) =
      checkExceptionValue(expectable, f, asString(klass), andFinally)

    private def checkMatchResult[T](expectable: Expectable[T], e: Throwable => Boolean, f: PartialFunction[Throwable, MatchResult[Any]], andFinally: Throwable => Unit) =
      checkExceptionValueWithMatcher(expectable, e, f, asString(klass), andFinally)

    private def asString(exception: Any) =
      exception match {
        case e: Class[_]   => e.getName
        case ex: Throwable => ex.getClass.getName + ": " + ex.getMessage.notNull
        case other         => other.toString
      }

    override def not = new Matcher[Any] {
      def apply[S <: Any](value: Expectable[S]) =
        checkBoolean(value, checkClassType, rethrowException).negate
    }
  }

  /** re-throw an Error if an Exception was expected */
  private def errorMustBeThrownIfExceptionIsExpected(e: Throwable, klass: Class[_]) =
    if (classOf[Exception].isAssignableFrom(klass) && classOf[Error].isAssignableFrom(e.getClass)) throw e

  /**
   * This matchers matches exception instances.
   * @see throwA
   */
  class ExceptionMatcher[E <: Throwable](exception: E) extends Matcher[Any] { outer =>
    def apply[S <: Any](value: Expectable[S]) = {
      checkBoolean(value, checkClassTypeAndMessage, dropException)
    }

    def like(f: PartialFunction[E, MatchResult[Any]]) = new Matcher[Any] {
      def apply[S <: Any](value: Expectable[S]) =
        checkMatchResult(value, checkClassTypeAndMessage, f, dropException)

      override def not = new Matcher[Any] {
        def apply[S <: Any](value: Expectable[S]) =
          checkMatchResult(value, checkClassTypeAndMessage, f, rethrowException).negate
      }
    }

    private val checkClassTypeAndMessage = (e: Throwable) => {
      errorMustBeThrownIfExceptionIsExpected(e, exception.getClass)
      exception.getClass == e.getClass && exception.getMessage.notNull == e.getMessage.notNull
    }

    private def checkBoolean[T](expectable: Expectable[T], f: Throwable => Boolean, andFinally: Throwable => Unit) =
      checkExceptionValue(expectable, f, exception.toString, dropException)

    private def checkMatchResult[T](expectable: Expectable[T], e: Throwable => Boolean, f: PartialFunction[E, MatchResult[Any]], andFinally: Throwable => Unit) =
      checkExceptionValueWithMatcher(expectable, e, f, exception.toString, dropException)

    override def not = new Matcher[Any] {
      def apply[S <: Any](value: Expectable[S]) =
        checkBoolean(value, checkClassTypeAndMessage, rethrowException).negate
    }

  }

  private val dropException = (e: Throwable) => ()

  private val rethrowException = (e: Throwable) => throw e

  /**
   * use the andFinally continuation when a result is not succesfull in order to
   * know what to do of unexpected exceptions for the case
   * expr must not(throw[ExceptionX])
    */
  private def rethrowFinally[T](e: Throwable, andFinally: Throwable => Unit)(r: MatchResult[T]): MatchResult[T] = {
    if (!r.isSuccess) andFinally(e)
    r
  }

  private def checkExceptionValue[T](expectable: Expectable[T], f: Throwable => Boolean, expectedAsString: String, andFinally: Throwable => Unit) = {
    checkException(expectable,
                   f,
                   (e: Throwable) => s"Got the exception $e",
                   (e: Throwable) => s"Expected: $expectedAsString. Got: $e instead \n\n The  ${e.getClass.simpleName} stacktrace is\n\n${e.getStackTrace.mkString("\n")}",
                   "Got the exception " + expectedAsString,
                   "Expected: "+ expectedAsString + ". Got nothing",
                   andFinally)
  }

  private def checkExceptionValueWithMatcher[T, E <: Throwable](
    expectable: Expectable[T],
    e: Throwable => Boolean,
    f: PartialFunction[E, MatchResult[Any]],
    expectedAsString: String,
    andFinally: Throwable => Unit) = {

    checkExceptionWithMatcher(expectable,
                   e, f,
                   (e: Throwable) => "Got the exception " + e,
                   (e: Throwable) => s"Expected: $expectedAsString. Got: $e",
                   "Got the exception " + expectedAsString,
                   "Expected: "+ expectedAsString + ". Got nothing",
                   (e: Throwable) => e.getStackTrace.toSeq,
                   andFinally)
  }

  private def checkException[T](expectable: Expectable[T], f: Throwable => Boolean,
      someOk: Throwable => String, someKo: Throwable => String,
      noneOk: String, noneKo: String,
      andFinally: Throwable => Unit) = {

    getException(expectable.value) match {
      case Some(e) =>
        rethrowFinally(e, andFinally) {
          Matcher.result(f(e), someOk(e), someKo(e), expectable)
        }

      case None =>
        Matcher.result(false, noneOk, noneKo, expectable)
    }
  }

  private def checkExceptionWithMatcher[T, E <: Throwable](expectable: Expectable[T], ef: Throwable => Boolean, f: PartialFunction[E, MatchResult[Any]],
      someOk: Throwable => String, someKo: Throwable => String,
      noneOk: String, noneKo: String, stacktrace: Throwable => Seq[StackTraceElement],
      andFinally: Throwable => Unit) = {

    getException(expectable.value) match {
      case Some(e) =>
        rethrowFinally(e, andFinally) {
          if (ef(e)) {
            val likeResult = f(e.asInstanceOf[E])
            Matcher.result(ef(e) && likeResult.isSuccess,
                           s"""${someOk(e)} and ${likeResult.message}""",
                           s"""${someKo(e)} and ${likeResult.message}\n\n The ${e.getClass.simpleName} stacktrace is\n\n${stacktrace(e).mkString("\n")}""", expectable)
          }
          else Matcher.result(false, someOk(e), someKo(e), expectable)
        }

      case None =>
        Matcher.result(false, noneOk, noneKo, expectable)
    }
  }

  /**
   * Evaluates a value and return any exception that is thrown
   * In the case of the use of the like method (throwAn[Exception].like) the value
   * will be an Expectable encapsulating the real value which needs to be evaluated
   */
  private def getException[E <: Throwable](value: =>Any): Option[Throwable] = {
    catchAll {
      value match {
        case e: Expectable[_] => e.value
        case _ => value
      }
    }(identity).left.toOption
  }
}

private[specs2]
trait ExceptionBeHaveMatchers extends BeHaveMatchers { outer: ExceptionBaseMatchers =>
  implicit class ExceptionMatcherResult[T](result: MatchResult[T]) {
    def throwA[E <: Throwable](implicit m: ClassTag[E]) = result(outer.throwA(m))
    def throwA[E <: Throwable](message: String = ".*")(implicit m: ClassTag[E]) = result(outer.throwA(message)(m))
    def throwA[E <: Throwable](e: E) = result(outer.throwA(e))

    def throwAn[E <: Throwable](implicit m: ClassTag[E]) = result(outer.throwA(m))
    def throwAn[E <: Throwable](message: String = ".*")(implicit m: ClassTag[E]) = result(outer.throwA(message)(m))
    def throwAn[E <: Throwable](e: E) = result(outer.throwA(e))
  }
}
