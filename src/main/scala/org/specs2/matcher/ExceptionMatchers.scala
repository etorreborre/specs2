package org.specs2
package matcher

import control.Exceptions._

/**
 * These matchers can be used to check if exceptions are thrown or not
 */
trait ExceptionMatchers {
  /**
   * @return a matcher checking the type of an Exception
   */
  def throwA[E <: Throwable](implicit m: ClassManifest[E]): ExceptionClassMatcher = new ExceptionClassMatcher(m.erasure)
  /**
   * @return a matcher checking the type of an Exception and its message (as a regexp)
   */
  def throwA[E <: Throwable](message: String = ".*")(implicit m: ClassManifest[E]): Matcher[Any] = {
    throwA(m).like { case e => Expectable(e.getMessage).applyMatcher(new BeMatching(".*"+message+".*")) }
  }
  /**
   * @return a matcher checking the value of an Exception
   */
  def throwA[E <: Throwable](e: E): ExceptionMatcher[E] = new ExceptionMatcher(e)
  /**
   * @alias for throwA
   */
  def throwAn[E <: Throwable](implicit m: ClassManifest[E]) = throwA[E](m)
  /**
   * @alias for throwA
   */
  def throwAn[E <: Throwable](message: String = ".*")(implicit m: ClassManifest[E]): Matcher[Any] = throwA(message)(m)
  /**
   * @alias for throwA
   */
  def throwAn[E <: Throwable](e: E) = throwA(e)
  /**
   * Exception matcher checking the type of a thrown exception.
   */
  class ExceptionClassMatcher(klass: Class[_]) extends Matcher[Any] { outer =>
    def apply[S <: Any](value: Expectable[S]) = checkBoolean(value, (e: Throwable) => classType(e))

    def like[T](f: PartialFunction[Throwable, MatchResult[Any]]) = new Matcher[T] {
      def apply[S <: T](value: Expectable[S]) = {
    	  checkMatchResult(value, (e: Throwable) => classType(e), f)
      }
    }
    private val classType = (e: Throwable) => klass.isAssignableFrom(e.getClass)
    private def checkBoolean[T, R](expectable: Expectable[T], f: Throwable => Boolean) = {
      checkExceptionValue(expectable, f, asString(klass))
    }
    private def checkMatchResult[T](expectable: Expectable[T], e: Throwable => Boolean, f: PartialFunction[Throwable, MatchResult[Any]]) = {
      checkExceptionValueWithMatcher(expectable, e, f, asString(klass))
    }
    private def asString(exception: Any) = {
      exception match {
        case e: Class[_] => e.getName
        case ex: Throwable => ex.getClass.getName + ": " + ex.getMessage
        case other => other.toString
      }
    }
  }
  /**
   * This matchers matches exception instances.
   * @see throwA
   */
  class ExceptionMatcher[E <: Throwable](exception: E) extends Matcher[Any] { outer =>
    def apply[S <: Any](value: Expectable[S]) = {
      checkBoolean(value, (e: Throwable) => classAndMessage(e))
    }
    def like(f: PartialFunction[E, MatchResult[Any]]) = new Matcher[Any] {
      def apply[S <: Any](value: Expectable[S]) =
    	  checkMatchResult(value, (e: Throwable) => classAndMessage(e), f)
    }
    private val classAndMessage = (e: Throwable) => exception.getClass == e.getClass && exception.getMessage == e.getMessage
    private def checkBoolean[T](expectable: Expectable[T], f: Throwable => Boolean) = {
      checkExceptionValue(expectable, f, exception.toString)
    }
    private def checkMatchResult[T](expectable: Expectable[T], e: Throwable => Boolean, f: PartialFunction[E, MatchResult[Any]]) = {
      checkExceptionValueWithMatcher(expectable, e, f, exception.toString)
    }
  }
  private def checkExceptionValue[T](expectable: Expectable[T], f: Throwable => Boolean, expectedAsString: String) = {
    checkException(expectable, 
		               f,
		               (e: Throwable) => "Got the exception " + e, 
		               (e: Throwable) => "Expected: "+ expectedAsString + ". Got: " + e + " instead",
		               "Got the exception " + expectedAsString, 
	 	               "Expected: "+ expectedAsString + ". Got nothing")
  }
  private def checkExceptionValueWithMatcher[T, E <: Throwable](expectable: Expectable[T], e: Throwable => Boolean, f: PartialFunction[E, MatchResult[Any]], expectedAsString: String) = {
    checkExceptionWithMatcher(expectable,
		               e, f,
		               (e: Throwable) => "Got the exception " + e,
		               (e: Throwable) => "Expected: "+ expectedAsString + ". Got: " + e + " instead",
		               "Got the exception " + expectedAsString,
	 	               "Expected: "+ expectedAsString + ". Got nothing")
  }
  private def checkException[T](expectable: Expectable[T], f: Throwable => Boolean,
		  someOk: Throwable => String, someKo: Throwable => String,
		  noneOk: String, noneKo: String) = {
    
    getException(expectable.value) match {
	    case Some(e) => Matcher.result(f(e), someOk(e), someKo(e), expectable)
	    case None    => Matcher.result(false, noneOk, noneKo, expectable)
    }
  }
  private def checkExceptionWithMatcher[T, E <: Throwable](expectable: Expectable[T], ef: Throwable => Boolean, f: PartialFunction[E, MatchResult[Any]],
		  someOk: Throwable => String, someKo: Throwable => String,
		  noneOk: String, noneKo: String) = {

    getException(expectable.value) match {
	    case Some(e) => {
        if (ef(e)) {
          val result = f(e.asInstanceOf[E])
          Matcher.result(ef(e) && result.isSuccess, someOk(e)+" ("+result.message+")", someKo(e)+" ("+result.message+")", expectable)
        }
        else
          Matcher.result(false, someOk(e), someKo(e), expectable)
      }
	    case None    => Matcher.result(false, noneOk, noneKo, expectable)
    }
  }


  private def message(exception: Any) = {
    exception match {
      case e: Class[_] => e.getName
      case ex: Throwable => ex.getClass.getName + ": " + ex.getMessage
      case other => other.toString
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