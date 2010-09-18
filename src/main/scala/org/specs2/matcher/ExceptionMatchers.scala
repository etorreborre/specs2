package org.specs2
package matcher
import control.Exceptions._

trait ExceptionMatchers {
  /**
   * return a matcher which will be ok if an exception of that type is thrown
   */
  def throwA[E <: Throwable](implicit m: ClassManifest[E]) = new ExceptionClassMatcher(m.erasure)
  /**
   * @see throwException description
   */
  def throwA[E <: Throwable](e: E) = new ExceptionMatcher(e)
  /**
   * return a matcher which will be ok if an exception of that type is thrown
   */
  def throwAn[E <: Throwable](implicit m: ClassManifest[E]) = throwA[E](m)
  /**
   * Alias for throwA(new Exception)
   * @see throwException description
   */
  def throwAn[E <: Throwable](e: E) = throwA(e)
  /**
   * Exception matcher checking the type of a thrown exception.
   */
  class ExceptionClassMatcher(klass: Class[_]) extends Matcher[Any] {
    def apply[S <: Any : Expectable](value: =>S) = {
      getException(value) match {
    	case Some(e)  => 
    	  result(klass.isAssignableFrom(e.getClass), 
    	 		 asString(e) + " is an instance of " + asString(klass), 
    	 		 "Expected: "+ asString(klass) + ". Got: " + e + " instead")
    	case None => 
    	  result(false, 
    	 		 "An exception of class " + asString(klass) + " was thrown", 
    	 		 "Expected an exception of class " + asString(klass) + " but no exception was thrown")
      }
    }
    def like[T](f: =>PartialFunction[Throwable, Boolean]) = new Matcher[T] {
      def apply[S <: T : Expectable](v: =>S) = {
	    getException(value) match {
	   	  case Some(e)  => 
	   	    result(klass.isAssignableFrom(e.getClass) && f(e), 
	   	 	  	   e + " is an instance of " + asString(klass) + " as expected", 
	   	 		   "Expected: "+ asString(klass) + ". Got: " + e + " instead")
	   	  case None => 
	   	    result(false, 
	   	 		   "An exception of class " + asString(klass) + " was thrown", 
	   	 		   "Expected an exception of class " + asString(klass) + " but no exception was thrown")
	    }
      }
    }
    protected[matcher] def asString(exception: Any) = {
      exception match {
        case e: Class[_] => e.toString.replaceFirst("class ", "")
        case ex: Throwable => ex.getClass.getName + ": " + ex.getMessage
        case other => other.toString
      }
    }
  }
  /**
   * This matchers matches exception instances.
   * @see throwA
   */
  class ExceptionMatcher[E <: Throwable](exception: E) extends Matcher[Any] {
    def apply[S <: Any : Expectable](value: =>S) = {
      getException(value) match {
    	case Some(e)  => 
    	  result(exception.getClass == e.getClass && exception.getMessage == e.getMessage, 
    	 		 "Got the exception " + exception, 
    	 		 "Expected: "+ exception + ". Got: " + e + " instead")
    	case None => 
    	  result(false, 
    	 		 "Got the exception " + exception, 
    	 		 "Expected: "+ exception + ". Got nothing")
      }
    }
    def like(f: =>PartialFunction[E, Boolean]) = new Matcher[Any] {
      def apply[S <: Any : Expectable](v: =>S) = {
	    getException(value) match {
	   	  case Some(e)  => 
	   	    result(exception.getClass == e.getClass && exception.getMessage == e.getMessage && f(e.asInstanceOf[E]), 
    	 	  	   "Got the exception " + exception, 
    	 		   "Expected: "+ exception + ". Got: " + e + " instead")
    	  case None => 
    	    result(false, 
    	 		   "Got the exception " + exception, 
    	 		   "Expected: "+ exception + ". Got nothing")
	    }
      }
    }
  }
  protected[matcher] def message(exception: Any) = {
    exception match {
      case e: Class[_] => e.toString.replaceFirst("class ", "")
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