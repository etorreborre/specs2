package org.specs2
package control
import scala.util.control.Exception._                                                                                          

/**
 * This trait provides methods to catch exceptions and transform them into values which can be passed to
 * further computations.
 *
 * It uses the facilities found in the scala.util.control.Exception object while providing
 * a more concise api on some use cases.
 *
 * @see org.specs2.control.ExceptionsSpec for examples
 */
private[specs2]
trait Exceptions {
  implicit def implicitUnit[T](t: T): Unit = ()
  
  /**
   * try to evaluate an expression, returning an Option
   * 
   * A function Exception => Unit can be used as a side-effect to print the exception
   * to the console for example.
   * 
   * The 'tryo' name comes from the lift project: http://liftweb.net
   * 
   * @return None if there is an exception, or Some(value)
   */
  def tryo[T](a: =>T)(implicit f: Exception => Unit): Option[T] = {
	  try { Some(a) }
	  catch { case e: Exception => {
	    f(e)
	    None
	  }}
  }
  /**
   * try to evaluate an expression, returning a value T
   * 
   * If the expression throws an Exception a function f is used to return a value
   * of the expected type.
   */
  def tryOr[T](a: =>T)(implicit f: Exception => T): T = {
	  trye(a)(f).fold(identity, identity)
  }
  /**
   * try to evaluate an expression and return it if nothing fails.
   * return ko otherwise
   */
  def tryOrElse[T](a: =>T)(ko: T): T = {
    tryo(a).map(identity).getOrElse(ko)
  }
  /**
   * try to evaluate an expression and return ok if nothing fails.
   * return ko otherwise
   */
  def tryMap[T, S](a: =>T)(ok: S)(ko: S): S = {
    tryo(a).map(x => ok).getOrElse(ko)
  }
  /**
   * try to evaluate an expression and return true if nothing fails.
   * return false otherwise
   */
  def tryOk[T](a: =>T) = tryMap(a)(true)(false)
  /**
   * try to evaluate an expression, returning Either
   * 
   * If the expression throws an Exception a function f is used to return the left value
   * of the Either returned value.
   */
  def trye[T, S](a: =>T)(implicit f: Exception =>S): Either[S, T] = {
	  try { Right(a) }
	  catch { case e: Exception => Left(f(e)) }
  }
  /**
   * try to evaluate an expression, returning Either
   * 
   * If the expression throws any Throwable a function f is used to return the left value
   * of the Either returned value.
   */
  def catchAll[T, S](a: =>T)(f: Throwable =>S): Either[S, T] = {
	  try { Right(a) }
	  catch { case e: Throwable => Left(f(e)) }
  }
}

private[specs2]
object Exceptions extends Exceptions