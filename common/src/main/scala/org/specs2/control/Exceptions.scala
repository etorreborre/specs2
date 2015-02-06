package org.specs2
package control

import scalaz.{-\/, \/-, \/}

/**
 * This trait provides methods to catch exceptions and transform them into values which can be passed to
 * further computations.
 *
 * It uses the facilities found in the scala.util.control.Exception object while providing
 * a more concise api on some use cases.
 *
 * @see org.specs2.control.ExceptionsSpec for examples
 */
trait Exceptions {

  /**
   * try to evaluate an expression, returning an Option
   *
   * The 'tryo' name comes from the lift project: http://liftweb.net
   *
   * @return None if there is an exception, or Some(value)
   */
  def tryo[T](a: =>T): Option[T] = {
    try Some(a)
    catch { case e: Exception => None }
  }

  /**
   * try to evaluate an expression, returning a value T
   *
   * If the expression throws an Exception a function f is used to return a value
   * of the expected type.
   */
  def tryOr[T](a: =>T)(f: Exception => T): T =
    trye(a)(f).fold(identity, identity)

  /**
   * try to evaluate an expression, returning a value T
   *
   * If the expression throws a Throwable a function f is used to return a value
   * of the expected type.
   */
  def catchAllOr[T](a: =>T)(f: Throwable => T): T = {
    try a
    catch { case e: Throwable => f(e) }
  }
  /**
   * try to evaluate an expression, returning a value T
   *
   * If the expression throws a Throwable, then return a default value
   */
  def catchAllOrElse[T](a: =>T)(ko: =>T): T = catchAllOr(a)((e: Throwable) => ko)
  /**
   * try to evaluate an expression and return it if nothing fails.
   * return ko otherwise
   */
  def tryOrElse[T](a: =>T)(ko: T): T = tryo(a).fold(ko)(identity)
  /**
   * try to evaluate an expression and return it in an Option if nothing fails.
   * return None otherwise
   */
  def tryOrNone[T](a: =>T): Option[T] = tryo(a).orElse(None)

  /**
   * try to evaluate an expression and return ok if nothing fails.
   * return ko otherwise
   */
  def tryMap[T, S](a: =>T)(ok: S)(ko: S): S =
    tryo(a).fold(ko)(_ => ok)

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
  def trye[T, S](a: =>T)(f: Exception => S): Either[S, T] = {
    try Right(a)
    catch { case e: Exception => Left(f(e)) }
  }

  /**
   * try to evaluate an expression, returning \/
   *
   * If the expression throws an Exception a function f is used to return the left value
   * of the Either returned value.
   */
  def try_\/[T, S](a: =>T)(f: Exception => S): \/[S, T] = {
    try \/-(a)
    catch { case e: Exception => -\/(f(e)) }
  }

  /** try to apply a partial function to a value */
  def tryCollect[T](a: =>T)(partialFunction: PartialFunction[T, Boolean]): Boolean =
    tryCollectOr(a, false)(partialFunction)

  /** try to apply a partial function to a value, with a default value if something goes wrong */
  def tryCollectOr[T, S](a: =>T, or: S)(partialFunction: PartialFunction[T, S]): S =
    tryOrElse { partialFunction.applyOrElse(a, (_: T) => or) }(or)

  /**
   * try to evaluate an expression, returning Either
   *
   * If the expression throws any Throwable a function f is used to return the left value
   * of the Either returned value.
   */
  def catchAll[T, S](a: =>T)(f: Throwable => S): Either[S, T] = {
    try Right(a)
    catch { case e: Throwable => Left(f(e)) }
  }

  /**
   * try to evaluate an expression, returning \/
   *
   * If the expression throws an Exception a function f is used to return the left value
   * of the Either returned value.
   */
  def catchAll_\/[T, S](a: =>T)(f: Throwable => S): \/[S, T] = {
    try \/-(a)
    catch { case e: Throwable => -\/(f(e)) }
  }
}

object Exceptions extends Exceptions