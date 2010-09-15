package org.specs2
package control

trait Exceptions {
  def tryo[T](a: =>T): Option[T] = {
	try { Some(a) }
	catch { case e: Exception => None }
  }
  def trye[T, S](a: =>T)(f: Exception =>S): Either[S, T] = {
	try { Right(a) }
	catch { case e: Exception => Left(f(e)) }
  }
}
object Exceptions extends Exceptions