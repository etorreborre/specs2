package org.specs2
package control

trait Exceptions {
  def tryo[T, S](a: =>T)(f: Throwable =>S): Either[S, T] = {
	try { Right(a) }
	catch { case e => Left(f(e)) }
  }
}
object Exceptions extends Exceptions