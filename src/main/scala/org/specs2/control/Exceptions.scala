package org.specs2
package control

trait Exceptions {
  def trye[T, S](a: =>T)(f: Exception =>S): Either[S, T] = {
	try { Right(a) }
	catch { case e: Exception => Left(f(e)) }
  }
}
object Exceptions extends Exceptions