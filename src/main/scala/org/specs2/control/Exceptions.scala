package org.specs2
package control

private[specs2]
trait Exceptions {
  def tryo[T](a: =>T): Option[T] = {
	try { Some(a) }
	catch { case e: Exception => None }
  }
  def tryOr[T](a: =>T)(f: Exception =>  T): T = {
	trye(a)(f).fold(identity, identity)
  }
  def trye[T, S](a: =>T)(f: Exception =>S): Either[S, T] = {
	try { Right(a) }
	catch { case e: Exception => Left(f(e)) }
  }
  def catchAll[T, S](a: =>T)(f: Throwable =>S): Either[S, T] = {
	try { Right(a) }
	catch { case e: Throwable => Left(f(e)) }
  }
}

private[specs2]
object Exceptions extends Exceptions