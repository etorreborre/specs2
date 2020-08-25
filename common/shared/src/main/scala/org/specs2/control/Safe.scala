package org.specs2
package control

/**
 * Typeclass for "safe" actions which are actions which can
 * be attempted or finalized
 */
trait Safe[F[_]]:
  def finalizeWith[A](fa: F[A], f: Finalizer): F[A]
  def attempt[A](fa: F[A]): F[Throwable Either A]

object Safe:
  def apply[F[_]](implicit f: Safe[F]): Safe[F] =
    f

/**
 * Delayed action
 */
case class Finalizer(run: () => Unit):
  def attempt: Option[Throwable] =
    try { run(); None }
    catch { case t: Throwable => Some(t) }

object Finalizer:
  def runFinalizers(finalizers: Vector[Finalizer]): Unit =
    finalizers.foreach(_.attempt)

  def create(action: =>Unit): Finalizer =
    Finalizer(() => action)
