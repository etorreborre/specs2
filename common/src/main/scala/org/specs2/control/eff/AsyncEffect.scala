package org.specs2.control
package eff

import all._
import syntax.all._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scalaz._, Scalaz._
import scalaz.concurrent.Task
import scala.concurrent.Promise

trait AsyncEffect extends AsyncCreation with AsyncInterpretation
object AsyncEffect extends AsyncEffect

/**
 * Asynchronous effects, using scalaz task as the underlying implementation for now.
 * This can be switch to a Future implementation later.
 */
trait AsyncCreation {

  type _async[R] = Async |= R
  type _Async[R] = Async <= R

  def asyncNow[R :_async, A](a: A): Eff[R, A] =
    create[R, A](Task.now(a))

  def asyncFail[R :_async, A](t: Throwable): Eff[R, A] =
    create[R, A](Task.fail(t))

  def asyncDelay[R :_async, A](a: =>A): Eff[R, A] =
    create[R, A](Task.delay(a))

  def asyncDelayAttempt[R :_async :_throwableOr, A](a: =>A): Eff[R, A] =
    asyncDelay(\/.fromTryCatchNonFatal(a)).collapse

  def asyncFork[R :_async, A](a: =>A): Eff[R, A] =
    create[R, A](Task.fork(Task.delay(a)))

  /** use a ThrowableXor effect to store any exception */
  def asyncForkAttempt[R :_async :_throwableOr, A](a: =>A): Eff[R, A] =
  asyncFork(\/.fromTryCatchNonFatal(a)).collapse

  def fromFuture[R :_async, A](f: =>Future[A])(implicit ec: ExecutionContext): Eff[R, A] =
    create[R, A](Task.async[A] { register =>
      try {
        f.onComplete {
          case scala.util.Success(a) => register(\/-(a))
          case scala.util.Failure(e) => register(-\/(e))
        }
      } catch {
        case NonFatal(t) => register(-\/(t))
      }
    })

  /** use a ThrowableXor effect to store any exception */
  def fromFutureAttempt[R :_async :_throwableOr, A](f: =>Future[A])(implicit ec: ExecutionContext): Eff[R, A] =
    fromFuture(Catchable[Future].attempt(f)).collapse

  def fromTask[R :_async, A](t: Task[A]): Eff[R, A] =
    send[Async, R, A](AsyncTask(t))

  /** use a ThrowableXor effect to store any exception */
  def fromTaskAttempt[R :_async :_throwableOr, A](t: Task[A]): Eff[R, A] =
    fromTask(t.attempt).flatMap {
      case \/-(a) => right[R, Throwable, A](a)
      case -\/(e) => left[R, Throwable, A](e)
    }

  private def create[R :_async, A](t: Task[A]): Eff[R, A] =
    send[Async, R, A](AsyncTask(t))
}

object AsyncCreation extends AsyncCreation

trait AsyncInterpretation { outer =>
  def runAsyncTask[A](e: Eff[Fx.fx1[Async], A]): Task[A] =
    e.detach match { case AsyncTask(a) => a }

  def runAsyncFuture[A](e: Eff[Fx.fx1[Async], A]): Future[A] =
    e.detach match { case AsyncTask(t) => taskToFuture(t) }

  def taskToFuture[T](task: Task[T]): Future[T] = {
    val p: Promise[T] = Promise()

    task.unsafePerformAsync {
      case -\/(ex) => p.failure(ex); ()
      case \/-(r) => p.success(r); ()
    }

    p.future
  }

}

object AsyncInterpretation extends AsyncInterpretation

sealed trait Async[A]
case class AsyncTask[A](run: Task[A]) extends Async[A]

/**
 * The Monad instance is necessary to be able to do a detach operation
 * when Async is the last effect of the stack (see runAsyncTask)
 */
object Async {
  implicit def MonadAsync: Monad[Async] = new Monad[Async] {
    def point[A](a: =>A) = AsyncTask(Task.now(a))

    def bind[A, B](fa: Async[A])(f: A => Async[B]): Async[B] =
      AsyncTask {
        fa match { case AsyncTask(fa1) =>
          fa1.flatMap(a => f(a) match { case AsyncTask(fa2) => fa2 })
        }
      }
  }
}

