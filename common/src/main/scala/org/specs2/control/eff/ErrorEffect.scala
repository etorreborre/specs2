package org.specs2.control.eff

import scala.util.control.NonFatal
import scalaz._, Scalaz._
import Eff._
import Interpret._
import org.specs2.control.eff.EvalEffect.Eval
import scala.reflect.ClassTag

/**
 * Effect for computation which can fail and return a Throwable, or just stop with a failure
 *
 * This effect is a mix of Eval and \/ in the sense that every computation passed to this effect (with the ok
 * method) is considered "impure" or "faulty" by default.
 *
 * The type F is used to represent the failure type.
 *
 */
trait ErrorEffect[F] extends
  ErrorCreation[F] with
  ErrorInterpretation[F]

trait ErrorTypes[F] {

  /** type of errors: exceptions or failure messages */
  type Error = Throwable \/ F

  /**
   * base type for this effect: either an error or a computation to evaluate
   * a "by-name" value
   */
  type ErrorOrOk[A] = Evaluate[F, A]

  type _ErrorOrOk[R] = ErrorOrOk <= R
  type _errorOrOk[R] = ErrorOrOk |= R
}

trait ErrorCreation[F] extends ErrorTypes[F] {
  /** create an Eff value from a computation */
  def ok[R :_errorOrOk, A](a: => A): Eff[R, A] =
    send[ErrorOrOk, R, A](Evaluate.ok[F, A](a))

  /** create an Eff value from a computation */
  def eval[R :_errorOrOk , A](a: Eval[A]): Eff[R, A] =
    send[ErrorOrOk, R, A](Evaluate.eval[F, A](a))

  /** create an Eff value from an error */
  def error[R :_errorOrOk, A](error: Error): Eff[R, A] =
    send[ErrorOrOk, R, A](Evaluate.error[F, A](error))

  /** create an Eff value from a failure */
  def fail[R :_errorOrOk, A](failure: F): Eff[R, A] =
    error(\/-(failure))

  /** create an Eff value from an exception */
  def exception[R :_errorOrOk, A](t: Throwable): Eff[R, A] =
    error(-\/(t))
}

trait ErrorInterpretation[F] extends ErrorCreation[F] { outer =>

  /**
   * Run an error effect.
   *
   * Stop all computation if there is an exception or a failure.
   */
  def runError[R, U, A](r: Eff[R, A])(implicit m: Member.Aux[ErrorOrOk, R, U]): Eff[U, Error \/ A] = {
    val recurse = new Recurse[ErrorOrOk, U, Error \/ A] {
      def apply[X](m: ErrorOrOk[X]) =
        m.run match {
          case -\/(e) =>
            \/-(EffMonad[U].point(-\/(e)))

          case \/-(a) =>
            try -\/(a.value)
            catch { case NonFatal(t) => \/-(EffMonad[U].point(-\/(-\/(t)))) }
        }

      def applicative[X, T[_]: Traverse](ms: T[ErrorOrOk[X]]): T[X] \/ ErrorOrOk[T[X]] =
        ms.map(_.run).sequence match {
          case -\/(e)   => \/-(Evaluate.error[F, T[X]](e))
          case \/-(ls) => \/-(Evaluate.ok[F, T[X]](ls.map(_.value)))
        }
    }

    interpret1[R, U, ErrorOrOk, A, Error \/ A]((a: A) => \/-(a): Error \/ A)(recurse)(r)
  }

  /**
   * evaluate 1 action possibly having error effects
   *
   * Execute a second action whether the first is successful or not
   */
  def andFinally[R, A](action: Eff[R, A], last: Eff[R, Unit])(implicit m: ErrorOrOk <= R): Eff[R, A] = {
    val recurse = new Recurse[ErrorOrOk, R, A] {
      def apply[X](current: ErrorOrOk[X]): X \/ Eff[R, A] =
        current.run match {
          case -\/(e) => \/-(last.flatMap(_ => outer.error[R, A](e)))
          case \/-(x) =>
            try -\/(x.value)
            catch { case NonFatal(t) => \/-(last.flatMap(_ => outer.exception[R, A](t))) }
        }

      def applicative[X, T[_]: Traverse](ms: T[ErrorOrOk[X]]): T[X] \/ ErrorOrOk[T[X]] =
        ms.map(_.run).sequence match {
          case -\/(e)   => \/-(Evaluate.error[F, T[X]](e))
          case \/-(ls) => \/-(Evaluate.ok[F, T[X]](ls.map(_.value)))
        }
    }
    intercept[R, ErrorOrOk, A, A]((a: A) => last.as(a), recurse)(action)
  }

  /**
   * evaluate 1 action possibly having error effects
   *
   * Execute a second action if the first one is not successful
   */
  def orElse[R, A](action: Eff[R, A], onError: Eff[R, A])(implicit m: ErrorOrOk <= R): Eff[R, A] =
    whenFailed(action, _ => onError)

  /**
   * evaluate 1 action possibly having error effects
   *
   * Execute a second action if the first one is not successful, based on the error
   */
  def catchError[R, A, B](action: Eff[R, A], pure: A => B, onError: Error => Eff[R, B])(implicit m: ErrorOrOk <= R): Eff[R, B] = {
    val recurse = new Recurse[ErrorOrOk, R, B] {
      def apply[X](current: ErrorOrOk[X]): X \/ Eff[R, B] =
        current.run match {
          case -\/(e) => \/-(onError(e))
          case \/-(x) =>
            try -\/[X](x.value)
            catch { case NonFatal(t) => \/-(onError(-\/(t))) }
        }

      def applicative[X, T[_]: Traverse](ms: T[ErrorOrOk[X]]): T[X] \/ ErrorOrOk[T[X]] =
        ms.map(_.run).sequence match {
          case -\/(e)   => \/-(Evaluate.error[F, T[X]](e))
          case \/-(ls) => \/-(Evaluate.ok[F, T[X]](ls.map(_.value)))
        }

    }
    intercept1[R, ErrorOrOk, A, B](pure)(recurse)(action)
  }

  /**
   * evaluate 1 action possibly having error effects
   *
   * Execute a second action if the first one is not successful, based on the error
   *
   * The final value type is the same as the original type
   */
  def whenFailed[R, A](action: Eff[R, A], onError: Error => Eff[R, A])(implicit m: ErrorOrOk <= R): Eff[R, A] =
    catchError(action, identity[A], onError)

  /**
   * ignore one possible exception that could be thrown
   */
  def ignoreException[R, E <: Throwable : ClassTag, A](action: Eff[R, A])(implicit m: ErrorOrOk <= R): Eff[R, Unit] =
    catchError[R, A, Unit](action, (a: A) => (), { error: Error =>
      error match {
        case -\/(t) if implicitly[ClassTag[E]].runtimeClass.isInstance(t) =>
          EffMonad[R].point(())
        case other => outer.error(other)
      }
    })

  /**
   * Lift a computation over a "small" error (for a subsystem) into
   * a computation over a "bigger" error (for the full application)
   */
  def localError[SR, BR, U, F1, F2, A](r: Eff[SR, A], getter: F1 => F2)
                                      (implicit sr: Member.Aux[Evaluate[F1, ?], SR, U],
                                       br: Member.Aux[Evaluate[F2, ?], BR, U]): Eff[BR, A] =
  transform[SR, BR, U, Evaluate[F1, ?], Evaluate[F2, ?], A](r,
    new ~>[Evaluate[F1, ?], Evaluate[F2, ?]] {
      def apply[X](r: Evaluate[F1, X]): Evaluate[F2, X] =
        Evaluate(r.run.leftMap(_.map(getter)))
    })

  /**
   * Translate an error effect to another one in the same stack
   * a computation over a "bigger" error (for the full application)
   */
  def runLocalError[R, U, F1, F2, A](r: Eff[R, A], getter: F1 => F2)
                                    (implicit sr: Member.Aux[Evaluate[F1, ?], R, U], br: Evaluate[F2, ?] |= U): Eff[U, A] =
  translate[R, U, Evaluate[F1, ?], A](r) { new Translate[Evaluate[F1, ?], U] {
    def apply[X](ex: Evaluate[F1, X]): Eff[U, X] =
      ex.run match {
        case -\/(-\/(t))  => send[Evaluate[F2, ?], U, X](Evaluate.exception[F2, X](t))
        case -\/(\/-(e1)) => send[Evaluate[F2, ?], U, X](Evaluate.fail[F2, X](getter(e1)))
        case \/-(x)       => send[Evaluate[F2, ?], U, X](Evaluate.eval[F2, X](x))
      }
  }}

}

/**
 * Simple instantiation of the ErrorEffect trait with String as a Failure type
 */
object ErrorEffect extends ErrorEffect[String] {

  def render(t: Throwable): String =
    s"Error[${t.getClass.getName}]" + (Option(t.getMessage) match {
      case None          => ""
      case Some(message) => s" $message"
    })

  def renderWithStack(t: Throwable): String =
    s"""============================================================
       |${render(t)}
       |------------------------------------------------------------
       |${traceWithIndent(t, "    ")}
       |============================================================
       |""".stripMargin

  def trace(t: Throwable): String =  {
    val out = new java.io.StringWriter
    t.printStackTrace(new java.io.PrintWriter(out))
    out.toString
  }

  def traceWithIndent(t: Throwable, indent: String): String =
    trace(t).lines.map(line => indent + line).mkString("\n")
}

case class Evaluate[F, A](run: (Throwable \/ F) \/ Name[A])

object Evaluate {
  def ok[F, A](a: =>A)               = Evaluate[F, A](\/-(Name(a)))
  def eval[F, A](a: Eval[A])         = Evaluate[F, A](\/-(a))
  def error[F, A](a: Throwable \/ F) = Evaluate[F, A](-\/(a))
  def fail[F, A](f: F)               = error[F, A](\/-(f))
  def exception[F, A](t: Throwable)  = error[F, A](-\/(t))
}

