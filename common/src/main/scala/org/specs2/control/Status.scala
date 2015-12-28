package org.specs2
package control

import scala.util.control.NonFatal
import scalaz.{\/, \&/, Equal, Monad, \/-, -\/}, \&/._, \/._
import scalaz.std.option._
import scalaz.std.AllInstances._
import scalaz.syntax.std.option._

/**
 * A data type for holding statuses. This is effectively just an
 * Either with a specialized left. This particular specialization
 * handles string/exception based failures and should be used
 * to wrap up unsafe apis (i.e. java code).
 *
 * This specialization exists for a number of reasons:
 *  - scala.
 *  - having a single type param helps inference in a non-trivial way
 *    (this is essential to it later being used in a monad transformer).
 *  - useful methods for manipulating error messages.
 *  - better pattern matching support.
 *  - and again, scala.
 *
 * Credits to @markhibberd
 */
sealed trait Status[+A] {
  @inline final def fold[X](ok: A => X,
                            error: These[String, Throwable] => X): X =
    this match {
      case Ok(a)    => ok(a)
      case Ko(e) => error(e)
    }

  @inline final def foldAll[X](ok: A => X,
                               fail: String => X,
                               exception: Throwable => X,
                               both: (String, Throwable) => X): X =
    fold(ok, {
      case This(m) => fail(m)
      case That(e) => exception(e)
      case Both(m, e) => both(m, e)
    })

  def map[B](f: A => B): Status[B] =
    flatMap(f andThen Status.ok[B])

  def flatMap[B](f: A => Status[B]): Status[B] =
    fold(f, Status.these[B])

  def mapError(f: These[String, Throwable] => These[String, Throwable]): Status[A] =
    fold(Status.ok, f andThen Status.these)

  def mapErrorMessage(f: Option[String] => String): Status[A] =
    foldAll(
      Status.ok,
      m => Status.fail(f(Some(m))),
      t => Status.ko(f(None), t),
      (m, t) => Status.ko(f(Some(m)), t)
    )

  def prependErrorMessage(annotation: String): Status[A] =
    mapErrorMessage({
      case None          => annotation
      case Some(current) => s"${annotation} - ${current}"
    })

  def isOk: Boolean =
    fold(_ => true, _ => false)

  def isError: Boolean =
    !isOk

  def toDisjunction: These[String, Throwable] \/ A =
    fold(\/-(_), -\/(_))

  def toOption: Option[A] =
    fold(_.some, _ => none[A])

  def toEither: Either[These[String, Throwable], A] =
    toDisjunction.toEither

  def toOptionError: Option[These[String, Throwable]] =
    fold(_ => none, _.some)

  def toOptionErrorMessage: Option[String] =
    fold(_ => none, e => Status.asString(e).some)

  def getOrElse[AA >: A](otherwise: => AA): AA =
    toOption.getOrElse(otherwise)

  def |||[AA >: A](otherwise: => Status[AA]): Status[AA] =
    if (isOk) this else otherwise
}

case class Ok[A](value: A) extends Status[A]
case class Ko[A](error: These[String, Throwable]) extends Status[A]

object Status {
  def safe[A](thunk: => A): Status[A] =
    try ok(thunk) catch { case NonFatal(t) => exception(t) }

  def option[A](thunk: => A): Status[Option[A]] =
    try ok(Option(thunk)) catch { case NonFatal(t) => exception(t) }

  def ok[A](a: A): Status[A] =
    Ok(a)

  def exception[A](t: Throwable): Status[A] =
    these(That(t))

  def fail[A](message: String): Status[A] =
    these(This(message))

  def ko[A](message: String, t: Throwable): Status[A] =
    these(Both(message, t))

  def these[A](error: These[String, Throwable]): Status[A] =
    Ko(error)

  def fromDisjunction[A](v: These[String, Throwable] \/ A): Status[A] =
    v.fold(these, ok)

  def asString(these: These[String, Throwable]) = these match {
    case (This(m)) => m
    case (That(t)) => Throwables.renderWithStack(t)
    case (Both(m, t)) => s"${m}, caused by:\n${Throwables.renderWithStack(t)}}"
  }

  def asException(these: These[String, Throwable]) = these match {
    case (This(m)) => new Exception(m)
    case (That(t)) => t
    case (Both(m, t)) => new Exception(m, t)
  }

  def prependThis(these: These[String, Throwable], prepend: String): These[String, Throwable] =
    these.fold(m      => This(prepend + " - " + m),
      t      => Both(prepend, t),
      (m, t) => Both(prepend + " - " + m, t))

  implicit def StatusMonad: Monad[Status] = new Monad[Status] {
    def point[A](v: => A) = ok(v)
    def bind[A, B](m: Status[A])(f: A => Status[B]) = m.flatMap(f)
  }

  implicit def StatusEqual[A: Equal]: Equal[Status[A]] = {
    implicit def ThrowableEqual = Equal.equalA[Throwable]
    implicitly[Equal[These[String, Throwable] \/ A]].contramap(_.toDisjunction)
  }
}

object Throwables {
  def render(t: Throwable): String =
    s"Error[${t.getClass.getName}]" + (Option(t.getMessage) match {
      case None          => ""
      case Some(message) => s" ${message}"
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
