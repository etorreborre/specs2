package org.specs2.control

import org.specs2.fp._
import org.specs2.fp.syntax._
import eff.{ eff => _, _ }
import eff.all._
import org.specs2.control.origami.Fold

package object producer {

  type Transducer[R, A, B] = Producer[R, A] => Producer[R, B]

  object producers extends Producers

  object transducers extends Transducers

  implicit class ProducerOps[R :_Safe, A](p: Producer[R, A]) {
    def filter(f: A => Boolean): Producer[R, A] =
      Producer.filter(p)(f)

    def sliding(n: Int): Producer[R, List[A]] =
      Producer.sliding(n)(p)

    def chunk(n: Int): Producer[R, A] =
      Producer.chunk(n)(p)

    def >(p2: Producer[R, A]): Producer[R, A] =
      p append p2

    def |>[B](t: Transducer[R, A, B]): Producer[R, B] =
      pipe(t)

    def pipe[B](t: Transducer[R, A, B]): Producer[R, B] =
      Producer.pipe(p, t)

    def into[U](implicit intoPoly: IntoPoly[R, U], s: _Safe[U]): Producer[U, A] =
      Producer.into(p)

    def fold[B, S](start: Eff[R, S], f: (S, A) => Eff[R, S], end: S => Eff[R, B]): Eff[R, B] =
      Producer.fold(p)(start, f, end)

    def fold[S, B](f: Fold[Eff[R, ?], A, B]): Eff[R, B] =
      Producer.fold(p)(f.start, f.fold, f.end)

    def observe[S](start: Eff[R, S], f: (S, A) => S, end: S => Eff[R, Unit]): Producer[R, A] =
      Producer.observe(p)(start, f, end)

    def runLast: Eff[R, Option[A]] =
      Producer.runLast(p)

    def runList: Eff[R, List[A]] =
      Producer.runList(p)

    def repeat: Producer[R, A] =
      Producer.repeat(p)
  }

  implicit class ProducerListOps[R :_Safe, A](p: Producer[R, List[A]]) {
    def flattenList: Producer[R, A] =
      Producer.flattenList(p)
  }

  implicit class ProducerSeqOps[R :_Safe, A](p: Producer[R, Seq[A]]) {
    def flattenSeq: Producer[R, A] =
      Producer.flattenSeq(p)
  }

  implicit class ProducerFlattenOps[R :_Safe, A](p: Producer[R, Producer[R, A]]) {
    def flatten: Producer[R, A] =
      Producer.flatten(p)
  }

  implicit class ProducerEffOps[R :_Safe, A](p: Producer[R, Eff[R, A]]) {
    def sequence[F[_]](n: Int): Producer[R, A] =
      Producer.sequence[R, F, A](n)(p)
  }

  implicit class ProducerTransducerOps[R :_Safe, A](p: Producer[R, A]) {
    def receiveOr[B](f: A => Producer[R, B])(or: =>Producer[R, B]): Producer[R, B] =
      p |> transducers.receiveOr(f)(or)

    def receiveOption[B]: Producer[R, Option[A]] =
      p |> transducers.receiveOption

    def drop(n: Int): Producer[R, A] =
      p |> transducers.drop(n)

    def dropRight(n: Int): Producer[R, A] =
      p |> transducers.dropRight(n)

    def take(n: Int): Producer[R, A] =
      p |> transducers.take(n)

    def takeWhile(f: A => Boolean): Producer[R, A] =
      p |> transducers.takeWhile(f)

    def zipWithPrevious: Producer[R, (Option[A], A)] =
      p |> transducers.zipWithPrevious

    def zipWithPreviousN(n: Int): Producer[R, (List[A], A)] =
      p |> transducers.zipWithPreviousN(n)

    def zipWithNext: Producer[R, (A, Option[A])] =
      p |> transducers.zipWithNext

    def zipWithNextN(n: Int): Producer[R, (A, List[A])] =
      p |> transducers.zipWithNextN(n)

    def zipWithPreviousAndNext: Producer[R, (Option[A], A, Option[A])] =
      p |> transducers.zipWithPreviousAndNext

    def zipWithPreviousAndNextN(n: Int): Producer[R, (List[A], A, List[A])] =
      p |> transducers.zipWithPreviousAndNextN(n)

    def zipWithIndex: Producer[R, (A, Int)] =
      p |> transducers.zipWithIndex

    def intersperse(a: A): Producer[R, A] =
      p |> transducers.intersperse(a: A)

    def first: Producer[R, A] =
      p |> transducers.first

    def last: Producer[R, A] =
      p |> transducers.last

    def scan[B](start: B)(f: (B, A) => B): Producer[R, B] =
      p |> transducers.scan(start)(f)

    def scan1(f: (A, A) => A): Producer[R, A] =
      p |> transducers.scan1(f)

    def state[B, S](start: S)(f: (A, S) => (B, S)): Producer[R, B] =
      p |> transducers.state(start)(f)

    def producerState[B, S](start: S, last: Option[S => Producer[R, B]] = None)(f: (A, S) => (Producer[R, B], S)): Producer[R, B] =
      p |> transducers.producerState(start, last)(f)

    def reduce(f: (A, A) => A): Producer[R, A] =
      p |> transducers.reduce(f)

    def reduceSemigroup(implicit semi: Semigroup[A]): Producer[R, A] =
      p |> transducers.reduceSemigroup

    def reduceMonoid(implicit monoid: Monoid[A]): Producer[R, A] =
      p |> transducers.reduceMonoid

    def reduceMap[B : Monoid](f: A => B): Producer[R, B] =
      p |> transducers.reduceMap[R, A, B](f)
  }

  implicit class TransducerOps[R :_Safe, A, B](t: Transducer[R, A, B]) {
    def |>[C](next: Transducer[R, B, C]): Transducer[R, A, C] =
      andThen(next)

    def andThen[C](next: Transducer[R, B, C]): Transducer[R, A, C] =
      (p: Producer[R, A]) => next(t(p))

    def flatMap[C](f: B => Producer[R, C]): Transducer[R, A, C] =
      (p: Producer[R, A]) => t(p).flatMap(f)

    def map[C](f: B => C): Transducer[R, A, C] =
      (p: Producer[R, A]) => t(p).map(f)
  }

  implicit class ProducerResourcesOps[R :_Safe, A](p: Producer[R, A]) {
    def thenFinally(e: Eff[R, Unit]): Producer[R, A] =
      Producer[R, A](p.run flatMap {
        case Done() => safe.thenFinally(Producer.done[R, A].run, e)
        case One(a) => safe.thenFinally(Producer.one[R, A](a).run, e)
        case More(as, next) => protect(More(as, ProducerResourcesOps(next).thenFinally(e)))
      })

    def `finally`(e: Eff[R, Unit]): Producer[R, A] =
      p.thenFinally(e)

    def attempt: Producer[R, Throwable Either A] =
      Producer[R, Throwable Either A](SafeInterpretation.attempt(p.run) map {
        case Right(Done()) => Done()
        case Right(One(a)) => One(Right(a))
        case Right(More(as, next)) => More(as.map(Either.right), next.map(Either.right))

        case Left(t) => One(Either.left(t))
      })
  }

  def bracket[R :_Safe, A, B, C](open: Eff[R, A])(step: A => Producer[R, B])(close: A => Eff[R, C]): Producer[R, B] =
    Producer[R, B] {
      open flatMap { resource =>
        (step(resource) `finally` close(resource).map(_ => ())).run
      }
    }
}
