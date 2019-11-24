package org.specs2.control
package producer

import Producer._
import Transducer._
import org.specs2.fp._, syntax._

trait Transducers {

  def id[F[_] : Monad : Safe, A, B]: Transducer[F, A, A] =
    (p: Producer[F, A]) => p

  def filter[F[_] : Monad : Safe, A, B](f: A => Boolean): Transducer[F, A, A] =
    (p: Producer[F, A]) => Producer.filter(p)(f)

  def receive[F[_] : Monad : Safe, A, B](f: A => Producer[F, B]): Transducer[F, A, B] =
    (p: Producer[F, A]) => p.flatMap(f)

  def transducer[F[_] : Monad : Safe, A, B](f: A => B): Transducer[F, A, B] =
    (p: Producer[F, A]) => p.map(f)

  def transducerEval[F[_] : Monad : Safe, A, B](f: A => F[B]): Transducer[F, A, B] =
    (p: Producer[F, A]) => p.mapEval(f)

  def producerState[F[_] : Monad : Safe, A, B, S](start: S, last: Option[S => Producer[F, B]] = None)(f: (A, S) => (Producer[F, B], S)): Transducer[F, A, B] =
    (producer: Producer[F, A]) => {
      def go(p: Producer[F, A], s: S): Producer[F, B] =
        Producer(p.run.flatMap {
          case Done() =>
            last.map(_(s).run).getOrElse(done[F, B].run)

          case One(a) =>
            val (b, news) = f(a, s)
            last match {
              case None => b.run
              case Some(l) => (b append l(news)).run
            }

          case More(as, next) =>
            val (bs, news) = as.drop(1).foldLeft(f(as.head, s)) { case ((pb, s1), a) =>
              val (pb1, s2) = f(a, s1)
              (pb append pb1, s2)
            }
            (bs append go(next, news)).run
        })
      go(producer, start)
    }

  def producerStateF[F[_] : Monad : Safe, A, B, S](start: S, last: Option[S => Producer[F, B]] = None)(f: (A, S) => F[(Producer[F, B], S)]): Transducer[F, A, B] =
    (producer: Producer[F, A]) => {
      def go(p: Producer[F, A], s: S): Producer[F, B] =
        Producer(p.run flatMap {
          case Done() =>
            last match {
              case Some(l) => l(s).run
              case None    => done[F, B].run
            }

          case One(a) =>
            f(a, s).flatMap { case (b, news) =>
              last match {
                case None => b.run
                case Some(l) => (b append l(news)).run
              }
            }

          case More(as, next) =>
            val res = as.drop(1).foldLeft(f(as.head, s)) { (res, a) =>
              res.flatMap { case (pb, s1) =>
                f(a, s1).map { case (pb1, s2) =>
                  (pb append pb1, s2)
                }
              }
            }
            Producer.eval(res.map { case (bs, news) => bs append go(next, news) }).flatten.run
        })

      go(producer, start)
    }

  def state[F[_] : Monad : Safe, A, B, S](start: S)(f: (A, S) => (B, S)): Transducer[F, A, B] = (producer: Producer[F, A]) => {
    def go(p: Producer[F, A], s: S): Producer[F, B] =
      Producer(p.run flatMap {
        case Done() => done[F, B].run
        case One(a) => one[F, B](f(a, s)._1).run
        case More(as, next) =>
          val (bs, news) = as.foldLeft((List[B](), s)) { case ((bs1, s1), a) =>
            val (b, s2) = f(a, s1)
            (bs1 :+ b, s2)
          }
          (emit[F, B](bs) append go(next, news)).run
      })
    go(producer, start)
  }

  def stateF[F[_] : Monad : Safe, A, B, S](start: S)(f: (A, S) => (F[B], S)): Transducer[F, A, F[B]] = (producer: Producer[F, A]) => {
    def go(p: Producer[F, A], s: S): Producer[F, F[B]] =
      Producer(p.run flatMap {
        case Done() => done[F, F[B]].run
        case One(a) => one[F, F[B]](f(a, s)._1).run
        case More(as, next) =>
          as match {
            case Nil => go(next, s).run
            case a :: rest =>
              val (b, s1) = f(a, s)
              (one[F, F[B]](b) append go(emit[F, A](rest) append next, s1)).run
          }
      })

    go(producer, start)
  }

  def receiveOr[F[_] : Monad : Safe, A, B](f: A => Producer[F, B])(or: =>Producer[F, B]): Transducer[F, A, B] =
    cata_[F, A, B](
      or,
      (a: A) => f(a),
      (as: List[A], next: Producer[F, A]) => as.headOption.map(f).getOrElse(or))

  def receiveOption[F[_] : Monad : Safe, A, B]: Transducer[F, A, Option[A]] =
    receiveOr[F, A, Option[A]]((a: A) => one[F, Option[A]](Option(a)))(one[F, Option[A]](None))

  def drop[F[_] : Monad : Safe, A](n: Int): Transducer[F, A, A] =
    cata_[F, A, A](
      done[F, A],
      (a: A) => if (n <= 0) one(a) else done,
      (as: List[A], next: Producer[F, A]) =>
        if (n < as.size) emit[F, A](as.drop(n)) append next
        else next |> drop(n - as.size))

  def dropRight[F[_] : Monad : Safe, A](n: Int): Transducer[F, A, A] =
    (producer: Producer[F, A]) => {
      def go(p: Producer[F, A], elements: Vector[A]): Producer[F, A] =
        Producer(peek(p).flatMap {
          case (Some(a), as) =>
            val es = elements :+ a
            if (es.size >= n) (emit[F, A](es.toList) append go(as, Vector.empty[A])).run
            else go(as, es).run

          case (None, _) =>
            if (elements.size <= n) done[F, A].run
            else emit[F, A](elements.toList).run

        })
      go(producer, Vector.empty[A])
    }

  def take[F[_] : Monad : Safe, A](n: Int): Transducer[F, A, A] =
    (producer: Producer[F, A]) => {
      def go(p: Producer[F, A], i: Int): Producer[F, A] =
        if (i <= 0) done
        else
          Producer(p.run flatMap {
            case Done() => done[F, A].run
            case One(a) => one[F, A](a).run
            case More(as, next) =>
              if (as.size <= i) (emit[F, A](as) append go(next, i - as.size)).run
              else              emit[F, A](as take i).run
          })

      go(producer, n)
    }

  def takeWhile[F[_] : Monad : Safe, A](f: A => Boolean): Transducer[F, A, A] =
    cata_[F, A, A](
      done[F,A],
      (a: A) => if (f(a)) one(a) else done,
      (as: List[A], next: Producer[F, A]) =>
        as.takeWhile(f) match {
          case Nil => done
          case some => emit[F, A](some) append next.takeWhile(f)
        })

  def first[F[_] : Monad : Safe, A]: Transducer[F, A, A] = (producer: Producer[F, A]) => {
    Producer(producer.run flatMap {
      case Done() => done[F, A].run
      case One(a) => one[F, A](a).run
      case More(as, next) => as.headOption.map(fr => one[F, A](fr)).getOrElse(done[F, A]).run
    })
  }

  def last[F[_] : Monad : Safe, A]: Transducer[F, A, A] = (producer: Producer[F, A]) => {
    def go(p: Producer[F, A], previous: Option[A]): Producer[F, A] =
      Producer(p.run flatMap {
        case Done() => previous.map(pr => one[F, A](pr)).getOrElse(done[F, A]).run
        case One(a) => one[F, A](a).run
        case More(as, next) => go(next, as.lastOption).run
      })

    go(producer, None)
  }

  def scan[F[_] : Monad : Safe, A, B](start: B)(f: (B, A) => B): Transducer[F, A, B] = (producer: Producer[F, A]) => {
    def go(p: Producer[F, A], previous: B): Producer[F, B] =
      Producer(p.run flatMap {
        case Done() => done[F, B].run
        case One(a) => one[F, B](f(previous, a)).run
        case More(as, next) =>
          val scanned = as.scanLeft(previous)(f).drop(1)
          (emit[F, B](scanned) append go(next, scanned.lastOption.getOrElse(previous))).run
      })

    one[F, B](start) append go(producer, start)
  }

  def scan1[F[_] : Monad : Safe, A](f: (A, A) => A): Transducer[F, A, A] = (producer: Producer[F, A]) =>
    producer.first.flatMap(a => producer.drop(1).scan(a)(f))

  def reduceSemigroup[F[_] : Monad : Safe, A : Semigroup]: Transducer[F, A, A] =
    reduce(Semigroup[A].append(_, _))

  def reduce[F[_] : Monad : Safe, A](f: (A, A) => A): Transducer[F, A, A] = (producer: Producer[F, A]) =>
    last[F, A].apply(scan1[F, A](f).apply(producer))

  def reduceMonoid[F[_] : Monad : Safe, A : Monoid]: Transducer[F, A, A] =
    reduceSemigroup[F, A]

  def reduceMap[F[_] : Monad : Safe, A, B : Monoid](f: A => B): Transducer[F, A, B] = (producer: Producer[F, A]) =>
    reduceMonoid[F, B].apply(transducer[F, A, B](f).apply(producer))

  def reduceMapEval[F[_] : Monad : Safe, A, B : Monoid](f: A => F[B]): Transducer[F, A, B] = (producer: Producer[F, A]) =>
    reduceMonoid[F, B].apply(transducerEval[F, A, B](f).apply(producer))

  def zipWithPrevious[F[_] : Monad : Safe, A]: Transducer[F, A, (Option[A], A)] =
    (producer: Producer[F, A]) =>
      one[F, Option[A]](None: Option[A]).append(producer.map(Option.apply)).zip(producer)

  def zipWithPreviousN[F[_] : Monad : Safe, A](n: Int): Transducer[F, A, (List[A], A)] =
    (producer: Producer[F, A]) => {
      def go(p: Producer[F, A], previous: Vector[A]): Producer[F, (List[A], A)] =
        Producer(peek(p) flatMap {
          case (Some(a), as) =>
            val ps = if (previous.size < n) previous else previous.drop(1)
            (one[F, (List[A], A)]((previous.take(n).toList, a)) append go(as, ps :+ a)).run
          case (None, _) =>
            done[F, (List[A], A)].run
        })

      go(producer, Vector.empty)
    }

  def zipWithNext[F[_] : Monad : Safe, A]: Transducer[F, A, (A, Option[A])] =
    (producer: Producer[F, A]) =>
      producer.zip(producer.drop(1).map(Option.apply).append(one(None: Option[A])))

  def zipWithNextN[F[_] : Monad : Safe, A](n: Int): Transducer[F, A, (A, List[A])] =
    (producer: Producer[F, A]) => {
      Producer[F, (A, List[A])](peekN(producer, n + 1).flatMap { case (next, as) =>
        if (next.isEmpty)
          done[F, (A, List[A])].run
        else {
          val rest = next.drop(1)
          (one[F, (A, List[A])]((next.head, rest)) append (emit[F, A](rest) append as).zipWithNextN(n)).run
        }
      })
    }

  def zipWithPreviousAndNext[F[_] : Monad : Safe, A]: Transducer[F, A, (Option[A], A, Option[A])] =
    zipWithPreviousAndNextN[F, A](n = 1).map { case (prev, a, next) => (prev.headOption, a, next.headOption) }

  def zipWithPreviousAndNextN[F[_] : Monad : Safe, A](n: Int): Transducer[F, A, (List[A], A, List[A])] =
    (p: Producer[F, A]) =>
      ((p |> zipWithPreviousN(n)) zip (p |> zipWithNextN(n))).map { case ((prev, a), (_, next)) => (prev, a, next) }

  def zipWithIndex[F[_] : Monad : Safe, A]: Transducer[F, A, (A, Int)] =
    zipWithState[F, A, Int](0)((_, n: Int) => n + 1)

  def zipWithState[F[_] : Monad : Safe, A, B](b: B)(f: (A, B) => B): Transducer[F, A, (A, B)] =
    (producer: Producer[F, A]) => {
      Producer[F, (A, B)] {
        producer.run flatMap {
          case Done() => done[F, (A, B)].run
          case One(a) => one[F, (A, B)]((a, b)).run

          case More(as, next) =>
            val (zipped, newState) =
              as match {
                case Nil => (Vector.empty, b)
                case a :: rest => rest.foldLeft((Vector((a, b)), f(a, b))) { case ((ls, s), cur) =>
                  (ls :+ ((cur, s)), f(cur, s))
                }
              }

            (emit[F, (A, B)](zipped.toList) append zipWithState[F, A, B](newState)(f).apply(next)).run
        }
      }
    }

  def intersperse[F[_] : Monad : Safe, A](in: A): Transducer[F, A, A] =
    (producer: Producer[F, A]) =>
      Producer[F, A](
        producer.run flatMap {
          case Done() => done[F, A].run
          case One(a) => one[F, A](a).run
          case More(Nil, next) => intersperse[F, A](in).apply(next).run
          case More(as, next) =>
            val interspersed = as.init.foldRight(as.lastOption.toList)(_ +: in +: _)

            (emit[F, A](interspersed) append
              Producer[F, A](next.run.flatMap {
                case Done() => done[F, A].run
                case _ =>      (one[F, A](in) append intersperse[F, A](in).apply(next)).run
              })).run
        })


  private def cata_[F[_] : Monad : Safe, A, B](onDone: Producer[F, B], onOne: A => Producer[F, B], onMore: (List[A], Producer[F, A]) => Producer[F, B]): Transducer[F, A, B] =
    (producer: Producer[F, A]) => cata(producer)(onDone, onOne, onMore)
}

object Transducers extends Transducers

object Transducer {

  type Transducer[F[_], A, B] = Producer[F, A] => Producer[F, B]

  implicit class TransducerOps[F[_], A, B](t: Transducer[F, A, B]) {
    def |>[C](next: Transducer[F, B, C]): Transducer[F, A, C] =
      andThen(next)

    def andThen[C](next: Transducer[F, B, C]): Transducer[F, A, C] =
      (p: Producer[F, A]) => next(t(p))

    def flatMap[C](f: B => Producer[F, C]): Transducer[F, A, C] =
      (p: Producer[F, A]) => t(p).flatMap(f)

    def map[C](f: B => C): Transducer[F, A, C] =
      (p: Producer[F, A]) => t(p).map(f)
  }

}
