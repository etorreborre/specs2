package org.specs2.control
package producer

import producer.Producer1._
import Transducer1._
import org.specs2.fp._, syntax._

trait Transducers1 {

  def id[A, B]: Transducer1[A, A] =
    (p: Producer1[A]) => p

  def filter[A, B](f: A => Boolean): Transducer1[A, A] =
    (p: Producer1[A]) => Producer1.filter(p)(f)

  def receive[A, B](f: A => Producer1[B]): Transducer1[A, B] =
    (p: Producer1[A]) => p.flatMap(f)

  def transducer[A, B](f: A => B): Transducer1[A, B] =
    (p: Producer1[A]) => p.map(f)

  def transducerEval[A, B](f: A => Action1[B]): Transducer1[A, B] =
    (p: Producer1[A]) => p.mapEval(f)

  def producerState[A, B, S](start: S, last: Option[S => Producer1[B]] = None)(f: (A, S) => (Producer1[B], S)): Transducer1[A, B] =
    (producer: Producer1[A]) => {
      def go(p: Producer1[A], s: S): Producer1[B] =
        Producer1(p.run.flatMap {
          case Done1() =>
            last.map(_(s).run).getOrElse(done.run)

          case One1(a) =>
            val (b, news) = f(a, s)
            last match {
              case None => b.run
              case Some(l) => (b append l(news)).run
            }

          case More1(as, next) =>
            val (bs, news) = as.drop(1).foldLeft(f(as.head, s)) { case ((pb, s1), a) =>
              val (pb1, s2) = f(a, s1)
              (pb append pb1, s2)
            }
            (bs append go(next, news)).run
        })
      go(producer, start)
    }

  def producerStateEff[A, B, S](start: S, last: Option[S => Producer1[B]] = None)(f: (A, S) => Action1[(Producer1[B], S)]): Transducer1[A, B] =
    (producer: Producer1[A]) => {
      def go(p: Producer1[A], s: S): Producer1[B] =
        Producer1(p.run flatMap {
          case Done1() =>
            last match {
              case Some(l) => l(s).run
              case None    => done.run
            }

          case One1(a) =>
            f(a, s).flatMap { case (b, news) =>
              last match {
                case None => b.run
                case Some(l) => (b append l(news)).run
              }
            }

          case More1(as, next) =>
            val res = as.drop(1).foldLeft(f(as.head, s)) { (res, a) =>
              res.flatMap { case (pb, s1) =>
                f(a, s1).map { case (pb1, s2) =>
                  (pb append pb1, s2)
                }
              }
            }
            producers1.eval(res.map { case (bs, news) => bs append go(next, news) }).flatten.run
        })

      go(producer, start)
    }

  def state[A, B, S](start: S)(f: (A, S) => (B, S)): Transducer1[A, B] = (producer: Producer1[A]) => {
    def go(p: Producer1[A], s: S): Producer1[B] =
      Producer1(p.run flatMap {
        case Done1() => done.run
        case One1(a) => one(f(a, s)._1).run
        case More1(as, next) =>
          val (bs, news) = as.foldLeft((List[B](), s)) { case ((bs1, s1), a) =>
            val (b, s2) = f(a, s1)
            (bs1 :+ b, s2)
          }
          (emit(bs) append go(next, news)).run
      })
    go(producer, start)
  }

  def stateEff[A, B, S](start: S)(f: (A, S) => (Action1[B], S)): Transducer1[A, Action1[B]] = (producer: Producer1[A]) => {
    def go(p: Producer1[A], s: S): Producer1[Action1[B]] =
      Producer1(p.run flatMap {
        case Done1() => done.run
        case One1(a) => one(f(a, s)._1).run
        case More1(as, next) =>
          as match {
            case Nil => go(next, s).run
            case a :: rest =>
              val (b, s1) = f(a, s)
              (one(b) append go(emit(rest) append next, s1)).run
          }
      })

    go(producer, start)
  }

  def receiveOr[A, B](f: A => Producer1[B])(or: =>Producer1[B]): Transducer1[A, B] =
    cata_[A, B](
      or,
      (a: A) => f(a),
      (as: List[A], next: Producer1[A]) => as.headOption.map(f).getOrElse(or))

  def receiveOption[A, B]: Transducer1[A, Option[A]] =
    receiveOr[A, Option[A]]((a: A) => one(Option(a)))(one(None))

  def drop[A](n: Int): Transducer1[A, A] =
    cata_[A, A](
      done[A],
      (a: A) => if (n <= 0) one(a) else done,
      (as: List[A], next: Producer1[A]) =>
        if (n < as.size) emit(as.drop(n)) append next
        else next |> drop(n - as.size))

  def dropRight[A](n: Int): Transducer1[A, A] =
    (producer: Producer1[A]) => {
      def go(p: Producer1[A], elements: Vector[A]): Producer1[A] =
        Producer1(peek(p).flatMap {
          case (Some(a), as) =>
            val es = elements :+ a
            if (es.size >= n) (emit(es.toList) append go(as, Vector.empty[A])).run
            else go(as, es).run

          case (None, _) =>
            if (elements.size <= n) done.run
            else emit(elements.toList).run

        })
      go(producer, Vector.empty[A])
    }

  def take[A](n: Int): Transducer1[A, A] =
    (producer: Producer1[A]) => {
      def go(p: Producer1[A], i: Int): Producer1[A] =
        if (i <= 0) done
        else
          Producer1(p.run flatMap {
            case Done1() => done.run
            case One1(a) => one(a).run
            case More1(as, next) =>
              if (as.size <= i) (emit(as) append go(next, i - as.size)).run
              else              emit(as take i).run
          })

      go(producer, n)
    }

  def takeWhile[A](f: A => Boolean): Transducer1[A, A] =
    cata_[A, A](
      done[A],
      (a: A) => if (f(a)) one(a) else done,
      (as: List[A], next: Producer1[A]) =>
        as.takeWhile(f) match {
          case Nil => done
          case some => emit(some) append next.takeWhile(f)
        })

  def first[A]: Transducer1[A, A] = (producer: Producer1[A]) => {
    Producer1(producer.run flatMap {
      case Done1() => done.run
      case One1(a) => one(a).run
      case More1(as, next) => as.headOption.map(fr => one(fr)).getOrElse(done).run
    })
  }

  def last[A]: Transducer1[A, A] = (producer: Producer1[A]) => {
    def go(p: Producer1[A], previous: Option[A]): Producer1[A] =
      Producer1(p.run flatMap {
        case Done1() => previous.map(pr => one(pr)).getOrElse(done).run
        case One1(a) => one(a).run
        case More1(as, next) => go(next, as.lastOption).run
      })

    go(producer, None)
  }

  def scan[A, B](start: B)(f: (B, A) => B): Transducer1[A, B] = (producer: Producer1[A]) => {
    def go(p: Producer1[A], previous: B): Producer1[B] =
      Producer1(p.run flatMap {
        case Done1() => done.run
        case One1(a) => one(f(previous, a)).run
        case More1(as, next) =>
          val scanned = as.scanLeft(previous)(f).drop(1)
          (emit(scanned) append go(next, scanned.lastOption.getOrElse(previous))).run
      })

    one(start) append go(producer, start)
  }

  def scan1[A](f: (A, A) => A): Transducer1[A, A] = (producer: Producer1[A]) =>
    producer.first.flatMap(a => producer.drop(1).scan(a)(f))

  def reduceSemigroup[A : Semigroup]: Transducer1[A, A] =
    reduce(Semigroup[A].append(_, _))

  def reduce[A](f: (A, A) => A): Transducer1[A, A] = (producer: Producer1[A]) =>
    last.apply(scan1(f).apply(producer))

  def reduceMonoid[A : Monoid]: Transducer1[A, A] =
    reduceSemigroup[A]

  def reduceMap[A, B : Monoid](f: A => B): Transducer1[A, B] = (producer: Producer1[A]) =>
    reduceMonoid[B].apply(transducer(f).apply(producer))

  def reduceMapEval[A, B : Monoid](f: A => Action1[B]): Transducer1[A, B] = (producer: Producer1[A]) =>
    reduceMonoid[B].apply(transducerEval(f).apply(producer))

  def zipWithPrevious[A]: Transducer1[A, (Option[A], A)] =
    (producer: Producer1[A]) =>
      one(None: Option[A]).append(producer.map(Option.apply)).zip(producer)

  def zipWithPreviousN[A](n: Int): Transducer1[A, (List[A], A)] =
    (producer: Producer1[A]) => {
      def go(p: Producer1[A], previous: Vector[A]): Producer1[(List[A], A)] =
        Producer1(peek(p) flatMap {
          case (Some(a), as) =>
            val ps = if (previous.size < n) previous else previous.drop(1)
            (one((previous.take(n).toList, a)) append go(as, ps :+ a)).run
          case (None, _) =>
            done.run
        })

      go(producer, Vector.empty)
    }

  def zipWithNext[A]: Transducer1[A, (A, Option[A])] =
    (producer: Producer1[A]) =>
      producer.zip(producer.drop(1).map(Option.apply).append(one(None: Option[A])))

  def zipWithNextN[A](n: Int): Transducer1[A, (A, List[A])] =
    (producer: Producer1[A]) => {
      Producer1(peekN(producer, n + 1).flatMap { case (next, as) =>
        if (next.isEmpty)
          done.run
        else {
          val rest = next.drop(1)
          (one((next.head, rest)) append (emit(rest) append as).zipWithNextN(n)).run
        }
      })
    }

  def zipWithPreviousAndNext[A]: Transducer1[A, (Option[A], A, Option[A])] =
    zipWithPreviousAndNextN(n = 1).map { case (prev, a, next) => (prev.headOption, a, next.headOption) }

  def zipWithPreviousAndNextN[A](n: Int): Transducer1[A, (List[A], A, List[A])] =
    (p: Producer1[A]) =>
      ((p |> zipWithPreviousN(n)) zip (p |> zipWithNextN(n))).map { case ((prev, a), (_, next)) => (prev, a, next) }

  def zipWithIndex[A]: Transducer1[A, (A, Int)] =
    zipWithState[A, Int](0)((_, n: Int) => n + 1)

  def zipWithState[A, B](b: B)(f: (A, B) => B): Transducer1[A, (A, B)] =
    (producer: Producer1[A]) => {
      Producer1[(A, B)] {
        producer.run flatMap {
          case Done1() => done.run
          case One1(a) => one((a, b)).run

          case More1(as, next) =>
            val (zipped, newState) =
              as match {
                case Nil => (Vector.empty, b)
                case a :: rest => rest.foldLeft((Vector((a, b)), f(a, b))) { case ((ls, s), cur) =>
                  (ls :+ ((cur, s)), f(cur, s))
                }
              }

            (emit(zipped.toList) append zipWithState(newState)(f).apply(next)).run
        }
      }
    }

  def intersperse[A](in: A): Transducer1[A, A] =
    (producer: Producer1[A]) =>
      Producer1[A](
        producer.run flatMap {
          case Done1() => done.run
          case One1(a) => one(a).run
          case More1(Nil, next) => intersperse(in).apply(next).run
          case More1(as, next) =>
            val interspersed = as.init.foldRight(as.lastOption.toList)(_ +: in +: _)

            (emit(interspersed) append
              Producer1(next.run.flatMap {
                case Done1() => done[A].run
                case _ =>      (one[A](in) append intersperse(in).apply(next)).run
              })).run
        })


  private def cata_[A, B](onDone: Producer1[B], onOne: A => Producer1[B], onMore: (List[A], Producer1[A]) => Producer1[B]): Transducer1[A, B] =
    (producer: Producer1[A]) => cata(producer)(onDone, onOne, onMore)
}

object Transducer1 {
  type Transducer1[A, B] = Producer1[A] => Producer1[B]

  implicit class Transducer1Ops[A, B](t: Transducer1[A, B]) {
    def |>[C](next: Transducer1[B, C]): Transducer1[A, C] =
      andThen(next)

    def andThen[C](next: Transducer1[B, C]): Transducer1[A, C] =
      (p: Producer1[A]) => next(t(p))

    def flatMap[C](f: B => Producer1[C]): Transducer1[A, C] =
      (p: Producer1[ A]) => t(p).flatMap(f)

    def map[C](f: B => C): Transducer1[A, C] =
      (p: Producer1[ A]) => t(p).map(f)
  }

}
