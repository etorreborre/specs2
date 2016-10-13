package org.specs2.control
package producer

import eff.all._
import scalaz._
import Producer._

trait Transducers {

  def id[R :_safe, A, B]: Transducer[R, A, A] =
    (p: Producer[R, A]) => p

  def filter[R :_safe, A, B](f: A => Boolean): Transducer[R, A, A] =
    (p: Producer[R, A]) => Producer.filter(p)(f)

  def receive[R :_safe, A, B](f: A => Producer[R, B]): Transducer[R, A, B] =
    (p: Producer[R, A]) => p.flatMap(f)

  def transducer[R :_safe, A, B](f: A => B): Transducer[R, A, B] =
    (p: Producer[R, A]) => p.map(f)

  def producerState[R :_safe, A, B, S](start: S)(f: (A, S) => (Producer[R, B], S)): Transducer[R, A, B] = (producer: Producer[R, A]) => {
    def go(p: Producer[R, A], s: S): Producer[R, B] =
      Producer(p.run flatMap {
        case Done() => done.run
        case One(a) => f(a, s)._1.run
        case More(as, next) =>
          val (bs, news) = as.drop(1).foldLeft(f(as.head, s)) { case ((pb, s1), a) =>
            val (pb1, s2) = f(a, s1)
            (pb append pb1, s2)
          }
          (bs append go(next, news)).run
      })
    go(producer, start)
  }

  def state[R :_safe, A, B, S](start: S)(f: (A, S) => (B, S)): Transducer[R, A, B] = (producer: Producer[R, A]) => {
    def go(p: Producer[R, A], s: S): Producer[R, B] =
      Producer(p.run flatMap {
        case Done() => done.run
        case One(a) => one(f(a, s)._1).run
        case More(as, next) =>
          val (bs, news) = as.foldLeft((List[B](), s)) { case ((bs1, s1), a) =>
            val (b, s2) = f(a, s1)
            (bs1 :+ b, s2)
          }
          (emit(bs) append go(next, news)).run
      })
    go(producer, start)
  }

  def receiveOr[R :_safe, A, B](f: A => Producer[R, B])(or: =>Producer[R, B]): Transducer[R, A, B] =
    cata_[R, A, B](
      or,
      (a: A) => f(a),
      (as: List[A], next: Producer[R, A]) => as.headOption.map(f).getOrElse(or))

  def receiveOption[R :_safe, A, B]: Transducer[R, A, Option[A]] =
    receiveOr[R, A, Option[A]]((a: A) => one(Option(a)))(one(None))

  def drop[R :_safe, A](n: Int): Transducer[R, A, A] =
    cata_[R, A, A](
      done[R, A],
      (a: A) => if (n <= 0) one(a) else done,
      (as: List[A], next: Producer[R, A]) =>
        if (n < as.size) emit(as.drop(n)) append next
        else next |> drop(n - as.size))

  def dropRight[R :_safe, A](n: Int): Transducer[R, A, A] =
    (producer: Producer[R, A]) => {
      def go(p: Producer[R, A], elements: Vector[A]): Producer[R, A] =
        Producer(peek(p).flatMap {
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

  def take[R :_safe, A](n: Int): Transducer[R, A, A] =
    (producer: Producer[R, A]) => {
      def go(p: Producer[R, A], i: Int): Producer[R, A] =
        if (i <= 0) done
        else
          Producer(p.run flatMap {
            case Done() => done.run
            case One(a) => one(a).run
            case More(as, next) =>
              if (as.size <= i) (emit(as) append go(next, i - as.size)).run
              else              emit(as take i).run
          })

      go(producer, n)
    }

  def takeWhile[R :_safe, A](f: A => Boolean): Transducer[R, A, A] =
    cata_[R, A, A](
      done[R, A],
      (a: A) => if (f(a)) one(a) else done,
      (as: List[A], next: Producer[R, A]) =>
        as.takeWhile(f) match {
          case Nil => done
          case some => emit(some) append next.takeWhile(f)
        })

  def first[R :_safe, A]: Transducer[R, A, A] = (producer: Producer[R, A]) => {
    Producer(producer.run flatMap {
      case Done() => done.run
      case One(a) => one(a).run
      case More(as, next) => as.headOption.map(fr => one(fr)).getOrElse(done).run
    })
  }

  def last[R :_safe, A]: Transducer[R, A, A] = (producer: Producer[R, A]) => {
    def go(p: Producer[R, A], previous: Option[A]): Producer[R, A] =
      Producer(p.run flatMap {
        case Done() => previous.map(pr => one(pr)).getOrElse(done).run
        case One(a) => one(a).run
        case More(as, next) => go(next, as.lastOption).run
      })

    go(producer, None)
  }

  def scan[R :_safe, A, B](start: B)(f: (B, A) => B): Transducer[R, A, B] = (producer: Producer[R, A]) => {
    def go(p: Producer[R, A], previous: B): Producer[R, B] =
      Producer(p.run flatMap {
        case Done() => done.run
        case One(a) => one(f(previous, a)).run
        case More(as, next) =>
          val scanned = as.scanLeft(previous)(f).drop(1)
          (emit(scanned) append go(next, scanned.lastOption.getOrElse(previous))).run
      })

    one(start) append go(producer, start)
  }

  def scan1[R :_safe, A](f: (A, A) => A): Transducer[R, A, A] = (producer: Producer[R, A]) =>
    producer.first.flatMap(a => producer.drop(1).scan(a)(f))

  def reduceSemigroup[R :_safe, A : Semigroup]: Transducer[R, A, A] =
    reduce(Semigroup[A].append(_, _))

  def reduce[R :_safe, A](f: (A, A) => A): Transducer[R, A, A] = (producer: Producer[R, A]) =>
    last.apply(scan1(f).apply(producer))

  def reduceMonoid[R :_safe, A : Monoid]: Transducer[R, A, A] =
    reduceSemigroup[R, A]

  def reduceMap[R :_safe, A, B : Monoid](f: A => B): Transducer[R, A, B] = (producer: Producer[R, A]) =>
    reduceMonoid[R, B].apply(transducer(f).apply(producer))


  def zipWithPrevious[R :_safe, A]: Transducer[R, A, (Option[A], A)] =
    (producer: Producer[R, A]) => {
      def go(p: Producer[R, A], previous: Option[A]): Producer[R, (Option[A], A)] =
        Producer(peek(p) flatMap {
          case (Some(a), as) => (one((previous, a)) append go(as, Option(a))).run
          case (None, _)     => done.run
        })

      go(producer, None)
    }

  def zipWithNext[R :_safe, A]: Transducer[R, A, (A, Option[A])] =
    (producer: Producer[R, A]) => {
      def go(p: Producer[R, A], previous: A): Producer[R, (A, Option[A])] =
        Producer(peek(p) flatMap {
          case (Some(a), as) => (one((previous, Option(a))) append go(as, a)).run
          case (None, _)     => one[R, (A, Option[A])]((previous, None)).run
        })
      Producer(peek(producer) flatMap {
        case (Some(a), as) => go(as, a).run
        case (None, _) => done.run
      })
    }

  def zipWithPreviousAndNext[R :_safe, A]: Transducer[R, A, (Option[A], A, Option[A])] =
    (p: Producer[R, A]) =>
      ((p |> zipWithPrevious) zip (p |> zipWithNext)).map { case ((prev, a), (_, next)) => (prev, a, next) }


  def zipWithIndex[R :_safe, A]: Transducer[R, A, (A, Int)] =
    zipWithState[R, A, Int](0)((_, n: Int) => n + 1)

  def zipWithState[R :_safe, A, B](b: B)(f: (A, B) => B): Transducer[R, A, (A, B)] =
    (producer: Producer[R, A]) => {
      def go(p: Producer[R, A], state: B): Producer[R, (A, B)] =
        Producer[R, (A, B)] {
          producer.run flatMap {
            case Done() => done.run
            case One(a) => one((a, state)).run

            case More(as, next) =>
              val zipped: Vector[(A, B)] =
                as match {
                  case Nil => Vector.empty
                  case a :: rest => rest.foldLeft((Vector((a, state)), state)) { case ((ls, s), cur) =>
                    val newState = f(cur, s)
                    (ls :+ ((cur, newState)), newState)
                  }._1
                }

              (emit(zipped.toList) append zipWithState(b)(f).apply(next)).run

          }
        }
      go(producer, b)
    }

  def intersperse[R :_safe, A](in: A): Transducer[R, A, A] =
    (producer: Producer[R, A]) =>
      Producer[R, A](
        producer.run flatMap {
          case Done() => done.run
          case One(a) => one(a).run
          case More(Nil, next) => intersperse(in).apply(next).run
          case More(as, next) =>
            val interspersed = as.init.foldRight(as.lastOption.toList)(_ +: in +: _)

            (emit(interspersed) append
              Producer(next.run flatMap {
                case Done() => done[R, A].run
                case _ =>      (one[R, A](in) append intersperse(in).apply(next)).run
              })).run
        })


  private def cata_[R :_safe, A, B](onDone: Producer[R, B], onOne: A => Producer[R, B], onMore: (List[A], Producer[R, A]) => Producer[R, B]): Transducer[R, A, B] =
    (producer: Producer[R, A]) => cata(producer)(onDone, onOne, onMore)
}

object Transducers extends Transducers


