package org.specs2.control
package producer

import scalaz._
import eff._, all._
import org.specs2.control.eff.syntax.eff._
import Scalaz.{cata =>_,_}
import Producer._

trait Transducers {

  def id[R :_Safe, A, B]: Transducer[R, A, A] =
    (p: Producer[R, A]) => p

  def filter[R :_Safe, A, B](f: A => Boolean): Transducer[R, A, A] =
    (p: Producer[R, A]) => Producer.filter(p)(f)

  def receive[R :_Safe, A, B](f: A => Producer[R, B]): Transducer[R, A, B] =
    (p: Producer[R, A]) => p.flatMap(f)

  def transducer[R :_Safe, A, B](f: A => B): Transducer[R, A, B] =
    (p: Producer[R, A]) => p.map(f)

  def producerState[R :_Safe, A, B, S](start: S, last: Option[S => Producer[R, B]] = None)(f: (A, S) => (Producer[R, B], S)): Transducer[R, A, B] =
    (producer: Producer[R, A]) => {
      def go(p: Producer[R, A], s: S): Producer[R, B] =
        Producer(p.run flatMap {
          case Done() =>
            last.map(_(s).run).getOrElse(done.run)

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

  def producerStateEff[R :_Safe, A, B, S](start: S, last: Option[S => Producer[R, B]] = None)(f: (A, S) => Eff[R, (Producer[R, B], S)]): Transducer[R, A, B] =
    (producer: Producer[R, A]) => {
      def go(p: Producer[R, A], s: S): Producer[R, B] =
        Producer(p.run flatMap {
          case Done() =>
            last match {
              case Some(l) => l(s).run
              case None    => done.run
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
            producers.eval(res.map { case (bs, news) => bs append go(next, news) }).flatten.run
        })

      go(producer, start)
    }

  def state[R :_Safe, A, B, S](start: S)(f: (A, S) => (B, S)): Transducer[R, A, B] = (producer: Producer[R, A]) => {
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

  def stateEval[R :_Safe, A, B, S](start: S)(f: (A, S) => Eff[R, (B, S)]): Transducer[R, A, B] = (producer: Producer[R, A]) => {
    def go(p: Producer[R, A], s: S): Producer[R, B] =
      Producer(p.run flatMap {
        case Done() => done.run
        case One(a) => oneEff(f(a, s).map(_._1)).run
        case More(as, next) =>
          type U = Fx.prepend[State[S, ?], R]
          val traversed: Eff[U, List[B]] =
            as.traverse { a: A =>
              for {
                s1 <- StateEffect.get[U, S]
                bs <- f(a, s1).into[U]
                _  <- StateEffect.put[U, S](bs._2)
              } yield bs._1
            }

          val result: Eff[R, (List[B], S)] = StateEffect.runState(s)(traversed)
          result.flatMap { case (bs, news) =>
            (emit(bs) append go(next, news)).run
          }
      })

    go(producer, start)
  }

  def stateEff[R :_Safe, A, B, S](start: S)(f: (A, S) => (Eff[R, B], S)): Transducer[R, A, Eff[R, B]] = (producer: Producer[R, A]) => {
    def go(p: Producer[R, A], s: S): Producer[R, Eff[R, B]] =
      Producer(p.run flatMap {
        case Done() => done.run
        case One(a) => one(f(a, s)._1).run
        case More(as, next) =>
          as match {
            case Nil => go(next, s).run
            case a :: rest =>
              val (b, s1) = f(a, s)
              (one(b) append go(emit(rest) append next, s1)).run
          }
      })

    go(producer, start)
  }

  def receiveOr[R :_Safe, A, B](f: A => Producer[R, B])(or: =>Producer[R, B]): Transducer[R, A, B] =
    cata_[R, A, B](
      or,
      (a: A) => f(a),
      (as: List[A], next: Producer[R, A]) => as.headOption.map(f).getOrElse(or))

  def receiveOption[R :_Safe, A, B]: Transducer[R, A, Option[A]] =
    receiveOr[R, A, Option[A]]((a: A) => one(Option(a)))(one(None))

  def drop[R :_Safe, A](n: Int): Transducer[R, A, A] =
    cata_[R, A, A](
      done[R, A],
      (a: A) => if (n <= 0) one(a) else done,
      (as: List[A], next: Producer[R, A]) =>
        if (n < as.size) emit(as.drop(n)) append next
        else next |> drop(n - as.size))

  def dropRight[R :_Safe, A](n: Int): Transducer[R, A, A] =
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

  def take[R :_Safe, A](n: Int): Transducer[R, A, A] =
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

  def takeWhile[R :_Safe, A](f: A => Boolean): Transducer[R, A, A] =
    cata_[R, A, A](
      done[R, A],
      (a: A) => if (f(a)) one(a) else done,
      (as: List[A], next: Producer[R, A]) =>
        as.takeWhile(f) match {
          case Nil => done
          case some => emit(some) append next.takeWhile(f)
        })

  def first[R :_Safe, A]: Transducer[R, A, A] = (producer: Producer[R, A]) => {
    Producer(producer.run flatMap {
      case Done() => done.run
      case One(a) => one(a).run
      case More(as, next) => as.headOption.map(fr => one(fr)).getOrElse(done).run
    })
  }

  def last[R :_Safe, A]: Transducer[R, A, A] = (producer: Producer[R, A]) => {
    def go(p: Producer[R, A], previous: Option[A]): Producer[R, A] =
      Producer(p.run flatMap {
        case Done() => previous.map(pr => one(pr)).getOrElse(done).run
        case One(a) => one(a).run
        case More(as, next) => go(next, as.lastOption).run
      })

    go(producer, None)
  }

  def scan[R :_Safe, A, B](start: B)(f: (B, A) => B): Transducer[R, A, B] = (producer: Producer[R, A]) => {
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

  def scan1[R :_Safe, A](f: (A, A) => A): Transducer[R, A, A] = (producer: Producer[R, A]) =>
    producer.first.flatMap(a => producer.drop(1).scan(a)(f))

  def reduceSemigroup[R :_Safe, A : Semigroup]: Transducer[R, A, A] =
    reduce(Semigroup[A].append(_, _))

  def reduce[R :_Safe, A](f: (A, A) => A): Transducer[R, A, A] = (producer: Producer[R, A]) =>
    last.apply(scan1(f).apply(producer))

  def reduceMonoid[R :_Safe, A : Monoid]: Transducer[R, A, A] =
    reduceSemigroup[R, A]

  def reduceMap[R :_Safe, A, B : Monoid](f: A => B): Transducer[R, A, B] = (producer: Producer[R, A]) =>
    reduceMonoid[R, B].apply(transducer(f).apply(producer))

  def zipWithPrevious[R :_Safe, A]: Transducer[R, A, (Option[A], A)] =
    zipWithPreviousN(1).map { case (prev, a) => (prev.headOption, a) }

  def zipWithPreviousN[R :_Safe, A](n: Int): Transducer[R, A, (List[A], A)] =
    (producer: Producer[R, A]) => {
      def go(p: Producer[R, A], previous: Vector[A]): Producer[R, (List[A], A)] =
        Producer(peek(p) flatMap {
          case (Some(a), as) =>
            val ps = if (previous.size < n) previous else previous.drop(1)
            (one((previous.take(n).toList, a)) append go(as, ps :+ a)).run
          case (None, _) =>
            done.run
        })

      go(producer, Vector.empty)
    }

  def zipWithNext[R :_Safe, A]: Transducer[R, A, (A, Option[A])] =
    zipWithNextN(1).map { case (a, next) => (a, next.headOption) }

  def zipWithNextN[R :_Safe, A](n: Int): Transducer[R, A, (A, List[A])] =
    (producer: Producer[R, A]) => {
      Producer(peekN(producer, n + 1) flatMap { case (next, as) =>
        if (next.isEmpty)
          done.run
        else
          (one((next.head, next.drop(1))) append producer.drop(1).zipWithNextN(n)).run
      })
    }

  def zipWithPreviousAndNext[R :_Safe, A]: Transducer[R, A, (Option[A], A, Option[A])] =
    zipWithPreviousAndNextN(n = 1).map { case (prev, a, next) => (prev.headOption, a, next.headOption) }

  def zipWithPreviousAndNextN[R :_Safe, A](n: Int): Transducer[R, A, (List[A], A, List[A])] =
    (p: Producer[R, A]) =>
      ((p |> zipWithPreviousN(n)) zip (p |> zipWithNextN(n))).map { case ((prev, a), (_, next)) => (prev, a, next) }


  def zipWithIndex[R :_Safe, A]: Transducer[R, A, (A, Int)] =
    zipWithState[R, A, Int](0)((_, n: Int) => n + 1)

  def zipWithState[R :_Safe, A, B](b: B)(f: (A, B) => B): Transducer[R, A, (A, B)] =
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

  def intersperse[R :_Safe, A](in: A): Transducer[R, A, A] =
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


  private def cata_[R :_Safe, A, B](onDone: Producer[R, B], onOne: A => Producer[R, B], onMore: (List[A], Producer[R, A]) => Producer[R, B]): Transducer[R, A, B] =
    (producer: Producer[R, A]) => cata(producer)(onDone, onOne, onMore)
}

object Transducers extends Transducers


