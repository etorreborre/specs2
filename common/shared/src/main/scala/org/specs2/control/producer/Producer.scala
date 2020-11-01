package org.specs2.control
package producer

import org.specs2._
import fp._, syntax._
import origami._
import Producer._

/**
 * ADT for streaming data with a Producer. It is either:
 *
 *  - Done: nothing left to stream
 *  - One: there is just one element
 *  - More(as, next): there are `as` elements and the next producer
 */
sealed trait LazyList[F[_], A]
case class Done[F[_], A]() extends LazyList[F, A]
case class One[F[_], A](a: A) extends LazyList[F, A]
case class More[F[_], A](as: List[A], next: Producer[F, A]) extends LazyList[F, A]

/**
 * Simple streaming data structure for elements of type A and effects F
 */
case class Producer[F[_] : Monad : Safe, A](run: F[LazyList[F, A]]):

  /**
   * Catamorphism on the LazyList data type to produce another stream
   */
  private[producer]
  def cata[B](onDone: Producer[F, B], onOne: A => Producer[F, B], onMore: (List[A], Producer[F, A]) => Producer[F, B]): Producer[F, B] =
    Producer[F, B](run.flatMap {
      case Done() => onDone.run
      case One(a) => onOne(a).run
      case More(as, next) => onMore(as, next).run
    })

  /**
   * BASIC OPERATIONS (LIST-LIKE)
   */

  /** flatMap a Producer similar to how flatMap works on lists */
  def flatMap[B](f: A => Producer[F, B]): Producer[F, B] =
    cata(
      done[F, B],
      (a: A) => f(a),
      (as: List[A], next: Producer[F, A]) => as.map(f).suml append next.flatMap(f))

  def map[B](f: A => B): Producer[F, B] =
    flatMap(a => one(f(a)))

  def mapEval[B](f: A => F[B]): Producer[F, B] =
    flatMap(a => Producer.eval(f(a)))

  def collect[B](pf: PartialFunction[A, B]): Producer[F, B] =
    flatMap { a =>
      if pf.isDefinedAt(a) then one(pf(a))
      else done
    }

  def append(other: Producer[F, A]): Producer[F, A] =
    Producer(run.flatMap {
      case Done()         => summon[Monad[F]].pure(other.run).flatten
      case One(a1)        => summon[Monad[F]].pure(More(List(a1), other))
      case More(as, next) => summon[Monad[F]].pure(More(as, next append other))
    })

  def filter(f: A => Boolean): Producer[F, A] =
    Producer(run flatMap {
      case Done() => done[F, A].run
      case One(a) => summon[Monad[F]].pure(a).as(if f(a) then One(a) else Done())
      case More(as, next) =>
        as filter f match {
          case Nil => next.filter(f).run
          case a :: rest => (oneOrMore[F, A](a, rest) append next.filter(f)).run
        }
    })

  def drop(n: Int): Producer[F, A] =
    cata(
      done[F, A],
      (a: A) => if n <= 0 then one(a) else done,
      (as: List[A], next: Producer[F, A]) =>
        if n < as.size then emit[F, A](as.drop(n)) append next
        else next.drop(n - as.size))

  def dropRight(n: Int): Producer[F, A] =
      def go(p: Producer[F, A], elements: Vector[A]): Producer[F, A] =
        Producer(p.peek.flatMap {
          case (Some(a), as) =>
            val es = elements :+ a
            if es.size >= n then (emit[F, A](es.toList) append go(as, Vector.empty[A])).run
            else go(as, es).run

          case (None, _) =>
            if elements.size <= n then done[F, A].run
            else emit[F, A](elements.toList).run

        })
      go(this, Vector.empty[A])

  def take(n: Int): Producer[F, A] =
    def go(p: Producer[F, A], i: Int): Producer[F, A] =
      if i <= 0 then done
      else
        Producer(p.run flatMap {
          case Done() => done[F, A].run
          case One(a) => one[F, A](a).run
          case More(as, next) =>
            if as.size <= i then (emit[F, A](as) append go(next, i - as.size)).run
            else              emit[F, A](as take i).run
        })
    go(this, n)

  def takeWhile(f: A => Boolean): Producer[F, A] =
    cata(
      done[F,A],
      (a: A) => if f(a) then one(a) else done,
      (as: List[A], next: Producer[F, A]) =>
        as.takeWhile(f) match {
          case Nil => done
          case some => emit[F, A](some) append next.takeWhile(f)
        })

  def first: Producer[F, A] =
    Producer(run flatMap {
      case Done() => done[F, A].run
      case One(a) => one[F, A](a).run
      case More(as, next) => as.headOption.map(fr => one[F, A](fr)).getOrElse(done[F, A]).run
    })

  def last: Producer[F, A] =
    def go(p: Producer[F, A], previous: Option[A]): Producer[F, A] =
      Producer(p.run flatMap {
        case Done() => previous.map(pr => one[F, A](pr)).getOrElse(done[F, A]).run
        case One(a) => one[F, A](a).run
        case More(as, next) => go(next, as.lastOption).run
      })

    go(this, None)

  /**
   * TRANSDUCER OPERATIONS
   *
   * When it is easier to compose stream transformations
   */

  def pipe[B](t: Transducer[F, A, B]): Producer[F, B] =
    t(this)

  def >(p2: Producer[F, A]): Producer[F, A] =
    this append p2

  def |>[B](t: Transducer[F, A, B]): Producer[F, B] =
    pipe(t)

  /**
   * SAFE OPERATIONS
   */

  /** run the stream and add a finalizer to run finally */
  def andFinally(finalizer: Finalizer): Producer[F, A] =
    Producer(Safe[F].finalizeWith(run, finalizer))

  /** run the stream and add a finalizer to run finally */
  def `finally`(e: Finalizer): Producer[F, A] =
    andFinally(e)

  /** attempt to run each effect of the stream */
  def attempt: Producer[F, Throwable Either A] =
    Producer[F, Throwable Either A](Safe[F].attempt(run) map {
      case Right(Done()) => Done()
      case Right(One(a)) => One(Right(a))
      case Right(More(as, next)) => More(as.map(Right.apply), next.map(Right.apply))

      case Left(t) => One(Left(t))
    })

  /**
   * CONSUME STREAMS
   */

  /** run a stream with a Fold */
  def fold[B, S](start: F[S], f: (S, A) => F[S], end: S => F[B]): F[B] =
    run flatMap {
      case Done() => start.flatMap(end)
      case One(a) => start.flatMap(s1 => f(s1, a).flatMap(end))
      case More(as, next) =>
        start.flatMap { s1 =>
          as.foldLeftM(s1)(f).flatMap { s =>
            next.fold(summon[Monad[F]].pure(s), f, end)
          }
        }
    }

  /** run a stream with a Fold */
  def fold[B](aFold: Fold[F, A, B]): F[B] =
    fold(aFold.start, aFold.fold, aFold.end)

  /** get all the elements as a List */
  def runList: F[List[A]] =
    fold(
      summon[Monad[F]].pure(Vector[A]()),
      (vs: Vector[A], a: A) => summon[Monad[F]].pure(vs :+ a),
      (vs:Vector[A]) => summon[Monad[F]].pure(vs.toList))

  /** get the last element  */
  def runLast: F[Option[A]] =
    run flatMap {
      case One(a) => summon[Monad[F]].pure[Option[A]](Option(a))
      case Done() => summon[Monad[F]].pure[Option[A]](None)
      case More(as, next) => next.runLast.map(_.orElse(as.lastOption))
    }

  /**
   * REPEAT ELEMENTS
   */
  def fill(n: Int): Producer[F, A] =
    if n <= 0 then done[F, A]
    else append(fill(n - 1))

  def repeat: Producer[F, A] =
    Producer(run flatMap {
      case Done() => summon[Monad[F]].pure(Done())
      case One(a) => summon[Monad[F]].pure(More(List(a), repeat))
      case More(as, next) => summon[Monad[F]].pure(More(as, next append repeat))
    })

  /**
   * SPECIALIZED OPERATIONS BASED ON THE STREAM ELEMENTS
   */
  def flattenList[B](using ev: A <:< List[B]): Producer[F, B] =
    flatMap(a => emit[F, B](ev(a)))

  def flattenSeq[B](using ev: A <:< Seq[B]): Producer[F, B] =
    flatMap(a => emitSeq[F, B](ev(a)))

  def flatten[B](using ev: A <:< Producer[F, B]): Producer[F, B] =
    Producer(run flatMap {
      case Done() => done[F, B].run
      case One(p) => ev(p).run
      case More(ps, next) => (flattenProducers(ps.map(ev)) append next.flatten).run
    })

  def sequence[B](n: Int)(using ev: A <:< F[B]): Producer[F, B] =
    sliding(n).flatMap { actions =>
      Producer.emitAction(summon[Traverse[List]].sequence(actions.map(ev)))
    }

  /**
   * QUERY STREAMS
   */

  /** accumulate chunks of size n inside More nodes */
  def chunk(size: Int): Producer[F, A] =
    def go(p: Producer[F, A], elements: Vector[A]): Producer[F, A] =
      Producer[F, A](
        p.run flatMap {
          case Done() => emit[F, A](elements.toList).run
          case One(a) => emit[F, A]((elements :+ a).toList).run

          case More(as, next) =>
            val es = elements ++ as
            if es.size == size then (emit[F, A](es.toList) append go(next, Vector.empty)).run
            else                 go(next, es).run
        })

    go(this, Vector.empty)

  /** produce non overlapping windows of 'size' elements */
  def sliding(size: Int): Producer[F, List[A]] =

    def go(p: Producer[F, A], elements: Vector[A]): Producer[F, List[A]] =
      Producer[F, List[A]](
        peek.flatMap {
          case (Some(a), as) =>
            val es = elements :+ a
            if es.size == size then (one[F, List[A]](es.toList) append go(as, Vector.empty)).run
            else                 go(as, es).run

          case (None, _) =>
            one[F, List[A]](elements.toList).run
        })

    go(this, Vector.empty)

  /** produce the next element if there is one + the remaining of the stream */
  def peek: F[(Option[A], Producer[F, A])] =
    run map {
      case Done() => (None, done[F, A])
      case One(a) => (Option(a), done[F, A])
      case More(as, next) => (as.headOption, emit[F, A](as.tail) append next)
    }

  /** produce the first n elements + the remaining of the stream */
  def peekN(n: Int): F[(List[A], Producer[F, A])] =
    def go(p: Producer[F, A], collected: Vector[A]): F[(List[A], Producer[F, A])] =
      p.run flatMap {
        case Done() => summon[Monad[F]].pure((collected.toList, done[F, A]))
        case One(a) => summon[Monad[F]].pure(((collected :+ a).take(n).toList, done[F, A]))
        case More(as, next) =>
          val all = collected ++ as
          if all.size >= n then
          summon[Monad[F]].pure((all.take(n).toList, emit[F, A](all.drop(n).toList) append next))
          else
            go(next, all)
      }

    go(this, Vector.empty)

  /** run some state changes on each element and run an action on the last state */
  def observe[S](start: F[S], f: (S, A) => S, onLastState: S => F[Unit]): Producer[F, A] =
    Producer[F, A](start flatMap { init =>
      def go(p: Producer[F, A], s: S): Producer[F, A] =
        Producer[F, A] {
          p.run flatMap {
            case Done() => onLastState(s) >> done[F, A].run
            case One(a) => onLastState(s) >> one[F, A](a).run
            case More(as, next) =>
              val newS = as.foldLeft(s)(f)
              (emit[F, A](as) append go(next, newS)).run
          }
        }

      go(this, init).run

    })

  def receiveOr[B](f: A => Producer[F, B])(or: =>Producer[F, B]): Producer[F, B] =
    cata(
      or,
      (a: A) => f(a),
      (as: List[A], next: Producer[F, A]) => as.headOption.map(f).getOrElse(or))

  def receiveOption[B]: Producer[F, Option[A]] =
    receiveOr[Option[A]]((a: A) => one[F, Option[A]](Option(a)))(one[F, Option[A]](None))

  /**
   * ZIP OPERATIONS
   */

  /** zip 2 streams together */
  def zip[B](other: Producer[F, B]): Producer[F, (A, B)] =
    Producer(run flatMap {
      case Done() => done[F, (A, B)].run
      case One(a) =>
        other.run flatMap {
          case Done() => done[F, (A, B)].run
          case One(b) => one[F, (A, B)]((a, b)).run

          case More(bs, next) =>
            (bs.headOption.map(b => one[F, (A, B)]((a, b))).getOrElse(done[F, (A, B)]) append (drop(1) zip other.drop(1))).run
        }

      case More(Nil, next) => next.zip(other).run

      case More(as, nexta) =>
        other.run flatMap {
          case Done() => done[F, (A, B)].run
          case One(b) => as.headOption.map(a => one[F, (A, B)]((a, b))).getOrElse(done[F, (A, B)]).run

          case More(bs, nextb) =>
            if as.size == bs.size then
              (emit[F, (A, B)](as zip bs) append (nexta zip nextb)).run
            else if as.size < bs.size then
              (emit[F, (A, B)](as zip bs) append (nexta zip (emit[F, B](bs.drop(as.size)) append nextb))).run
            else
              (emit[F, (A, B)](as zip bs) append ((emit[F, A](as.drop(bs.size)) append nexta) zip nextb)).run
        }
    })

  /** zip each element with the previous one in the stream (which will not exist for the first element) */
  def zipWithPrevious: Producer[F, (Option[A], A)] =
    one[F, Option[A]](None: Option[A]).append(map(Option.apply)).zip(this)

  /**
   * zip each element with the previous n elements in the stream
   * which will not exist for the first element, but we take as
   * many elements to fill the "previous" list as we can
   */
  def zipWithPreviousN(n: Int): Producer[F, (List[A], A)] =
      def go(p: Producer[F, A], previous: Vector[A]): Producer[F, (List[A], A)] =
        Producer(p.peek flatMap {
          case (Some(a), as) =>
            val ps = if previous.size < n then previous else previous.drop(1)
            (one[F, (List[A], A)]((previous.take(n).toList, a)) append go(as, ps :+ a)).run
          case (None, _) =>
            done[F, (List[A], A)].run
        })

      go(this, Vector.empty)

  /** zip each element with the next one in the stream (which will not exist for the last element) */
  def zipWithNext: Producer[F, (A, Option[A])] =
    zip(drop(1).map(Option.apply).append(one(None: Option[A])))

  /**
   * zip each element with the next n elements in the stream
   * which will not exist for the last element, but we take as
   * many elements to fill the "next" list as we can
   */
  def zipWithNextN(n: Int): Producer[F, (A, List[A])] =
      Producer[F, (A, List[A])](peekN(n + 1).flatMap { case (next, as) =>
        if next.isEmpty then
          done[F, (A, List[A])].run
        else {
          val rest = next.drop(1)
          (one[F, (A, List[A])]((next.head, rest)) append (emit[F, A](rest) append as).zipWithNextN(n)).run
        }
      })

  /** zip each element with the previous and next one in the stream (which will not exist for the first and last element */
  def zipWithPreviousAndNext: Producer[F, (Option[A], A, Option[A])] =
    zipWithPreviousAndNextN(n = 1).map { case (prev, a, next) => (prev.headOption, a, next.headOption) }

  /** zip each element with the previous and next N elements in the stream */
  def zipWithPreviousAndNextN(n: Int): Producer[F, (List[A], A, List[A])] =
    zipWithPreviousN(n).zip(zipWithNextN(n)).map { case ((prev, a), (_, next)) => (prev, a, next) }

  /** zip each element with its index in the stream */
  def zipWithIndex: Producer[F,(A, Int)] =
    zipWithState[Int](0)((_, n: Int) => n + 1)

  /** zip each element with a running state computed from a function */
  def zipWithState[B](b: B)(f: (A, B) => B): Producer[F, (A, B)] =
    Producer[F, (A, B)] {
      run flatMap {
        case Done() => done[F, (A, B)].run
        case One(a) => one[F, (A, B)]((a, b)).run
        case More(as, next) =>
          val (zipped, newState) =
            as match
              case Nil => (Vector.empty, b)
              case a :: rest => rest.foldLeft((Vector((a, b)), f(a, b))) { case ((ls, s), cur) =>
                (ls :+ ((cur, s)), f(cur, s))
              }
          emit[F, (A, B)](zipped.toList).append(next.zipWithState(newState)(f)).run
      }
    }

  /** insert an element in between every element of the stream */
  def intersperse(in: A): Producer[F, A] =
    Producer[F, A](
      run flatMap {
        case Done() => done[F, A].run
        case One(a) => one[F, A](a).run
        case More(Nil, next) => next.intersperse(in).run
        case More(as, next) =>
          val interspersed = as.init.foldRight(as.lastOption.toList)(_ +: in +: _)

          (emit[F, A](interspersed) append
            Producer[F, A](next.run.flatMap {
              case Done() => done[F, A].run
              case _ =>      (one[F, A](in) append next.intersperse(in)).run
            })).run
      })

  /**
   * SCAN FUNCTIONS
   */

  /** run a stateful function on each element and stream the new state */
  def scan[B](start: B)(f: (B, A) => B): Producer[F, B] =
    def go(p: Producer[F, A], previous: B): Producer[F, B] =
      Producer(p.run flatMap {
        case Done() => done[F, B].run
        case One(a) => one[F, B](f(previous, a)).run
        case More(as, next) =>
          val scanned = as.scanLeft(previous)(f).drop(1)
          (emit[F, B](scanned) append go(next, scanned.lastOption.getOrElse(previous))).run
      })

    one[F, B](start) append go(this, start)

  /** map the stream elements with a stateful function and return the mapped elements */
  def state[B, S](start: S)(f: (A, S) => (B, S)): Producer[F, B] =
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
    go(this, start)

  /** scan without an initial state */
  def scan1(f: (A, A) => A): Producer[F, A] =
    first.flatMap(a => drop(1).scan(a)(f))

  /** scan with a semigroup operation */
  def reduceSemigroup(using s: Semigroup[A]): Producer[F, A] =
    reduce(s.append(_, _))

  /** scan with a semigroup operation */
  def reduce(f: (A, A) => A): Producer[F, A] =
    scan1(f).last

  /** scan with a Monoid operation */
  def reduceMonoid(using s: Monoid[A]): Producer[F, A] =
    reduceSemigroup

  /** map and scan with a Monoid operation */
  def reduceMap[B](f: A => B)(using m: Monoid[B]): Producer[F, B] =
    map(f).reduceMonoid

  /** map with an effectful function and scan with a Monoid operation */
  def reduceMapEval[B](f: A => F[B])(using m: Monoid[B]): Producer[F, B] =
    mapEval(f).reduceMonoid

  /** map the stream elements with a stateful and effectful function */
  def stateF[B, S](start: S)(f: (A, S) => (F[B], S)): Producer[F, F[B]] =
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

    go(this, start)

  /**
   * map the stream elements with a stateful and effectful function returning a full stream
   * Then run one last function based on the latest state
   */
  def producerStateF[B, S](start: S, last: Option[S => Producer[F, B]] = None)(f: (A, S) => F[(Producer[F, B], S)]): Producer[F, B] =
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

    go(this, start)

  /**
   * map the stream elements with a stateful function returning a full stream
   * Then run one last function based on the latest state
   */
  def producerState[B, S](start: S, last: Option[S => Producer[F, B]] = None)(f: (A, S) => (Producer[F, B], S)): Producer[F, B] =
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
    go(this, start)



/**
 * List of common producers
 */
object Producer extends Producers:

  given [F[_] : Monad : Safe, A] as Monoid[Producer[F, A]] = new Monoid[Producer[F, A]]:
    def zero: Producer[F, A] = done[F, A]
    def append(p1: Producer[F, A], p2: =>Producer[F, A]): Producer[F, A] =
      p1 append p2

  given [F[_] : Monad : Safe] as Monad[Producer[F, *]] = new Monad[Producer[F, *]]:
    def bind[A, B](fa: Producer[F, A])(f: A => Producer[F, B]): Producer[F, B] =
      fa.flatMap(f)

    def point[A](a: =>A): Producer[F, A] =
      one[F, A](a)

  def flattenProducers[F[_] : Monad : Safe, A](producers: List[Producer[F, A]]): Producer[F, A] =
    producers match
      case Nil => done
      case p :: rest => p append flattenProducers(rest)

/**
 * List of common producers or functions creating producers
 */
trait Producers:

  def done[F[_] : Monad : Safe, A]: Producer[F, A] =
    Producer[F, A](summon[Monad[F]].pure(Done()))

  def one[F[_] : Monad : Safe, A](a: A): Producer[F, A] =
    Producer[F, A](summon[Monad[F]].pure(One(a)))

  def oneAsync[A](a: A): Producer[Action, A] =
    one[Action, A](a)

  def oneSync[A](a: A): Producer[Operation, A] =
    one[Operation, A](a)

  def oneDelayed[F[_] : Monad : Safe, A](e: =>A): Producer[F, A] =
    oneEval(summon[Monad[F]].pure(e))

  def oneDelayedAsync[A](e: =>A): Producer[Action, A] =
    oneDelayed[Action, A](e)

  def oneDelayedSync[A](e: =>A): Producer[Operation, A] =
    oneDelayed[Operation, A](e)

  def oneEval[F[_] : Monad : Safe, A](e: F[A]): Producer[F, A] =
    Producer[F, A](e.flatMap(a => one[F, A](a).run))

  def oneOrMore[F[_] : Monad : Safe, A](a: A, as: List[A]): Producer[F, A] =
    Producer[F, A](summon[Monad[F]].pure(More(a +: as, done[F, A])))

  def repeatValue[F[_] : Monad : Safe, A](a: A): Producer[F, A] =
    Producer(summon[Monad[F]].pure(More(List(a), repeatValue[F, A](a))))

  def repeatEval[F[_] : Monad : Safe, A](e: F[A]): Producer[F, A] =
    Producer(e.map(a => More(List(a), repeatEval(e))))

  def emit[F[_] : Monad : Safe, A](elements: List[A]): Producer[F, A] =
    elements match
      case Nil      => done[F, A]
      case a :: Nil => one[F, A](a)
      case a :: as  => oneOrMore(a, as)

  def emitAsync[A](elements: List[A]): Producer[Action, A] =
    emit[Action, A](elements)

  def emitSync[A](elements: List[A]): Producer[Operation, A] =
    emit[Operation, A](elements)

  def emitSeq[F[_] : Monad : Safe, A](elements: Seq[A]): Producer[F, A] =
    elements.headOption match
      case None    => done[F, A]
      case Some(a) => Producer(summon[Monad[F]].pure(More[F, A](elements.headOption.toList, emitSeq(elements.tail))))

  def emitSeqAsync[A](elements: Seq[A]): Producer[Action, A] =
    emitSeq[Action, A](elements)

  def emitSeqSync[A](elements: Seq[A]): Producer[Operation, A] =
    emitSeq[Operation, A](elements)

  def emitAll[F[_] : Monad : Safe, A](elements: A*): Producer[F, A] =
    emitSeq(elements)

  def emitAllAsync[A](elements: A*): Producer[Action, A] =
    emitSeq[Action, A](elements)

  def emitAllSync[A](elements: A*): Producer[Operation, A] =
    emitSeq[Operation, A](elements)

  def eval[F[_] : Monad : Safe, A](a: F[A]): Producer[F, A] =
    Producer(a.map(One(_)))

  def evalProducer[F[_] : Monad : Safe, A](a: F[Producer[F, A]]): Producer[F, A] =
    Producer(a.flatMap(_.run))

  def emitAction[F[_] : Monad : Safe, A](elements: F[List[A]]): Producer[F, A] =
    Producer(elements flatMap {
      case Nil      => done[F, A].run
      case a :: Nil => one[F, A](a).run
      case a :: as  => oneOrMore[F, A](a, as).run
    })

  def empty[F[_] : Monad : Safe, A]: Producer[F, A] =
    done

  def bracket1[F[_] : Monad : Safe, A, B, C](open: F[A])(step: A => Producer[F, B])(close: A => Finalizer): Producer[F, B] =
    Producer[F, B] {
      open flatMap { resource =>
        Safe[F].finalizeWith(step(resource).run, close(resource))
      }
    }

  implicit class TransducerOps[F[_], A, B](transducer: Transducer[F, A, B]):
    def |>[C](other: Transducer[F, B, C]): Transducer[F, A, C] = (p: Producer[F, A]) =>
      other(transducer(p))

object Producers extends Producers
