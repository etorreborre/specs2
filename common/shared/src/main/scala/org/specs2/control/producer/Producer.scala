package org.specs2.control
package producer

import org.specs2._
import fp._, syntax._
import Producer._
import Transducer._
import Action._
import origami._

sealed trait Stream[A]
case class Done1[A]() extends Stream[A]
case class One1[A](a: A) extends Stream[A]
case class More1[A](as: List[A], next: Producer[A]) extends Stream[A]

case class Producer[A](run: Action[Stream[A]]) {

  def flatMap[B](f: A => Producer[B]): Producer[B] =
    cata[A, B](this)(
      done[B],
      (a: A) => f(a),
      (as: List[A], next: Producer[A]) => as.map(f).foldMap(identity) append next.flatMap(f))

  def map[B](f: A => B): Producer[B] =
    flatMap(a => one(f(a)))

  def mapEval[B](f: A => Action[B]): Producer[B] =
    flatMap(a => Producer.eval(f(a)))

  def collect[B](pf: PartialFunction[A, B]): Producer[B] =
    flatMap { a =>
      if (pf.isDefinedAt(a)) one(pf(a))
      else done
    }

  def append(other: Producer[A]): Producer[A] =
    Producer(run.flatMap {
      case Done1()         => protect(other.run).flatten
      case One1(a1)        => protect(More1(List(a1), other))
      case More1(as, next) => protect(More1(as, next append other))
    })

  def zip[B](other: Producer[B]): Producer[(A, B)] =
    Producer(run flatMap {
      case Done1() => done.run
      case One1(a) =>
        other.run flatMap {
          case Done1() => done.run
          case One1(b) => one((a, b)).run

          case More1(bs, next) =>
            (bs.headOption.map(b => one[(A, B)]((a, b))).getOrElse(done) append (this.drop(1) zip other.drop(1))).run
        }

      case More1(Nil, next) => next.zip(other).run

      case More1(as, nexta) =>
        other.run flatMap {
          case Done1() => done.run
          case One1(b) => as.headOption.map(a => one[(A, B)]((a, b))).getOrElse(done).run

          case More1(bs, nextb) =>
            if (as.size == bs.size)
              (emit(as zip bs) append (nexta zip nextb)).run
            else if (as.size < bs.size)
              (emit(as zip bs) append (nexta zip (emit(bs.drop(as.size)) append nextb))).run
            else
              (emit(as zip bs) append ((emit(as.drop(bs.size)) append nexta) zip nextb)).run
        }
    })

  def andFinally(finalizer: Finalizer): Producer[A] =
    Producer(run.addLast(finalizer))
}


object Producer extends Producers1 {

  implicit def MonoidProducer[A]: Monoid[Producer[A]] = new Monoid[Producer[A]] {
    def zero: Producer[A] = done[A]
    def append(p1: Producer[A], p2: =>Producer[A]): Producer[A] =
      p1 append p2
  }

//  implicit def FoldableProducer: Foldable[Producer[?]] = new Foldable[Producer[?]] {
//    override def foldLeft[A, B](fa: Producer[A], b: B)(f: (B, A) => B): B = {
//      var s = b
//      fa.run.run match {
//        case Done1() => Action.unit
//        case One1(a) => s = f(s, a); Action.unit
//        case More1(as, next) => s = as.foldLeft(s)(f); s = foldLeft(next, s)(f); Action.unit
//      }
//      s
//    }
//
//    def foldRight[A, B](fa: Producer[A], lb: =>B)(f: (A, =>B) => B): B = {
//      var s = Need(lb)
//      fa.run.run match {
//        case Done1() => Action.unit
//        case One1(a) => s = Need(f(a, s.value)); Action.unit
//        case More1(as, next) =>
//          lazy val ls = Need(as.foldRight(s.value)((a, b) => f(a, b)))
//          s = Need(foldRight(next, ls.value)((a, b) => f(a, b)))
//          Action.unit
//      }
//      s.value
//    }
//
//    def foldMap[A,B](fa: Producer[A])(f: A => B)(implicit F: Monoid[B]): B =
//      foldLeft(fa, F.zero)((b: B, a: A) => F.append(b, f(a)))
//
//  }

  implicit def ProducerMonad: Monad[Producer[?]] = new Monad[Producer[?]] {
    def bind[A, B](fa: Producer[A])(f: A => Producer[B]): Producer[B] =
      fa.flatMap(f)

    def point[A](a: =>A): Producer[A] =
      one(a)
  }

  implicit class ProducerOps[A](p: Producer[A]) {
    def filter(f: A => Boolean): Producer[A] =
      Producer.filter(p)(f)

    def sliding(n: Int): Producer[List[A]] =
      Producer.sliding(n)(p)

    def chunk(n: Int): Producer[A] =
      Producer.chunk(n)(p)

    def >(p2: Producer[A]): Producer[A] =
      p append p2

    def |>[B](t: Transducer[A, B]): Producer[B] =
      pipe(t)

    def pipe[B](t: Transducer[A, B]): Producer[B] =
      Producer.pipe(p, t)

    def fold[B, S](start: Action[S], f: (S, A) => Action[S], end: S => Action[B]): Action[B] =
      Producer.fold(p)(start, f, end)

    def fold[S, B](f: Fold[Action[?], A, B]): Action[B] =
      Producer.fold(p)(f.start, f.fold, f.end)

    def observe[S](start: Action[S], f: (S, A) => S, end: S => Action[Unit]): Producer[A] =
      Producer.observe(p)(start, f, end)

    def runLast: Action[Option[A]] =
      Producer.runLast(p)

    def runList: Action[List[A]] =
      Producer.runList(p)

    def repeat: Producer[A] =
      Producer.repeat(p)
  }

  implicit class ProducerListOps[A](p: Producer[List[A]]) {
    def flattenList: Producer[A] =
      Producer.flattenList(p)
  }

  implicit class ProducerSeqOps[A](p: Producer[Seq[A]]) {
    def flattenSeq: Producer[A] =
      Producer.flattenSeq(p)
  }

  implicit class ProducerFlattenOps[A](p: Producer[Producer[A]]) {
    def flatten: Producer[A] =
      Producer.flatten(p)
  }

  implicit class ProducerActionOps[A](p: Producer[Action[A]]) {
    def sequence[F[_]](n: Int): Producer[A] =
      Producer.sequence[F, A](n)(p)
  }

  implicit class ProducerTransducerOps[A](p: Producer[A]) {
    def receiveOr[B](f: A => Producer[B])(or: =>Producer[B]): Producer[B] =
      p |> Transducers.receiveOr(f)(or)

    def receiveOption[B]: Producer[Option[A]] =
      p |> Transducers.receiveOption

    def drop(n: Int): Producer[A] =
      p |> Transducers.drop(n)

    def dropRight(n: Int): Producer[A] =
      p |> Transducers.dropRight(n)

    def take(n: Int): Producer[A] =
      p |> Transducers.take(n)

    def takeWhile(f: A => Boolean): Producer[A] =
      p |> Transducers.takeWhile(f)

    def zipWithPrevious: Producer[(Option[A], A)] =
      p |> Transducers.zipWithPrevious

    def zipWithPreviousN(n: Int): Producer[(List[A], A)] =
      p |> Transducers.zipWithPreviousN(n)

    def zipWithNext: Producer[(A, Option[A])] =
      p |> Transducers.zipWithNext

    def zipWithNextN(n: Int): Producer[(A, List[A])] =
      p |> Transducers.zipWithNextN(n)

    def zipWithPreviousAndNext: Producer[(Option[A], A, Option[A])] =
      p |> Transducers.zipWithPreviousAndNext

    def zipWithPreviousAndNextN(n: Int): Producer[(List[A], A, List[A])] =
      p |> Transducers.zipWithPreviousAndNextN(n)

    def zipWithIndex: Producer[(A, Int)] =
      p |> Transducers.zipWithIndex

    def intersperse(a: A): Producer[A] =
      p |> Transducers.intersperse(a: A)

    def first: Producer[A] =
      p |> Transducers.first

    def last: Producer[A] =
      p |> Transducers.last

    def scan[B](start: B)(f: (B, A) => B): Producer[B] =
      p |> Transducers.scan(start)(f)

    def scan1(f: (A, A) => A): Producer[A] =
      p |> Transducers.scan1(f)

    def state[B, S](start: S)(f: (A, S) => (B, S)): Producer[B] =
      p |> Transducers.state(start)(f)

    def producerState[B, S](start: S, last: Option[S => Producer[B]] = None)(f: (A, S) => (Producer[B], S)): Producer[B] =
      p |> Transducers.producerState(start, last)(f)

    def reduce(f: (A, A) => A): Producer[A] =
      p |> Transducers.reduce(f)

    def reduceSemigroup(implicit semi: Semigroup[A]): Producer[A] =
      p |> Transducers.reduceSemigroup

    def reduceMonoid(implicit monoid: Monoid[A]): Producer[A] =
      p |> Transducers.reduceMonoid

    def reduceMap[B : Monoid](f: A => B): Producer[B] =
      p |> Transducers.reduceMap[A, B](f)
  }

  implicit class ProducerResourcesOps[A](p: Producer[A]) {
    def thenFinally(e: Finalizer): Producer[A] =
      Producer[A](p.run flatMap {
        case Done1() => Action.thenFinally(Producer.done[A].run, e)
        case One1(a) => Action.thenFinally(Producer.one[A](a).run, e)
        case More1(as, next) => protect(More1(as, ProducerResourcesOps(next).thenFinally(e)))
      })

    def `finally`(e: Finalizer): Producer[A] =
      p.thenFinally(e)

    def attempt: Producer[Throwable Either A] =
      Producer[Throwable Either A](Action.attempt(p.run) map {
        case Right(Done1()) => Done1()
        case Right(One1(a)) => One1(Right(a))
        case Right(More1(as, next)) => More1(as.map(Either.right), next.map(Either.right))

        case Left(t) => One1(Either.left(t))
      })
  }

  def bracket1[A, B, C](open: Action[A])(step: A => Producer[B])(close: A => Finalizer): Producer[B] =
    Producer[B] {
      open flatMap { resource =>
        step(resource).run.addLast(close(resource))
      }
    }
}

trait Producers1 {
  def done[A]: Producer[A] =
    Producer[A](Action.pure(Done1()))

  def one[A](a: A): Producer[A] =
    Producer[A](Action.pure(One1(a)))

  def oneAction[A](e: Action[A]): Producer[A] =
    Producer[A](e.flatMap(a => one(a).run))

  def oneOrMore[A](a: A, as: List[A]): Producer[A] =
    Producer[A](Action.pure(More1(a +: as, done)))

  def repeat[A](p: Producer[A]): Producer[A] =
    Producer(p.run flatMap {
      case Done1() => Action.pure(Done1())
      case One1(a) => protect(More1(List(a), repeat(p)))
      case More1(as, next) => protect(More1(as, next append repeat(p)))
    })

  def repeatValue[A](a: A): Producer[A] =
    Producer(protect(More1(List(a), repeatValue(a))))

  def repeatEval[A](e: Action[A]): Producer[A] =
    Producer(e.map(a => More1(List(a), repeatEval(e))))

  def fill[A](n: Int)(p: Producer[A]): Producer[A] =
    if (n <= 0) done[A]
    else p append fill(n - 1)(p)

  def emit[A](elements: List[A]): Producer[A] =
    elements match {
      case Nil      => done[A]
      case a :: Nil => one[A](a)
      case a :: as  => oneOrMore(a, as)
    }

  def emitSeq[A](elements: Seq[A]): Producer[A] =
    elements.headOption match {
      case None    => done[A]
      case Some(a) => Producer(protect(More1[A](elements.headOption.toList, emitSeq(elements.tail))))
    }

  def eval[A](a: Action[A]): Producer[A] =
    Producer(a.map(One1(_)))

  def evalProducer[A](a: Action[Producer[A]]): Producer[A] =
    Producer(a.flatMap(_.run))

  def emitAction[A](elements: Action[List[A]]): Producer[A] =
    Producer(elements flatMap {
      case Nil      => done[A].run
      case a :: Nil => one(a).run
      case a :: as  => oneOrMore(a, as).run
    })

  def fold[A, B, S](producer: Producer[A])(start: Action[S], f: (S, A) => Action[S], end: S => Action[B]): Action[B] = {
    producer.run flatMap {
      case Done1() => start.flatMap(end)
      case One1(a) => start.flatMap(s1 => f(s1, a).flatMap(end))
      case More1(as, next) =>
        start.flatMap { s1 =>
          as.foldLeftM(s1)(f).flatMap { s =>
            fold(next)(protect(s), f, end)
          }
        }
    }
  }

  def observe[A, S](producer: Producer[A])(start: Action[S], f: (S, A) => S, end: S => Action[Unit]): Producer[A] =
    Producer[A](start flatMap { init =>
      def go(p: Producer[A], s: S): Producer[A] =
        Producer[A] {
          p.run flatMap {
            case Done1() => end(s) >> done[A].run
            case One1(a) => end(s) >> one[A](a).run
            case More1(as, next) =>
              val newS = as.foldLeft(s)(f)
              (emit(as) append go(next, newS)).run
          }
        }

      go(producer, init).run
    })

  def runLast[A](producer: Producer[A]): Action[Option[A]] =
    producer.run flatMap {
      case One1(a) => Action.pure[Option[A]](Option(a))
      case Done1() => Action.pure[Option[A]](None)
      case More1(as, next) => runLast(next).map(_.orElse(as.lastOption))
    }

  def runList[A](producer: Producer[A]): Action[List[A]] =
    producer.fold(Action.pure(Vector[A]()), (vs: Vector[A], a: A) => Action.pure(vs :+ a), (vs:Vector[A]) => Action.pure(vs.toList))

//  def collect[A](producer: Producer[A])(implicit m: Member[Writer[A, ?], R]): Action[Unit] =
//    producer.run flatMap {
//      case Done1() => Action.pure(())
//      case One1(a) => tell(a)
//      case More1(as, next) => as.traverse(tell[A]) >> collect(next)
//    }

  def empty[A]: Producer[A] =
    done

  def pipe[A, B](p: Producer[A], t: Transducer[A, B]): Producer[B] =
    t(p)

  def filter[A](producer: Producer[A])(f: A => Boolean): Producer[A] =
    Producer(producer.run flatMap {
      case Done1() => done.run
      case One1(a) => protect[A](a).as(if (f(a)) One1(a) else Done1())
      case More1(as, next) =>
        as filter f match {
          case Nil => next.filter(f).run
          case a :: rest => (oneOrMore(a, rest) append next.filter(f)).run
        }
    })

  def flatten[A](producer: Producer[Producer[A]]): Producer[A] =
    Producer(producer.run flatMap {
      case Done1() => done.run
      case One1(p) => p.run
      case More1(ps, next) => (flattenProducers(ps) append flatten(next)).run
    })

  def flattenProducers[A](producers: List[Producer[A]]): Producer[A] =
    producers match {
      case Nil => done
      case p :: rest => p append flattenProducers(rest)
    }

  def flattenSeq[A](producer: Producer[Seq[A]]): Producer[A] =
    producer.flatMap(as => emitSeq(as.toList))

  /** accumulate chunks of size n inside More nodes */
  def chunk[A](size: Int)(producer: Producer[A]): Producer[A] = {
    def go(p: Producer[A], elements: Vector[A]): Producer[A] =
      Producer[A](
        p.run flatMap {
          case Done1() => emit[A](elements.toList).run
          case One1(a) => emit[A]((elements :+ a).toList).run

          case More1(as, next) =>
            val es = elements ++ as
            if (es.size == size) (emit[A](es.toList) append go(next, Vector.empty)).run
            else                 go(next, es).run
        })

    go(producer, Vector.empty)
  }

  def sliding[A](size: Int)(producer: Producer[A]): Producer[List[A]] = {

    def go(p: Producer[A], elements: Vector[A]): Producer[List[A]] =
      Producer[List[A]](
        peek(p).flatMap {
          case (Some(a), as) =>
            val es = elements :+ a
            if (es.size == size) (one(es.toList) append go(as, Vector.empty)).run
            else                 go(as, es).run

          case (None, _) =>
            one(elements.toList).run
        })

    go(producer, Vector.empty)
  }

  def peek[A](producer: Producer[A]): Action[(Option[A], Producer[A])] =
    producer.run map {
      case Done1() => (None, done[A])
      case One1(a) => (Option(a), done[A])
      case More1(as, next) => (as.headOption, emit(as.tail) append next)
    }

  def peekN[A](producer: Producer[A], n: Int): Action[(List[A], Producer[A])] = {
    def go(p: Producer[A], collected: Vector[A]): Action[(List[A], Producer[A])] =
      p.run flatMap {
        case Done1() => Action.pure((collected.toList, done[A]))
        case One1(a) => Action.pure(((collected :+ a).take(n).toList, done[A]))
        case More1(as, next) =>
          val all = collected ++ as
          if (all.size >= n)
            Action.pure((all.take(n).toList, emit(all.drop(n).toList) append next))
          else
            go(next, all)
      }

    go(producer, Vector.empty)
  }

  def flattenList[A](producer: Producer[List[A]]): Producer[A] =
    producer.flatMap(emit[A])

  def sequence[F[_], A](n: Int)(producer: Producer[Action[A]]) =
    sliding(n)(producer).flatMap { actions => Producer.emitAction(Traverse[List].sequence(actions)) }

  private[producer] def cata[A, B](producer: Producer[A])(onDone: Producer[B], onOne: A => Producer[B], onMore: (List[A], Producer[A]) => Producer[B]): Producer[B] =
    Producer[B](producer.run.flatMap {
      case Done1() => onDone.run
      case One1(a) => onOne(a).run
      case More1(as, next) => onMore(as, next).run
    })

}
