package org.specs2.control
package producer

import org.specs2._
import fp._, syntax._
import Producer._
import Transducer._
import origami._

sealed trait Stream[F[_], A]
case class Done[F[_], A]() extends Stream[F, A]
case class One[F[_], A](a: A) extends Stream[F, A]
case class More[F[_], A](as: List[A], next: Producer[F, A]) extends Stream[F, A]

case class Producer[F[_] : Monad : Safe, A](run: F[Stream[F, A]]) {

  def flatMap[B](f: A => Producer[F, B]): Producer[F, B] =
    cata[F, A, B](this)(
      done[F, B],
      (a: A) => f(a),
      (as: List[A], next: Producer[F, A]) => as.map(f).foldMap(identity) append next.flatMap(f))

  def map[B](f: A => B): Producer[F, B] =
    flatMap(a => one(f(a)))

  def mapEval[B](f: A => F[B]): Producer[F, B] =
    flatMap(a => Producer.eval(f(a)))

  def collect[B](pf: PartialFunction[A, B]): Producer[F, B] =
    flatMap { a =>
      if (pf.isDefinedAt(a)) one(pf(a))
      else done
    }

  def append(other: Producer[F, A]): Producer[F, A] =
    Producer(run.flatMap {
      case Done()         => Monad[F].pure(other.run).flatten
      case One(a1)        => Monad[F].pure(More(List(a1), other))
      case More(as, next) => Monad[F].pure(More(as, next append other))
    })

  def zip[B](other: Producer[F, B]): Producer[F, (A, B)] =
    Producer(run flatMap {
      case Done() => done[F, (A, B)].run
      case One(a) =>
        other.run flatMap {
          case Done() => done[F, (A, B)].run
          case One(b) => one[F, (A, B)]((a, b)).run

          case More(bs, next) =>
            (bs.headOption.map(b => one[F, (A, B)]((a, b))).getOrElse(done[F, (A, B)]) append (this.drop(1) zip other.drop(1))).run
        }

      case More(Nil, next) => next.zip(other).run

      case More(as, nexta) =>
        other.run flatMap {
          case Done() => done[F, (A, B)].run
          case One(b) => as.headOption.map(a => one[F, (A, B)]((a, b))).getOrElse(done[F, (A, B)]).run

          case More(bs, nextb) =>
            if (as.size == bs.size)
              (emit[F, (A, B)](as zip bs) append (nexta zip nextb)).run
            else if (as.size < bs.size)
              (emit[F, (A, B)](as zip bs) append (nexta zip (emit[F, B](bs.drop(as.size)) append nextb))).run
            else
              (emit[F, (A, B)](as zip bs) append ((emit[F, A](as.drop(bs.size)) append nexta) zip nextb)).run
        }
    })

  def andFinally(finalizer: Finalizer): Producer[F, A] =
    Producer(Safe[F].finalizeWith(run, finalizer))
}


object Producer extends Producers {

  implicit def MonoidProducer[F[_] : Monad : Safe, A]: Monoid[Producer[F, A]] = new Monoid[Producer[F, A]] {
    def zero: Producer[F, A] = done[F, A]
    def append(p1: Producer[F, A], p2: =>Producer[F, A]): Producer[F, A] =
      p1 append p2
  }

//  implicit def FoldableProducer[F[_] : Monad]: Foldable[Producer[F, ?]] = new Foldable[Producer[F, ?]] {
//    override def foldLeft[A, B](fa: Producer[F, A], b: B)(f: (B, A) => B): F[B] = {
//      var s: F[B] = Monad[F].pure(b)
//      fa.run.flatMap {
//        case Done() => Monad[F].pure(())
//        case One(a) => s = s.map(s1 => f(s1, a))
//        case More(as, next) => s = as.foldLeft(s)(f); s = foldLeft(next, s)(f); Monad[F].pure(())
//      }
//      s
//    }

//    def foldRight[A, B](fa: Producer[F, A], lb: =>B)(f: (A, =>B) => B): B = {
//      var s = Need(lb)
//      fa.run.flatMap {
//        case Done() => Monad[F].pure(())
//        case One(a) => s = Need(f(a, s.value)); Monad[F].pure(())
//        case More(as, next) =>
//          lazy val ls = Need(as.foldRight(s.value)((a, b) => f(a, b)))
//          s = Need(foldRight(next, ls.value)((a, b) => f(a, b)))
//          Monad[F].pure(())
//      }
//      s.value
//    }

//    def foldMap[A,B](fa: Producer[F, A])(f: A => B)(implicit F: Monoid[B]): F[B] =
//      foldLeft(fa, F.zero)((b: B, a: A) => F.append(b, f(a)))

//  }

  implicit def ProducerMonad[F[_] : Monad : Safe]: Monad[Producer[F, ?]] = new Monad[Producer[F, ?]] {
    def bind[A, B](fa: Producer[F, A])(f: A => Producer[F, B]): Producer[F, B] =
      fa.flatMap(f)

    def point[A](a: =>A): Producer[F, A] =
      one[F, A](a)
  }

  implicit class ProducerOps[F[_]: Monad : Safe, A](p: Producer[F, A]) {
    def filter(f: A => Boolean): Producer[F, A] =
      Producer.filter(p)(f)

    def sliding(n: Int): Producer[F, List[A]] =
      Producer.sliding(n)(p)

    def chunk(n: Int): Producer[F, A] =
      Producer.chunk(n)(p)

    def >(p2: Producer[F, A]): Producer[F, A] =
      p append p2

    def |>[B](t: Transducer[F, A, B]): Producer[F, B] =
      pipe(t)

    def pipe[B](t: Transducer[F, A, B]): Producer[F, B] =
      Producer.pipe(p, t)

    def fold[B, S](start: F[S], f: (S, A) => F[S], end: S => F[B]): F[B] =
      Producer.fold(p)(start, f, end)

    def fold[S, B](f: Fold[F, A, B]): F[B] =
      Producer.fold(p)(f.start, f.fold, f.end)

    def observe[S](start: F[S], f: (S, A) => S, end: S => F[Unit]): Producer[F, A] =
      Producer.observe(p)(start, f, end)

    def runLast: F[Option[A]] =
      Producer.runLast(p)

    def runList: F[List[A]] =
      Producer.runList(p)

    def repeat: Producer[F, A] =
      Producer.repeat(p)
  }

  implicit class ProducerListOps[F[_] : Monad : Safe, A](p: Producer[F, List[A]]) {
    def flattenList: Producer[F, A] =
      Producer.flattenList(p)
  }

  implicit class ProducerSeqOps[F[_] : Monad : Safe, A](p: Producer[F, Seq[A]]) {
    def flattenSeq: Producer[F, A] =
      Producer.flattenSeq(p)
  }

  implicit class ProducerFlattenOps[F[_] : Monad : Safe, A](p: Producer[F, Producer[F, A]]) {
    def flatten: Producer[F, A] =
      Producer.flatten(p)
  }

  implicit class ProducerActionOps[F[_] : Monad : Safe, A](p: Producer[F, F[A]]) {
    def sequence(n: Int): Producer[F, A] =
      Producer.sequence[F, A](n)(p)
  }

  implicit class ProducerTransducerOps[F[_] : Monad : Safe, A](p: Producer[F, A]) {
    def receiveOr[B](f: A => Producer[F, B])(or: =>Producer[F, B]): Producer[F, B] =
      p |> Transducers.receiveOr(f)(or)

    def receiveOption[B]: Producer[F, Option[A]] =
      p |> Transducers.receiveOption

    def drop(n: Int): Producer[F, A] =
      p |> Transducers.drop(n)

    def dropRight(n: Int): Producer[F, A] =
      p |> Transducers.dropRight(n)

    def take(n: Int): Producer[F, A] =
      p |> Transducers.take(n)

    def takeWhile(f: A => Boolean): Producer[F, A] =
      p |> Transducers.takeWhile(f)

    def zipWithPrevious: Producer[F, (Option[A], A)] =
      p |> Transducers.zipWithPrevious

    def zipWithPreviousN(n: Int): Producer[F, (List[A], A)] =
      p |> Transducers.zipWithPreviousN(n)

    def zipWithNext: Producer[F, (A, Option[A])] =
      p |> Transducers.zipWithNext

    def zipWithNextN(n: Int): Producer[F, (A, List[A])] =
      p |> Transducers.zipWithNextN(n)

    def zipWithPreviousAndNext: Producer[F, (Option[A], A, Option[A])] =
      p |> Transducers.zipWithPreviousAndNext

    def zipWithPreviousAndNextN(n: Int): Producer[F, (List[A], A, List[A])] =
      p |> Transducers.zipWithPreviousAndNextN(n)

    def zipWithIndex: Producer[F, (A, Int)] =
      p |> Transducers.zipWithIndex

    def intersperse(a: A): Producer[F, A] =
      p |> Transducers.intersperse(a: A)

    def first: Producer[F, A] =
      p |> Transducers.first

    def last: Producer[F, A] =
      p |> Transducers.last

    def scan[B](start: B)(f: (B, A) => B): Producer[F, B] =
      p |> Transducers.scan(start)(f)

    def scan1(f: (A, A) => A): Producer[F, A] =
      p |> Transducers.scan1(f)

    def state[B, S](start: S)(f: (A, S) => (B, S)): Producer[F, B] =
      p |> Transducers.state(start)(f)

    def producerState[B, S](start: S, last: Option[S => Producer[F, B]] = None)(f: (A, S) => (Producer[F, B], S)): Producer[F, B] =
      p |> Transducers.producerState(start, last)(f)

    def reduce(f: (A, A) => A): Producer[F, A] =
      p |> Transducers.reduce(f)

    def reduceSemigroup(implicit semi: Semigroup[A]): Producer[F, A] =
      p |> Transducers.reduceSemigroup

    def reduceMonoid(implicit monoid: Monoid[A]): Producer[F, A] =
      p |> Transducers.reduceMonoid

    def reduceMap[B : Monoid](f: A => B): Producer[F, B] =
      p |> Transducers.reduceMap[F, A, B](f)
  }

  implicit class ProducerResourcesOps[F[_]: Monad : Safe, A](p: Producer[F, A]) {
    def thenFinally(e: Finalizer): Producer[F, A] =
      Producer[F, A](p.run flatMap {
        case Done() => Safe[F].finalizeWith(Producer.done[F, A].run, e)
        case One(a) => Safe[F].finalizeWith(Producer.one[F, A](a).run, e)
        case More(as, next) => Monad[F].pure(More(as, ProducerResourcesOps(next).thenFinally(e)))
      })

    def `finally`(e: Finalizer): Producer[F, A] =
      p.thenFinally(e)

    def attempt: Producer[F, Throwable Either A] =
      Producer[F, Throwable Either A](Safe[F].attempt(p.run) map {
        case Right(Done()) => Done()
        case Right(One(a)) => One(Right(a))
        case Right(More(as, next)) => More(as.map(Either.right), next.map(Either.right))

        case Left(t) => One(Either.left(t))
      })
  }

  def bracket1[F[_] : Monad : Safe, A, B, C](open: F[A])(step: A => Producer[F, B])(close: A => Finalizer): Producer[F, B] =
    Producer[F, B] {
      open flatMap { resource =>
        Safe[F].finalizeWith(step(resource).run, close(resource))
      }
    }
}

trait Producers {

  def done[F[_] : Monad : Safe, A]: Producer[F, A] =
    Producer[F, A](Monad[F].pure(Done()))

  def one[F[_] : Monad : Safe, A](a: A): Producer[F, A] =
    Producer[F, A](Monad[F].pure(One(a)))

  def oneAction[F[_] : Monad : Safe, A](e: F[A]): Producer[F, A] =
    Producer[F, A](e.flatMap(a => one[F, A](a).run))

  def oneOrMore[F[_] : Monad : Safe, A](a: A, as: List[A]): Producer[F, A] =
    Producer[F, A](Monad[F].pure(More(a +: as, done[F, A])))

  def repeat[F[_] : Monad : Safe, A](p: Producer[F, A]): Producer[F, A] =
    Producer(p.run flatMap {
      case Done() => Monad[F].pure(Done())
      case One(a) => Monad[F].pure(More(List(a), repeat(p)))
      case More(as, next) => Monad[F].pure(More(as, next append repeat(p)))
    })

  def repeatValue[F[_] : Monad : Safe, A](a: A): Producer[F, A] =
    Producer(Monad[F].pure(More(List(a), repeatValue[F, A](a))))

  def repeatEval[F[_] : Monad : Safe, A](e: F[A]): Producer[F, A] =
    Producer(e.map(a => More(List(a), repeatEval(e))))

  def fill[F[_] : Monad : Safe, A](n: Int)(p: Producer[F, A]): Producer[F, A] =
    if (n <= 0) done[F, A]
    else p append fill(n - 1)(p)

  def emit[F[_] : Monad : Safe, A](elements: List[A]): Producer[F, A] =
    elements match {
      case Nil      => done[F, A]
      case a :: Nil => one[F, A](a)
      case a :: as  => oneOrMore(a, as)
    }

  def emitSeq[F[_] : Monad : Safe, A](elements: Seq[A]): Producer[F, A] =
    elements.headOption match {
      case None    => done[F, A]
      case Some(a) => Producer(Monad[F].pure(More[F, A](elements.headOption.toList, emitSeq(elements.tail))))
    }

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

  def fold[F[_] : Monad : Safe, A, B, S](producer: Producer[F, A])(start: F[S], f: (S, A) => F[S], end: S => F[B]): F[B] = {
    producer.run flatMap {
      case Done() => start.flatMap(end)
      case One(a) => start.flatMap(s1 => f(s1, a).flatMap(end))
      case More(as, next) =>
        start.flatMap { s1 =>
          as.foldLeftM(s1)(f).flatMap { s =>
            fold(next)(Monad[F].pure(s), f, end)
          }
        }
    }
  }

  def observe[F[_] : Monad : Safe, A, S](producer: Producer[F, A])(start: F[S], f: (S, A) => S, end: S => F[Unit]): Producer[F, A] =
    Producer[F, A](start flatMap { init =>
      def go(p: Producer[F, A], s: S): Producer[F, A] =
        Producer[F, A] {
          p.run flatMap {
            case Done() => end(s) >> done[F, A].run
            case One(a) => end(s) >> one[F, A](a).run
            case More(as, next) =>
              val newS = as.foldLeft(s)(f)
              (emit[F, A](as) append go(next, newS)).run
          }
        }

      go(producer, init).run
    })

  def runLast[F[_] : Monad : Safe, A](producer: Producer[F, A]): F[Option[A]] =
    producer.run flatMap {
      case One(a) => Monad[F].pure[Option[A]](Option(a))
      case Done() => Monad[F].pure[Option[A]](None)
      case More(as, next) => runLast(next).map(_.orElse(as.lastOption))
    }

  def runList[F[_] : Monad : Safe, A](producer: Producer[F, A]): F[List[A]] =
    producer.fold(Monad[F].pure(Vector[A]()), (vs: Vector[A], a: A) => Monad[F].pure(vs :+ a), (vs:Vector[A]) => Monad[F].pure(vs.toList))

  def empty[F[_] : Monad : Safe, A]: Producer[F, A] =
    done

  def pipe[F[_] : Monad : Safe, A, B](p: Producer[F, A], t: Transducer[F, A, B]): Producer[F, B] =
    t(p)

  def filter[F[_] : Monad : Safe, A](producer: Producer[F, A])(f: A => Boolean): Producer[F, A] =
    Producer(producer.run flatMap {
      case Done() => done[F, A].run
      case One(a) => Monad[F].pure(a).as(if (f(a)) One(a) else Done())
      case More(as, next) =>
        as filter f match {
          case Nil => next.filter(f).run
          case a :: rest => (oneOrMore[F, A](a, rest) append next.filter(f)).run
        }
    })

  def flatten[F[_] : Monad : Safe, A](producer: Producer[F, Producer[F, A]]): Producer[F, A] =
    Producer(producer.run flatMap {
      case Done() => done[F, A].run
      case One(p) => p.run
      case More(ps, next) => (flattenProducers(ps) append flatten(next)).run
    })

  def flattenProducers[F[_] : Monad : Safe, A](producers: List[Producer[F, A]]): Producer[F, A] =
    producers match {
      case Nil => done
      case p :: rest => p append flattenProducers(rest)
    }

  def flattenSeq[F[_] : Monad : Safe, A](producer: Producer[F, Seq[A]]): Producer[F, A] =
    producer.flatMap(as => emitSeq(as.toList))

  /** accumulate chunks of size n inside More nodes */
  def chunk[F[_] : Monad : Safe, A](size: Int)(producer: Producer[F, A]): Producer[F, A] = {
    def go(p: Producer[F, A], elements: Vector[A]): Producer[F, A] =
      Producer[F, A](
        p.run flatMap {
          case Done() => emit[F, A](elements.toList).run
          case One(a) => emit[F, A]((elements :+ a).toList).run

          case More(as, next) =>
            val es = elements ++ as
            if (es.size == size) (emit[F, A](es.toList) append go(next, Vector.empty)).run
            else                 go(next, es).run
        })

    go(producer, Vector.empty)
  }

  def sliding[F[_] : Monad : Safe, A](size: Int)(producer: Producer[F, A]): Producer[F, List[A]] = {

    def go(p: Producer[F, A], elements: Vector[A]): Producer[F, List[A]] =
      Producer[F, List[A]](
        peek(p).flatMap {
          case (Some(a), as) =>
            val es = elements :+ a
            if (es.size == size) (one[F, List[A]](es.toList) append go(as, Vector.empty)).run
            else                 go(as, es).run

          case (None, _) =>
            one[F, List[A]](elements.toList).run
        })

    go(producer, Vector.empty)
  }

  def peek[F[_] : Monad : Safe, A](producer: Producer[F, A]): F[(Option[A], Producer[F, A])] =
    producer.run map {
      case Done() => (None, done[F, A])
      case One(a) => (Option(a), done[F, A])
      case More(as, next) => (as.headOption, emit[F, A](as.tail) append next)
    }

  def peekN[F[_] : Monad : Safe, A](producer: Producer[F, A], n: Int): F[(List[A], Producer[F, A])] = {
    def go(p: Producer[F, A], collected: Vector[A]): F[(List[A], Producer[F, A])] =
      p.run flatMap {
        case Done() => Monad[F].pure((collected.toList, done[F, A]))
        case One(a) => Monad[F].pure(((collected :+ a).take(n).toList, done[F, A]))
        case More(as, next) =>
          val all = collected ++ as
          if (all.size >= n)
          Monad[F].pure((all.take(n).toList, emit[F, A](all.drop(n).toList) append next))
          else
            go(next, all)
      }

    go(producer, Vector.empty)
  }

  def flattenList[F[_] : Monad : Safe, A](producer: Producer[F, List[A]]): Producer[F, A] =
    producer.flatMap(emit[F, A])

  def sequence[F[_] : Monad : Safe, A](n: Int)(producer: Producer[F, F[A]]) =
    sliding(n)(producer).flatMap { actions => Producer.emitAction(Traverse[List].sequence(actions)) }

  private[producer] def cata[F[_] : Monad : Safe, A, B](producer: Producer[F, A])(onDone: Producer[F, B], onOne: A => Producer[F, B], onMore: (List[A], Producer[F, A]) => Producer[F, B]): Producer[F, B] =
    Producer[F, B](producer.run.flatMap {
      case Done() => onDone.run
      case One(a) => onOne(a).run
      case More(as, next) => onMore(as, next).run
    })

}
