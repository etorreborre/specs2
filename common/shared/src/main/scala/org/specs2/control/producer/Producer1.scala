package org.specs2.control
package producer

import org.specs2.fp._
import org.specs2.fp.syntax._
import Producer1._
import transducers1._
import Action1._

sealed trait Stream1[A]
case class Done1[A]() extends Stream1[A]
case class One1[A](a: A) extends Stream1[A]
case class More1[A](as: List[A], next: Producer1[A]) extends Stream1[A]

case class Producer1[A](run: Action1[Stream1[A]]) {

  def flatMap[B](f: A => Producer1[B]): Producer1[B] =
    cata[A, B](this)(
      done[B],
      (a: A) => f(a),
      (as: List[A], next: Producer1[A]) => as.map(f).foldMap(identity) append next.flatMap(f))

  def map[B](f: A => B): Producer1[B] =
    flatMap(a => one(f(a)))

  def mapEval[B](f: A => Action1[B]): Producer1[B] =
    flatMap(a => Producer1.eval(f(a)))

  def collect[B](pf: PartialFunction[A, B]): Producer1[B] =
    flatMap { a =>
      if (pf.isDefinedAt(a)) one(pf(a))
      else done
    }

  def append(other: Producer1[A]): Producer1[A] =
    Producer1(run.flatMap {
      case Done1()         => protect(other.run).flatten
      case One1(a1)        => protect(More1(List(a1), other))
      case More1(as, next) => protect(More1(as, next append other))
    })

  def zip[B](other: Producer1[B]): Producer1[(A, B)] =
    Producer1(run flatMap {
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

  def andFinally(last: Action1[Unit]): Producer1[A] =
    Producer1(run.addLast(last))
}


object Producer1 extends Producers1 {

  implicit def MonoidProducer1[A]: Monoid[Producer1[A]] = new Monoid[Producer1[A]] {
    def zero: Producer1[A] = done[A]
    def append(p1: Producer1[A], p2: =>Producer1[A]): Producer1[A] =
      p1 append p2
  }

//  implicit def FoldableProducer1: Foldable[Producer1[?]] = new Foldable[Producer1[?]] {
//    override def foldLeft[A, B](fa: Producer1[A], b: B)(f: (B, A) => B): B = {
//      var s = b
//      fa.run.run match {
//        case Done1() => Action1.unit
//        case One1(a) => s = f(s, a); Action1.unit
//        case More1(as, next) => s = as.foldLeft(s)(f); s = foldLeft(next, s)(f); Action1.unit
//      }
//      s
//    }
//
//    def foldRight[A, B](fa: Producer1[A], lb: =>B)(f: (A, =>B) => B): B = {
//      var s = Need(lb)
//      fa.run.run match {
//        case Done1() => Action1.unit
//        case One1(a) => s = Need(f(a, s.value)); Action1.unit
//        case More1(as, next) =>
//          lazy val ls = Need(as.foldRight(s.value)((a, b) => f(a, b)))
//          s = Need(foldRight(next, ls.value)((a, b) => f(a, b)))
//          Action1.unit
//      }
//      s.value
//    }
//
//    def foldMap[A,B](fa: Producer1[A])(f: A => B)(implicit F: Monoid[B]): B =
//      foldLeft(fa, F.zero)((b: B, a: A) => F.append(b, f(a)))
//
//  }

  implicit def Producer1Monad: Monad[Producer1[?]] = new Monad[Producer1[?]] {
    def bind[A, B](fa: Producer1[A])(f: A => Producer1[B]): Producer1[B] =
      fa.flatMap(f)

    def point[A](a: =>A): Producer1[A] =
      one(a)
  }

}

trait Producers1 {
  def done[A]: Producer1[A] =
    Producer1[A](Action1.pure(Done1()))

  def one[A](a: A): Producer1[A] =
    Producer1[A](Action1.pure(One1(a)))

  def oneAction1[A](e: Action1[A]): Producer1[A] =
    Producer1[A](e.flatMap(a => one(a).run))

  def oneOrMore[A](a: A, as: List[A]): Producer1[A] =
    Producer1[A](Action1.pure(More1(a +: as, done)))

  def repeat[A](p: Producer1[A]): Producer1[A] =
    Producer1(p.run flatMap {
      case Done1() => Action1.pure(Done1())
      case One1(a) => protect(More1(List(a), repeat(p)))
      case More1(as, next) => protect(More1(as, next append repeat(p)))
    })

  def repeatValue[A](a: A): Producer1[A] =
    Producer1(protect(More1(List(a), repeatValue(a))))

  def repeatEval[A](e: Action1[A]): Producer1[A] =
    Producer1(e.map(a => More1(List(a), repeatEval(e))))

  def fill[A](n: Int)(p: Producer1[A]): Producer1[A] =
    if (n <= 0) done[A]
    else p append fill(n - 1)(p)

  def emit[A](elements: List[A]): Producer1[A] =
    elements match {
      case Nil      => done[A]
      case a :: Nil => one[A](a)
      case a :: as  => oneOrMore(a, as)
    }

  def emitSeq[A](elements: Seq[A]): Producer1[A] =
    elements.headOption match {
      case None    => done[A]
      case Some(a) => Producer1(protect(More[A](elements.headOption.toList, emitSeq(elements.tail))))
    }

  def eval[A](a: Action1[A]): Producer1[A] =
    Producer1(a.map(One1(_)))

  def evalProducer1[A](a: Action1[Producer1[A]]): Producer1[A] =
    Producer1(a.flatMap(_.run))

  def emitAction1[A](elements: Action1[List[A]]): Producer1[A] =
    Producer1(elements flatMap {
      case Nil      => done[A].run
      case a :: Nil => one(a).run
      case a :: as  => oneOrMore(a, as).run
    })

  def fold[A, B, S](producer: Producer1[A])(start: Action1[S], f: (S, A) => Action1[S], end: S => Action1[B]): Action1[B] = {
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

  def observe[A, S](producer: Producer1[A])(start: Action1[S], f: (S, A) => S, end: S => Action1[Unit]): Producer1[A] =
    Producer1[A](start flatMap { init =>
      def go(p: Producer1[A], s: S): Producer1[A] =
        Producer1[A] {
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

  def runLast[A](producer: Producer1[A]): Action1[Option[A]] =
    producer.run flatMap {
      case One1(a) => Action1.pure[Option[A]](Option(a))
      case Done1() => Action1.pure[Option[A]](None)
      case More1(as, next) => runLast(next).map(_.orElse(as.lastOption))
    }

  def runList[A](producer: Producer1[A]): Action1[List[A]] =
    producer.fold(Action1.pure(Vector[A]()), (vs: Vector[A], a: A) => Action1.pure(vs :+ a), (vs:Vector[A]) => Action1.pure(vs.toList))

//  def collect[A](producer: Producer1[A])(implicit m: Member[Writer[A, ?], R]): Action1[Unit] =
//    producer.run flatMap {
//      case Done1() => Action1.pure(())
//      case One1(a) => tell(a)
//      case More1(as, next) => as.traverse(tell[A]) >> collect(next)
//    }

  def empty[A]: Producer1[A] =
    done

  def pipe[A, B](p: Producer1[A], t: Transducer1[A, B]): Producer1[B] =
    t(p)

  def filter[A](producer: Producer1[A])(f: A => Boolean): Producer1[A] =
    Producer1(producer.run flatMap {
      case Done1() => done.run
      case One1(a) => protect[A](a).as(if (f(a)) One1(a) else Done1())
      case More1(as, next) =>
        as filter f match {
          case Nil => next.filter(f).run
          case a :: rest => (oneOrMore(a, rest) append next.filter(f)).run
        }
    })

  def flatten[A](producer: Producer1[Producer1[A]]): Producer1[A] =
    Producer1(producer.run flatMap {
      case Done1() => done.run
      case One1(p) => p.run
      case More1(ps, next) => (flattenProducers(ps) append flatten(next)).run
    })

  def flattenProducers[A](producers: List[Producer1[A]]): Producer1[A] =
    producers match {
      case Nil => done
      case p :: rest => p append flattenProducers(rest)
    }

  def flattenSeq[A](producer: Producer1[Seq[A]]): Producer1[A] =
    producer.flatMap(as => emitSeq(as.toList))

  /** accumulate chunks of size n inside More nodes */
  def chunk[A](size: Int)(producer: Producer1[A]): Producer1[A] = {
    def go(p: Producer1[A], elements: Vector[A]): Producer1[A] =
      Producer1[A](
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

  def sliding[A](size: Int)(producer: Producer1[A]): Producer1[List[A]] = {

    def go(p: Producer1[A], elements: Vector[A]): Producer1[List[A]] =
      Producer1[List[A]](
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

  def peek[A](producer: Producer1[A]): Action1[(Option[A], Producer1[A])] =
    producer.run map {
      case Done1() => (None, done[A])
      case One1(a) => (Option(a), done[A])
      case More1(as, next) => (as.headOption, emit(as.tail) append next)
    }

  def peekN[A](producer: Producer1[A], n: Int): Action1[(List[A], Producer1[A])] = {
    def go(p: Producer1[A], collected: Vector[A]): Action1[(List[A], Producer1[A])] =
      p.run flatMap {
        case Done1() => Action1.pure((collected.toList, done[A]))
        case One1(a) => Action1.pure(((collected :+ a).take(n).toList, done[A]))
        case More1(as, next) =>
          val all = collected ++ as
          if (all.size >= n)
            Action1.pure((all.take(n).toList, emit(all.drop(n).toList) append next))
          else
            go(next, all)
      }

    go(producer, Vector.empty)
  }

  def flattenList[A](producer: Producer1[List[A]]): Producer1[A] =
    producer.flatMap(emit[A])

  def sequence[F[_], A](n: Int)(producer: Producer1[Action1[A]]) =
    sliding(n)(producer).flatMap { actions => Producer1.emitAction1(Action1.sequenceA(actions)) }

  private[producer] def cata[A, B](producer: Producer1[A])(onDone: Producer1[B], onOne: A => Producer1[B], onMore: (List[A], Producer1[A]) => Producer1[B]): Producer1[B] =
    Producer1[B](producer.run.flatMap {
      case Done1() => onDone.run
      case One1(a) => onOne(a).run
      case More1(as, next) => onMore(as, next).run
    })

}
