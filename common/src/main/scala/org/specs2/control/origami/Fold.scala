package org.specs2
package control
package origami

import eff.syntax.all._
import eff._, all._
import org.specs2.fp._
import org.specs2.fp.syntax._

/**
 * A Fold is a "left fold" over a data structure with:
 *  - a 'start' value
 *  - a 'fold' method to accumulate state
 *  - an 'end' method to finalize the result
 *
 * Both 'start' and 'end' have an effect which allows the whole folding to take place inside a context.
 *
 */
trait Fold[R, A, B] { self =>
  type S

  def start: Eff[R, S]
  def fold: (S, A) => S
  def end(s: S): Eff[R, B]

  /** map the output value */
  def map[C](f: B => C) = new Fold[R, A, C] {
    type S = self.S
    def start = self.start
    def fold = self.fold
    def end(s: S) = self.end(s).map(f)
  }

  /** flatMap the output value */
  def mapFlatten[C](f: B => Eff[R, C]) = new Fold[R, A, C] {
    type S = self.S
    def start = self.start
    def fold = self.fold
    def end(s: S) = self.end(s).flatMap(f)
  }

  /** run another fold on the end result */
  def pipe[C](f: Fold[R, B, C]) = new Fold[R, A, C] {
    type S = self.S
    def start = self.start
    def fold = self.fold
    def end(s: S) = self.end(s).flatMap(f.run1)
  }

  /** parallel composition */
  def ***[V, W](f: Fold[R, V, W]) = new Fold[R, (A, V), (B, W)] {
    type S = (self.S, f.S)
    def start = (self.start |@| f.start)((_,_))
    def fold = (s: S, av: (A, V)) => (self.fold(s._1, av._1), f.fold(s._2, av._2))
    def end(s: S) = (self.end(s._1) |@| f.end(s._2))((_,_))
  }

  /** fanout = zip in the Arrow terminology */
  def &&&[C](f: Fold[R, A, C]) =
    zip(f)

  /** contramap the input values */
  def contramap[C](f: C => A) = new Fold[R, C, B] {
    type S = self.S
    def start = self.start
    def fold = (s: S, c: C) => self.fold(s, f(c))
    def end(s: S) = self.end(s)
  }

  /** zip 2 folds to return a pair of values. alias for zip */
  def <*>[C](f: Fold[R, A, C]) =
    zip(f)

  /** zip 2 folds to return a pair of values. alias for <*> */
  def zip[C](f: Fold[R, A, C]) = new Fold[R, A, (B, C)] {
    type S = (self.S, f.S)
    def start = Applicative[Eff[R, ?]].tuple2(self.start, f.start)
    def fold = (s, a) => (self.fold(s._1, a), f.fold(s._2, a))
    def end(s: S) = Applicative[Eff[R, ?]].tuple2(self.end(s._1), f.end(s._2))
  }

  /** zip with another fold, running this one only for its side effects */
  def *>[C](f: Fold[R, A, C]): Fold[R, A, C] =
    zip(f).map(_._2)

  /** alias for *> */
  def observedBy[C](f: Fold[R, A, C]): Fold[R, A, C] =
    zip(f).map(_._2)

  /** zip with another fold only for its side effects */
  def <*[C](f: Fold[R, A, C]) =
    zip(f).map(_._1)

  /** alias for <* */
  def observe[C](f: Fold[R, A, C]) =
    zip(f).map(_._1)

  /** observe both the input value and the current state */
  def observeWithState(sink: Sink[R, (A, S)]) = new Fold[R, A, B] {
    type S = (self.S, sink.S)
    def start = Applicative[Eff[R, ?]].tuple2(self.start , sink.start)
    def fold = (s: S, a: A) => (self.fold(s._1, a), sink.fold(s._2, (a, s._1)))
    def end(s: S) = Applicative[Eff[R, ?]].tuple2(self.end(s._1), sink.end(s._2)).map(_._1)
  }

  /** alias for observeWithState */
  def <<-*(sink: Sink[R, (A, S)]) =
    observeWithState(sink)

  /** observe the current state */
  def observeState(sink: Sink[R, S]) = new Fold[R, A, B] {
    type S = (self.S, sink.S)
    def start = Applicative[Eff[R, ?]].tuple2(self.start , sink.start)
    def fold = (s: S, a: A) => (self.fold(s._1, a), sink.fold(s._2, s._1))
    def end(s: S) = Applicative[Eff[R, ?]].tuple2(self.end(s._1), sink.end(s._2)).map(_._1)
  }

  /** alias for observeState */
  def <-*(sink: Sink[R, S]) =
    observeState(sink)

  /** observe both the input value and the next state */
  def observeWithNextState(sink: Sink[R, (A, S)]) = new Fold[R, A, B] {
    type S = (self.S, sink.S)
    def start = Applicative[Eff[R, ?]].tuple2(self.start , sink.start)
    def fold = (s: S, a: A) => { val next = self.fold(s._1, a); (next, sink.fold(s._2, (a, next))) }
    def end(s: S) = Applicative[Eff[R, ?]].tuple2(self.end(s._1), sink.end(s._2)).map(_._1)
  }

  /** alias for observeWithNextState */
  def <<+*(sink: Sink[R, (A, S)]) =
    observeWithNextState(sink)

  /** observe the next state */
  def observeNextState(sink: Sink[R, S]) = new Fold[R, A, B] {
    type S = (self.S, sink.S)
    def start = Applicative[Eff[R, ?]].tuple2(self.start , sink.start)
    def fold = (s: S, a: A) => { val next = self.fold(s._1, a); (next, sink.fold(s._2, next)) }
    def end(s: S) = Applicative[Eff[R, ?]].tuple2(self.end(s._1), sink.end(s._2)).map(_._1)
  }

  /** alias for observeNextState */
  def <+*(sink: Sink[R, S]) =
    observeNextState(sink)

  /**
   * run a Fold with a Foldable instance
   */
  def run[F[_] : Foldable](foldable: F[A]): Eff[R, B] =
    start.flatMap { s =>
      end(foldable.foldLeft(s)((res, cur) => fold(res, cur)))
    }

  /**
   * run over one element
   */
  def run1(a: A): Eff[R, B] =
    start.flatMap(s => end(fold(s, a)))


  /** pipe the output of this fold into another fold */
  def compose[C](f2: Fold[R, B, C]) = new Fold[R, A, C] {
    type S = Eff[R, (self.S, f2.S)]
    def start = Eff.EffMonad[R].pure(Eff.EffMonad[R].tuple2(self.start, f2.start))

    def fold = (s, a) =>
      s.flatMap { case (f1s, f2s) =>
        self.end(self.fold(f1s, a)).map((u: B) => (self.fold(f1s, a), f2.fold(f2s, u)))
      }

    def end(s: S) = s.flatMap { case (f1s, f2s) =>
      f2.end(f2s)
    }
  }

  /** create a fold that will run this fold repeatedly on input elements and collect all results */
  def nest[F[_], C](f: C => F[A])(implicit monoid: Monoid[B], foldable: Foldable[F]) = new Fold[R, C, B] {
    type S = Eff[R, B]
    def start = Eff.pure(Eff.pure(monoid.zero))
    def fold = (s: S, c: C) =>
      self.run(f(c)).flatMap((b: B) => s.map(s1 => monoid.append(s1, b)))

    def end(s: S) = s
  }

  /** create a fold that will run this fold repeatedly on input elements and collect all results */
  def asFoldable[F[_]](implicit monoid: Monoid[B], foldable: Foldable[F]) =
    nest[F, F[A]](fa => fa)

  /**
   * use a transformation to go from effect stack to another
   */
  def into[U](implicit intoPoly: IntoPoly[R, U]): Fold[U, A, B] { type S = self.S } = new Fold[U, A, B] {
    type S = self.S
    def start = self.start.into[U]
    def fold = (s, a) => self.fold(s, a)
    def end(s: S) = self.end(s).into[U]
  }

  /** equivalent of the as method for functors, added here for easier type inference */
  def as[C](c: =>C) =
    map(_ => c)

  /** equivalent of the void method for functors, added here for easier type inference */
  def void =
    as(())

  def startWith(action: Eff[R, Unit]): Fold[R, A, B] { type S = self.S } = new Fold[R, A, B] {
    type S = self.S
    def start = action >> self.start
    def fold = (s, a) => self.fold(s, a)
    def end(s: S) = self.end(s)
  }

  def endWith(action: Eff[R, Unit]): Fold[R, A, B] { type S = self.S } = new Fold[R, A, B] {
    type S = self.S
    def start = self.start
    def fold = (s, a) => self.fold(s, a)
    def end(s: S) = self.end(s).flatMap(b => action.as(b))
  }
}

object Fold {

  implicit def MonoidSink[R, A]: Monoid[Fold[R, A, Unit]] = new Monoid[Fold[R, A, Unit]] {
    def zero = Folds.fromStart(pure(()))
    def append(s1: Fold[R, A, Unit], s2: =>Fold[R, A, Unit]): Fold[R, A, Unit] = new Fold[R, A, Unit] {
      lazy val s2_ = s2
      type S = (s1.S, s2_.S)
      def start = s1.start.flatMap(s1s => s2_.start.map(s2s => (s1s, s2s)))
      def fold = (s: S, a: A) => (s1.fold(s._1, a), s2_.fold(s._2, a))
      def end(s: S) = s1.end(s._1) >> s2_.end(s._2)
    }
  }

  /**
   * Applicative instance
   *
   * This means that we can write:
   *
   *   val mean: Fold[Int, Int] = (sum |@| count)(_ / _)
   *
   * An Applicative instance is also a Functor instance so we can write:
   *
   *   val meanTimes2 = mean.map(_ * 2)
   */
  implicit def ApplicativeFold[R, T]: Applicative[Fold[R, T, ?]] = new Applicative[Fold[R, T, ?]] {
    type F[U] = Fold[R, T, U]

    def point[A](a: =>A): Fold[R, T, A] =
      new Fold[R, T, A] {
        type S = Unit

        def start: Eff[R, S] = Eff.pure(())
        def fold: (S, T) => S = (s, t) => s
        def end(s: S): Eff[R, A] = Eff.pure(a)
      }

    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      fa map f

    def ap[A, B](fa: =>F[A])(f: =>F[A => B]): F[B] =
      map(fa zip f) { case (a, b) => b(a) }
  }
}

/**
 * Typeclass instances and creation methods for folds
 */
trait Folds {

  /** @return a fold which uses a Monoid to accumulate elements */
  def fromMonoidMap[R, A, M : Monoid](f: A => M): Fold[R, A, M] { type S = M } = new Fold[R, A, M] {
    type S = M
    def start = Eff.pure(Monoid[M].zero)
    def fold = (s: S, a: A) => Monoid[M].append(s, f(a))
    def end(s: S) = Eff.pure(s)
  }

  /** @return a fold from arguments of a fold left */
  def fromFoldLeft[R, A, B](b: B)(f: (B, A) => B): Fold[R, A, B] { type S = B } = new Fold[R, A, B] {
    type S = B
    def start = Eff.pure(b)
    def fold = (s: S, a: A) => f(s, a)
    def end(s: S) = Eff.pure(s)
  }

  /** @return a fold with just a start action */
  def fromStart[R, A, S1](action: Eff[R, S1]) = new Fold[R, A, S1] {
    type S = S1
    def start = action
    def fold = (s: S, a: A) => s
    def end(s: S) = Eff.pure(s)
  }

  def bracket[R :_Safe, A, C](open: Eff[R, C])(step: (C, A) => Eff[R, C])(close: C => Eff[R, Unit]): Fold[R, A, Unit] = new Fold[R, A, Unit] {
    type S = Eff[R, C]
    def start = pure(open)
    def fold = (s: S, a: A) => otherwise(s.flatMap(c => step(c, a)), s.flatMap(close).flatMap(_ => s))
    def end(s: S) = s.flatMap(close)
  }

  def fromSink[R, A](action: A => Eff[R, Unit]): Fold[R, A, Unit] = new Fold[R, A, Unit] {
    type S = Eff[R, Unit]
    def start = pure(pure(()))
    def fold = (s: S, a: A) => s >> action(a)
    def end(s: S) = s.void
  }

  /** @return a Fold which simply accumulates elements into a List */
  def list[A]: Fold[NoFx, A, List[A]] = new Fold[NoFx, A, List[A]] {
    // a ListBuffer is used for efficient appends
    type S = scala.collection.mutable.ListBuffer[A]
    def start = pure(new scala.collection.mutable.ListBuffer[A])
    def fold = (s: S, a: A) => { s.append(a); s }
    def end(s: S) = pure(s.toList)
  }

}

object Folds extends Folds
