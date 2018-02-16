package org.specs2
package control.eff

import fp._

/**
 * A non-empty list of Unions.
 *
 * It is only partially typed, we just keep track of the type of the first object
 */
case class Unions[R, A](first: Union[R, A], rest: Vector[Union[R, Any]]) {
  type X = A

  def size: Int =
    rest.size + 1

  def unions: Vector[Union[R, Any]]=
    first.asInstanceOf[Union[R, Any]] +: rest

  def append[B](others: Unions[R, B]): Unions[R, A] =
    Unions(first, rest ++ others.unions)

  /**
   * create a continuation which will apply the 'map' function
   * if the first effect of this Unions object is interpreted
   */
  def continueWith[B](continuation: Continuation[R, Vector[Any], B]): Continuation[R, A, B] =
    Continuation.lift({ x: X =>
      rest match {
        case v if v.isEmpty =>
          continuation(x +: Vector.empty)

        case h +: t =>
          ImpureAp[R, h.X, B](Unions[R, h.X](h, t), continuation.contramap(x +: _))
      }
    }, continuation.onNone)

  def into[S](f: UnionInto[R, S]): Unions[S, A] =
    Unions[S, A](f(first), rest.map(f.apply))

  /**
   * collect all the M effects and create a continuation for other effects
   * in a stack containing no more M effects
   */
  def project[M[_], U](implicit m: Member.Aux[M, R, U]): CollectedUnions[M, R, U] =
    collect[M, U](m.project)

  /**
   * collect all the M effects and create a continuation for other effects
   * in the same stack
   */
  def extract[M[_]](implicit m: M /= R): CollectedUnions[M, R, R] =
    collect[M, R](u => m.extract(u) match {
      case Some(mx) => Right(mx)
      case None     => Left(u)
    })

  private def collect[M[_], U](collect: Union[R, Any] => Union[U, Any] Either M[Any]): CollectedUnions[M, R, U] = {
    val (effectsAndIndices, othersAndIndices) =
      unions.iterator.zipWithIndex.foldLeft((Vector[(M[Any], Int)](), Vector[(Union[U, Any], Int)]())) {
        case ((es, os), (u, i)) =>
          collect(u) match {
            case Right(mx) => (es :+ ((mx, i)), os)
            case Left(o) => (es, os :+ ((o, i)))
          }
      }

    val (effects, indices) = effectsAndIndices.unzip
    val (otherEffects, otherIndices) = othersAndIndices.unzip

    CollectedUnions[M, R, U](effects, otherEffects, indices, otherIndices)
  }

  def transform[M[_]](nat: M ~> M)(implicit m: M /= R): Unions[R, A] =
    Unions(m.transformUnion(nat)(first), rest.map(m.transformUnion(nat)))

  def transformInto[M[_], N[_], U, S](nat: M ~> N)(implicit m: Member.Aux[M, R, U], n: Member.Aux[N, S, U]): Unions[S, A] =
    Unions[S, A](m.transformUnionInto(nat)(first), rest.map(u => m.transformUnionInto(nat)(u)))
}

object Unions {
  def send[M[_], R, X](mx: M[X])(implicit m: MemberIn[M, R]) =
    Unions[R, X](m.inject(mx), Vector.empty)
}

/**
 * Collection of effects of a given type from a Unions objects
 *
 */
case class CollectedUnions[M[_], R, U](effects: Vector[M[Any]], otherEffects: Vector[Union[U, Any]], indices: Vector[Int], otherIndices: Vector[Int]) {

  def continuation[A](continueWith: Continuation[R, Vector[Any], A], m: Member.Aux[M, R, U]): Continuation[R, Vector[Any], A] =
    otherEffects match {
      case v if v.isEmpty =>
        continueWith

      case o +: rest =>
        Continuation.lift[R, Vector[Any], A](ls =>
          ImpureAp[R, Any, A](Unions(m.accept(o), rest.map(m.accept)), continueWith.contramap(reorder(ls, _))), continueWith.onNone)
    }

  def continuation[A](continueWith: Continuation[U, Vector[Any], A]): Continuation[U, Vector[Any], A] =
    otherEffects match {
      case v if v.isEmpty =>
        continueWith

      case o +: rest =>
        Continuation.lift[U, Vector[Any], A](ls =>
          ImpureAp[U, Any, A](Unions(o, rest), continueWith.contramap(reorder(ls, _)), continueWith.onNone))
    }

  def othersEff[A](continueWith: Continuation[U, Vector[Any], A]): Eff[U, A] =
    otherEffects match {
      case v if v.isEmpty =>
        continueWith(Vector.empty)

      case o +: rest =>
        ImpureAp[U, Any, A](Unions(o, rest), continueWith)
    }

  private def reorder(ls: Vector[Any], xs: Vector[Any]): Vector[Any] =
    (ls.zip(indices) ++ xs.zip(otherIndices)).sortBy(_._2).map(_._1)

}

/**
 * transform a Union for a given stack into a Union for another stack
 */
trait UnionInto[R, S] {

  def apply[A](union: Union[R, A]): Union[S, A]

  /** transform the unions for a given effect */
  def into[A](e: Eff[R, A]): Eff[S, A] =
    e match {
      case Pure(a, last) =>
        Eff.pure(a).addLast(last.interpret(into))

      case Impure(NoEffect(a), c, l) =>
        Impure(NoEffect(a), c.interpretEff(into)(into), l.interpret(into))

      case Impure(u: Union[_, _], c, l) =>
        Impure(apply(u), c.interpretEff(into)(into), l.interpret(into))

      case ImpureAp(unions, continuation, last) =>
        ImpureAp(unions.into(this), continuation.interpretEff(into)(into), last.interpret(into))
    }
}
