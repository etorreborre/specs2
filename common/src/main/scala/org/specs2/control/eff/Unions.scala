package org.specs2.control.eff

import scalaz._

/**
 * A non-empty list of Unions.
 *
 * It is only partially typed, we just keep track of the type of the first object
 */
case class Unions[R, A](first: Union[R, A], rest: List[Union[R, Any]]) {
  type X = A

  def size: Int =
    rest.size + 1

  def unions: List[Union[R, Any]]=
    first.asInstanceOf[Union[R, Any]] +: rest

  def append[B](others: Unions[R, B]): Unions[R, A] =
    Unions(first, rest ++ others.unions)

  /**
   * create a continuation which will apply the 'map' function
   * if the first effect of this Unions object is interpreted
   */
  def continueWith[B](continuation: Arrs[R, List[Any], B]): Arrs[R, A, B] =
  Arrs.singleton { (x: X) =>
    rest match {
      case Nil    => continuation(x :: Nil)
      case h :: t => ImpureAp[R, h.X, B](Unions[R, h.X](h, t), Arrs.singleton((ys: List[Any]) => continuation(x :: ys)))
    }
  }

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
      unions.zipWithIndex.foldLeft((Vector[(M[Any], Int)](), Vector[(Union[U, Any], Int)]())) {
        case ((es, os), (u, i)) =>
          collect(u) match {
            case Right(mx) => (es :+ ((mx, i)), os)
            case Left(o) => (es, os :+ ((o, i)))
          }
      }

    val (effects, indices) = effectsAndIndices.toList.unzip
    val (otherEffects, otherIndices) = othersAndIndices.toList.unzip

    CollectedUnions[M, R, U](effects, otherEffects, indices, otherIndices)
  }

  def transform[M[_]](nat: M ~> M)(implicit m: M /= R): Unions[R, A] =
    Unions(m.transformUnion(nat)(first), rest.map(m.transformUnion(nat)))

  def transformInto[M[_], N[_], U, S](nat: M ~> N)(implicit m: Member.Aux[M, R, U], n: Member.Aux[N, S, U]): Unions[S, A] =
    Unions[S, A](m.transformUnionInto(nat)(first), rest.map(u => m.transformUnionInto(nat)(u)))
}

/**
 * Collection of effects of a given type from a Unions objects
 *
 */
case class CollectedUnions[M[_], R, U](effects: List[M[Any]], otherEffects: List[Union[U, Any]], indices: List[Int], otherIndices: List[Int]) {
  def continuation[A](continueWith: List[Any] => Eff[R, A], m: Member.Aux[M, R, U]): Arrs[R, List[Any], A] =
    otherEffects match {
      case Nil       => Arrs.singleton[R, List[Any], A](ls => continueWith(ls))
      case o :: rest => Arrs.singleton[R, List[Any], A](ls => ImpureAp[R, Any, A](Unions(m.accept(o), rest.map(m.accept)), Arrs.singleton(xs => continueWith(reorder(ls, xs)))))
    }

  def continuation[A](continueWith: Arrs[U, List[Any], A]): Arrs[U, List[Any], A] =
    otherEffects match {
      case Nil       => Arrs.singleton[U, List[Any], A](ls => continueWith(ls))
      case o :: rest => Arrs.singleton[U, List[Any], A](ls => ImpureAp[U, Any, A](Unions(o, rest), Arrs.singleton(xs => continueWith(reorder(ls, xs)))))
    }

  def othersEff[A](continueWith: Arrs[U, List[Any], A]): Eff[U, A] =
    otherEffects match {
      case Nil       => continueWith(Nil)
      case o :: rest => ImpureAp[U, Any, A](Unions(o, rest), Arrs.singleton(ls => continueWith(ls)))
    }

  private def reorder(ls: List[Any], xs: List[Any]): List[Any] =
    (ls.zip(indices) ++ xs.zip(otherIndices)).sortBy(_._2).map(_._1)

}

trait UnionInto[R, S] {
  def apply[A](union: Union[R, A]): Union[S, A]
}
