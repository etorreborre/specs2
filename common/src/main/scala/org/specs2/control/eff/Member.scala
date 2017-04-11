package org.specs2.control
package eff

import scala.annotation.implicitNotFound
import org.specs2.fp.syntax._
import org.specs2.fp._

@implicitNotFound("No instance found for MemberIn[${T}, ${R}].\nThe effect ${T} is not part of the stack ${R}")
trait MemberIn[T[_], R] { outer =>
  def inject[V](tv: T[V]): Union[R, V]

  def transform[O[_]](implicit from: O ~> T): MemberIn[O, R] = new MemberIn[O, R] {
    def inject[V](ov: O[V]): Union[R, V] =
      outer.inject(from(ov))
  }
}

object MemberIn extends MemberInLower1 {

  @implicitNotFound("No instance found for MemberIn[${T}, ${R}].\nThe effect ${T} is not part of the stack ${R}")
  type |=[T[_], R] = MemberIn[T, R]
}

trait MemberInLower1 extends MemberInLower2 {
  implicit def MemberIn1[T[_]]: MemberIn[T, Fx1[T]] = new MemberIn[T, Fx1[T]] {
    def inject[V](effect: T[V]): Union[Fx1[T], V] =
      Union1(effect)
  }
}

trait MemberInLower2 extends MemberInLower3 {
  implicit def MemberIn2L[L[_], R[_]]: MemberIn[L, Fx2[L, R]] = new MemberIn[L, Fx2[L, R]] {
    def inject[V](effect: L[V]): Union[Fx2[L, R], V] =
      Union2L(effect)
  }

  implicit def MemberIn3L[L[_], M[_], R[_]]: MemberIn[L, Fx3[L, M, R]] = new MemberIn[L, Fx3[L, M, R]] {
    def inject[V](effect: L[V]): Union[Fx3[L, M, R], V] =
      Union3L(effect)
  }

  implicit def MemberInAppendL[T[_], L, R](implicit append: MemberIn[T, L]): MemberIn[T, FxAppend[L, R]] = new MemberIn[T, FxAppend[L, R]] {
    def inject[V](effect: T[V]): Union[FxAppend[L, R], V] =
      UnionAppendL(append.inject(effect))
  }
}

trait MemberInLower3 extends MemberInLower4 {
  implicit def MemberIn2R[L[_], R[_]]: MemberIn[R, Fx2[L, R]] = new MemberIn[R, Fx2[L, R]] {
    def inject[V](effect: R[V]): Union[Fx2[L, R], V] =
      Union2R(effect)
  }

  implicit def MemberIn3M[L[_], M[_], R[_]]: MemberIn[M, Fx3[L, M, R]] = new MemberIn[M, Fx3[L, M, R]] {
    def inject[V](effect: M[V]): Union[Fx3[L, M, R], V] =
      Union3M(effect)
  }

  implicit def MemberInAppendR[T[_], L, R](implicit append: MemberIn[T, R]): MemberIn[T, FxAppend[L, R]] = new MemberIn[T, FxAppend[L, R]] {
    def inject[V](effect: T[V]): Union[FxAppend[L, R], V] =
      UnionAppendR(append.inject(effect))
  }
}

trait MemberInLower4 extends MemberInLower5 {
  implicit def MemberIn3R[L[_], M[_], R[_]]: MemberIn[R, Fx3[L, M, R]] = new MemberIn[R, Fx3[L, M, R]] {
    def inject[V](effect: R[V]): Union[Fx3[L, M, R], V] =
      Union3R(effect)
  }
}

trait MemberInLower5 {
  implicit def MemberInAppendAnyL[T[_], R]: MemberIn[T, FxAppend[Fx1[T], R]] = new MemberIn[T, FxAppend[Fx1[T], R]] {
    def inject[V](effect: T[V]): Union[FxAppend[Fx1[T], R], V] =
      UnionAppendL(Union1(effect))
  }

  implicit def MemberInAppendAnyR[T[_], L, R](implicit m: MemberIn[T, R]): MemberIn[T, FxAppend[L, R]] = new MemberIn[T, FxAppend[L, R]] {
    def inject[V](effect: T[V]): Union[FxAppend[L, R], V] =
      UnionAppendR(m.inject(effect))
  }
}

@implicitNotFound("No instance found for MemberInOut[${T}, ${R}].\nThe effect ${T} is not part of the stack ${R}")
trait MemberInOut[T[_], R] extends MemberIn[T, R] { outer =>
  def extract[V](union: Union[R, V]): Option[T[V]]

  override def transform[O[_]](implicit from: O ~> T): MemberIn[O, R] = new MemberIn[O, R] {
    def inject[V](ov: O[V]): Union[R, V] =
      outer.inject(from(ov))
  }

  def transformUnion[A](nat: T ~> T)(union: Union[R, A]): Union[R, A] =
    extract(union).map(tx => nat(tx)).fold(union)(tx => inject(tx))

}

object MemberInOut extends MemberInOutLower1 {

  @implicitNotFound("No instance found for MemberInOut[${T}, ${R}].\nThe effect ${T} is not part of the stack ${R}")
  type /=[T[_], R] = MemberInOut[T, R]
}

trait MemberInOutLower1 extends MemberInOutLower2 {
  implicit def MemberInOutOut1[T[_]]: MemberInOut[T, Fx1[T]] = new MemberInOut[T, Fx1[T]] {
    def inject[V](effect: T[V]): Union[Fx1[T], V] =
      Union1(effect)

    def extract[V](union: Union[Fx1[T], V]): Option[T[V]] =
      union match {
        case Union1(e) => Option(e)
      }

  }
}

trait MemberInOutLower2 extends MemberInOutLower3 {
  implicit def MemberInOut2L[L[_], R[_]]: MemberInOut[L, Fx2[L, R]] = new MemberInOut[L, Fx2[L, R]] {
    def inject[V](effect: L[V]): Union[Fx2[L, R], V] =
      Union2L(effect)

    def extract[V](union: Union[Fx2[L, R], V]): Option[L[V]] =
      union match {
        case Union2L(e) => Option(e)
        case _          => None
      }
  }

  implicit def MemberInOut3L[L[_], M[_], R[_]]: MemberInOut[L, Fx3[L, M, R]] = new MemberInOut[L, Fx3[L, M, R]] {
    def inject[V](effect: L[V]): Union[Fx3[L, M, R], V] =
      Union3L(effect)

    def extract[V](union: Union[Fx3[L, M, R], V]): Option[L[V]] =
      union match {
        case Union3L(e) => Option(e)
        case _          => None
      }
  }

  implicit def MemberInOutAppendL[T[_], L, R](implicit append: MemberInOut[T, L]): MemberInOut[T, FxAppend[L, R]] = new MemberInOut[T, FxAppend[L, R]] {
    def inject[V](effect: T[V]): Union[FxAppend[L, R], V] =
      UnionAppendL(append.inject(effect))

    def extract[V](union: Union[FxAppend[L, R], V]): Option[T[V]] =
      union match {
        case UnionAppendL(u) => append.extract(u)
        case _               => None
      }
  }
}

trait MemberInOutLower3 extends MemberInOutLower4 {
  implicit def MemberInOut2R[L[_], R[_]]: MemberInOut[R, Fx2[L, R]] = new MemberInOut[R, Fx2[L, R]] {
    def inject[V](effect: R[V]): Union[Fx2[L, R], V] =
      Union2R(effect)

    def extract[V](union: Union[Fx2[L, R], V]): Option[R[V]] =
      union match {
        case Union2R(e) => Option(e)
        case _          => None
      }
  }

  implicit def MemberInOut3M[L[_], M[_], R[_]]: MemberInOut[M, Fx3[L, M, R]] = new MemberInOut[M, Fx3[L, M, R]] {
    def inject[V](effect: M[V]): Union[Fx3[L, M, R], V] =
      Union3M(effect)

    def extract[V](union: Union[Fx3[L, M, R], V]): Option[M[V]] =
      union match {
        case Union3M(e) => Option(e)
        case _          => None
      }
  }

  implicit def MemberInOutAppendR[T[_], L, R](implicit append: MemberInOut[T, R]): MemberInOut[T, FxAppend[L, R]] = new MemberInOut[T, FxAppend[L, R]] {
    def inject[V](effect: T[V]): Union[FxAppend[L, R], V] =
      UnionAppendR(append.inject(effect))

    def extract[V](union: Union[FxAppend[L, R], V]): Option[T[V]] =
      union match {
        case UnionAppendR(u) => append.extract(u)
        case _               => None
      }
  }
}

trait MemberInOutLower4 extends MemberInOutLower5 {
  implicit def MemberInOut3R[L[_], M[_], R[_]]: MemberInOut[R, Fx3[L, M, R]] = new MemberInOut[R, Fx3[L, M, R]] {
    def inject[V](effect: R[V]): Union[Fx3[L, M, R], V] =
      Union3R(effect)

    def extract[V](union: Union[Fx3[L, M, R], V]): Option[R[V]] =
      union match {
        case Union3R(e) => Option(e)
        case _          => None
      }
  }
}

trait MemberInOutLower5 {
  implicit def MemberInOutAppendAnyL[T[_], R]: MemberInOut[T, FxAppend[Fx1[T], R]] = new MemberInOut[T, FxAppend[Fx1[T], R]] {
    def inject[V](effect: T[V]): Union[FxAppend[Fx1[T], R], V] =
      UnionAppendL(Union1(effect))

    def extract[V](union: Union[FxAppend[Fx1[T], R], V]): Option[T[V]] =
      union match {
        case UnionAppendL(Union1(e)) => Option(e)
        case _                       => None
      }
  }

  implicit def MemberInOutAppendAnyR[T[_], L, R](implicit m: MemberInOut[T, R]): MemberInOut[T, FxAppend[L, R]] = new MemberInOut[T, FxAppend[L, R]] {
    def inject[V](effect: T[V]): Union[FxAppend[L, R], V] =
      UnionAppendR(m.inject(effect))

    def extract[V](union: Union[FxAppend[L, R], V]): Option[T[V]] =
      union match {
        case UnionAppendR(u) => m.extract(u)
        case _               => None
      }
  }
}


@implicitNotFound("No instance found for Member[${T}, ${R}].\nThe effect ${T} is not part of the stack ${R}\n or it was not possible to determine the stack that would result from removing ${T} from ${R}")
trait Member[T[_], R] extends MemberInOut[T, R] {
  type Out

  def accept[V](union: Union[Out, V]): Union[R, V]

  def project[V](union: Union[R, V]): Union[Out, V] Either T[V]

  def aux: Member.Aux[T, R, Out] =
    this

  def extract[V](union: Union[R, V]): Option[T[V]] =
    project(union).right.toOption

  def transformUnionInto[N[_], U, S, X](nat: T ~> N)(union: Union[R, X])(implicit n: Member.Aux[N, S, U]): Union[S, X] =
    project(union) match {
      case Right(tv) => n.inject(nat(tv))
      case Left(u)   => n.accept(u.asInstanceOf[Union[n.Out, X]])
    }

}

object Member extends MemberLower1 {

  @implicitNotFound("No instance found for Member[${T}, ${R}].\nThe effect ${T} is not part of the stack ${R}\n or it was not possible to determine the stack that would result from removing ${T} from ${R}")
  type <=[T[_], R] = Member[T, R]

  @implicitNotFound("No instance found for Member[${T}, ${R}].\nThe effect ${T} is not part of the stack ${R}\n or it was not possible to determine the stack that would result from removing ${T} from ${R}")
  type Aux[T[_], R, U] = Member[T, R] { type Out = U }

  def apply[T[_], R](implicit m: Member[T, R]): Member[T, R] =
    m

  def aux[T[_], R, U](implicit m: Member.Aux[T, R, U]): Member.Aux[T, R, U] =
    m

  def unaux[T[_], R, U](implicit m: Member.Aux[T, R, U]): Member[T, R] =
    m
}

trait MemberLower1 extends MemberLower2 {
  implicit def Member1[T[_]]: Member.Aux[T, Fx1[T], NoFx] = new Member[T, Fx1[T]] { outer =>
    type Out = NoFx

    def inject[V](tv: T[V]): Union[Fx1[T], V] =
      Union1[T, V](tv)

    def accept[V](union: Union[Out, V]): Union[Fx1[T], V] =
      sys.error("cannot accept a nil effect as In1")

    def project[V](union: Union[Fx1[T], V]): Union[Out, V] Either T[V] =
      union match {
        case Union1(e) => Right(e)
      }
  }
}

trait MemberLower2 extends MemberLower3 {
  implicit def Member2L[L[_], R[_]]: Member.Aux[L, Fx2[L, R], Fx1[R]] = new Member[L, Fx2[L, R]] { outer =>
    type Out = Fx1[R]

    def inject[V](tv: L[V]): Union[Fx2[L, R], V] =
      Union2L[L, R, V](tv)

    def accept[V](union: Union[Out, V]): Union[Fx2[L, R], V] =
      union match {
        case Union1(e) => Union2R(e)
      }

    def project[V](union: Union[Fx2[L, R], V]): Union[Out, V] Either L[V] =
      union match {
        case Union2L(e) => Right(e)
        case Union2R(e) => Left(Union1[R, V](e))
      }
  }
}

trait MemberLower3 extends MemberLower4 {
  implicit def Member3L[L[_], M[_], R[_]]: Member.Aux[L, Fx3[L, M, R], Fx2[M, R]] = new Member[L, Fx3[L, M, R]] { outer =>
    type Out = Fx2[M, R]

    def inject[V](tv: L[V]): Union[Fx3[L, M, R], V] =
      Union3L[L, M, R, V](tv)

    def accept[V](union: Union[Out, V]): Union[Fx3[L, M, R], V] =
      union match {
        case Union2L(e) => Union3M(e)
        case Union2R(e) => Union3R(e)
      }

    def project[V](union: Union[Fx3[L, M, R], V]): Union[Out, V] Either L[V] =
      union match {
        case Union3L(e) => Right(e)
        case Union3M(e) => Left(Union2L[M, R, V](e))
        case Union3R(e) => Left(Union2R[M, R, V](e))
      }
  }
}

trait MemberLower4 extends MemberLower5 {
  implicit def Member4L[T[_], L[_], M[_], R[_]]: Member.Aux[T, FxAppend[Fx1[T], Fx3[L, M, R]], Fx3[L, M, R]] = new Member[T, FxAppend[Fx1[T], Fx3[L, M, R]]] { outer =>
    type Out = Fx3[L, M, R]

    def inject[V](tv: T[V]): Union[FxAppend[Fx1[T], Out], V] =
      UnionAppendL[Fx1[T], Fx3[L, M, R], V](Union1(tv))

    def accept[V](union: Union[Out, V]): Union[FxAppend[Fx1[T], Fx3[L, M, R]], V] =
      UnionAppendR(union)

    def project[V](union: Union[FxAppend[Fx1[T], Fx3[L, M, R]], V]): Union[Out, V] Either T[V] =
      union match {
        case UnionAppendL(Union1(e)) => Right(e)
        case UnionAppendR(u)         => Left(u)
      }
  }
}

trait MemberLower5 extends MemberLower6 {
  implicit def Member4RL[T[_], L[_], M[_], R[_]]: Member.Aux[L, FxAppend[Fx1[T], Fx3[L, M, R]], Fx3[T, M, R]] = new Member[L, FxAppend[Fx1[T], Fx3[L, M, R]]] { outer =>
    type Out = Fx3[T, M, R]

    def inject[V](l: L[V]): Union[FxAppend[Fx1[T], Fx3[L, M, R]], V] =
      UnionAppendR[Fx1[T], Fx3[L, M, R], V](Union3L(l))

    def accept[V](union: Union[Fx3[T, M, R], V]): Union[FxAppend[Fx1[T], Fx3[L, M, R]], V] =
      union match {
        case Union3L(t) => UnionAppendL(Union1(t))
        case Union3M(m) => UnionAppendR(Union3M(m))
        case Union3R(r) => UnionAppendR(Union3R(r))
      }

    def project[V](union: Union[FxAppend[Fx1[T], Fx3[L, M, R]], V]): Union[Fx3[T, M, R], V] Either L[V] =
      union match {
        case UnionAppendL(Union1(t))  => Left(Union3L(t))
        case UnionAppendR(Union3L(l)) => Right(l)
        case UnionAppendR(Union3M(m)) => Left(Union3M(m))
        case UnionAppendR(Union3R(r)) => Left(Union3R(r))
        case UnionAppendR(_)          => sys.error("appeasing the compiler exhaustiveness checking for Member4RL")
      }
  }
}

trait MemberLower6 extends MemberLower7 {
  implicit def Member4RM[T[_], L[_], M[_], R[_]]: Member.Aux[M, FxAppend[Fx1[T], Fx3[L, M, R]], Fx3[T, L, R]] = new Member[M, FxAppend[Fx1[T], Fx3[L, M, R]]] { outer =>
    type Out = Fx3[T, L, R]

    def inject[V](m: M[V]): Union[FxAppend[Fx1[T], Fx3[L, M, R]], V] =
      UnionAppendR[Fx1[T], Fx3[L, M, R], V](Union3M(m))

    def accept[V](union: Union[Fx3[T, L, R], V]): Union[FxAppend[Fx1[T], Fx3[L, M, R]], V] =
      union match {
        case Union3L(t) => UnionAppendL(Union1(t))
        case Union3M(l) => UnionAppendR(Union3L(l))
        case Union3R(r) => UnionAppendR(Union3R(r))
      }

    def project[V](union: Union[FxAppend[Fx1[T], Fx3[L, M, R]], V]): Union[Fx3[T, L, R], V] Either M[V] =
      union match {
        case UnionAppendL(Union1(t))  => Left(Union3L(t))
        case UnionAppendR(Union3L(l)) => Left(Union3M(l))
        case UnionAppendR(Union3M(m)) => Right(m)
        case UnionAppendR(Union3R(r)) => Left(Union3R(r))
        case UnionAppendR(_)          => sys.error("appeasing the compiler exhaustiveness checking for Member4RM")
      }
  }
}

trait MemberLower7 extends MemberLower8 {
  implicit def Member4RR[T[_], L[_], M[_], R[_]]: Member.Aux[R, FxAppend[Fx1[T], Fx3[L, M, R]], Fx3[T, L, M]] = new Member[R, FxAppend[Fx1[T], Fx3[L, M, R]]] { outer =>
    type Out = Fx3[T, L, M]

    def inject[V](r: R[V]): Union[FxAppend[Fx1[T], Fx3[L, M, R]], V] =
      UnionAppendR[Fx1[T], Fx3[L, M, R], V](Union3R(r))

    def accept[V](union: Union[Fx3[T, L, M], V]): Union[FxAppend[Fx1[T], Fx3[L, M, R]], V] =
      union match {
        case Union3L(t) => UnionAppendL(Union1(t))
        case Union3M(l) => UnionAppendR(Union3L(l))
        case Union3R(m) => UnionAppendR(Union3M(m))
      }

    def project[V](union: Union[FxAppend[Fx1[T], Fx3[L, M, R]], V]): Union[Fx3[T, L, M], V] Either R[V] =
      union match {
        case UnionAppendL(Union1(t))  => Left(Union3L(t))
        case UnionAppendR(Union3L(l)) => Left(Union3M(l))
        case UnionAppendR(Union3M(m)) => Left(Union3R(m))
        case UnionAppendR(Union3R(r)) => Right(r)
        case UnionAppendR(_)          => sys.error("appeasing the compiler exhaustiveness checking for Member4RM")
      }
  }
}

trait MemberLower8 extends MemberLower9 {
  implicit def MemberAppend1L[T[_], R]: Member.Aux[T, FxAppend[Fx1[T], R], R] = new Member[T, FxAppend[Fx1[T], R]] {
    type Out = R

    def inject[V](e: T[V]): Union[FxAppend[Fx1[T], R], V] =
      UnionAppendL(Union1(e))

    def accept[V](union: Union[Out, V]): Union[FxAppend[Fx1[T], R], V] =
      UnionAppendR(union)

    def project[V](union: Union[FxAppend[Fx1[T], R], V]): Union[Out, V] Either T[V] =
      union match {
        case UnionAppendL(Union1(e)) => Right(e)
        case UnionAppendR(u)         => Left(u)
      }
  }
}

trait MemberLower9 extends MemberLower10 {
  implicit def MemberAppend2L[T1[_], T2[_], R]: Member.Aux[T1, FxAppend[Fx2[T1, T2], R], FxAppend[Fx1[T2], R]] =
    Member.MemberAppendL(Member.Member2L)
}

trait MemberLower10 extends MemberLower11 {
  implicit def MemberAppend2R[T1[_], T2[_], R]: Member.Aux[T2, FxAppend[Fx2[T1, T2], R], FxAppend[Fx1[T1], R]] =
    Member.MemberAppendL(Member.Member2R)
}

trait MemberLower11 extends MemberLower12 {
  implicit def MemberAppend3L[T1[_], T2[_], T3[_], R]: Member.Aux[T1, FxAppend[Fx3[T1, T2, T3], R], FxAppend[Fx2[T2, T3], R]] =
    Member.MemberAppendL(Member.Member3L)
}

trait MemberLower12 extends MemberLower13 {
  implicit def MemberAppend3M[T1[_], T2[_], T3[_], R]: Member.Aux[T2, FxAppend[Fx3[T1, T2, T3], R], FxAppend[Fx2[T1, T3], R]] =
    Member.MemberAppendL(Member.Member3M)
}

trait MemberLower13 extends MemberLower14 {
  implicit def MemberAppend3R[T1[_], T2[_], T3[_], R]: Member.Aux[T3, FxAppend[Fx3[T1, T2, T3], R], FxAppend[Fx2[T1, T2], R]] =
    Member.MemberAppendL(Member.Member3R)
}

trait MemberLower14 extends MemberLower15 {
  implicit def MemberAppendL[T[_], L, R, U](implicit append: Member.Aux[T, L, U]): Member.Aux[T, FxAppend[L, R], FxAppend[U, R]] = new Member[T, FxAppend[L, R]] {
    type Out = FxAppend[U, R]

    def inject[V](effect: T[V]): Union[FxAppend[L, R], V] =
      UnionAppendL(append.inject(effect))

    def accept[V](union: Union[Out, V]): Union[FxAppend[L, R], V] =
      union match {
        case UnionAppendL(u) => UnionAppendL(append.accept(u))
        case UnionAppendR(u) => UnionAppendR(u)
      }

    def project[V](union: Union[FxAppend[L, R], V]): Union[Out, V] Either T[V] =
      union match {
        case UnionAppendL(u) => append.project(u).leftMap(UnionAppendL.apply)
        case UnionAppendR(u) => Left(UnionAppendR(u))
      }
  }
}

trait MemberLower15 extends MemberLower16 {
  implicit def Member2R[L[_], R[_]]: Member.Aux[R, Fx2[L, R], Fx1[L]] = new Member[R, Fx2[L, R]] { outer =>
    type Out = Fx1[L]

    def inject[V](tv: R[V]): Union[Fx2[L, R], V] =
      Union2R[L, R, V](tv)

    def accept[V](union: Union[Out, V]): Union[Fx2[L, R], V] =
      union match {
        case Union1(e) => Union2L(e)
      }

    def project[V](union: Union[Fx2[L, R], V]): Union[Out, V] Either R[V] =
      union match {
        case Union2R(e) => Right(e)
        case Union2L(e) => Left(Union1[L, V](e))
      }

  }
}

trait MemberLower16 extends MemberLower17 {
  implicit def Member3M[L[_], M[_], R[_]]: Member.Aux[M, Fx3[L, M, R], Fx2[L, R]] = new Member[M, Fx3[L, M, R]] { outer =>
    type Out = Fx2[L, R]

    def inject[V](tv: M[V]): Union[Fx3[L, M, R], V] =
      Union3M[L, M, R, V](tv)

    def accept[V](union: Union[Out, V]): Union[Fx3[L, M, R], V] =
      union match {
        case Union2L(e) => Union3L(e)
        case Union2R(e) => Union3R(e)
      }

    def project[V](union: Union[Fx3[L, M, R], V]): Union[Out, V] Either M[V] =
      union match {
        case Union3L(e) => Left(Union2L[L, R, V](e))
        case Union3M(e) => Right(e)
        case Union3R(e) => Left(Union2R[L, R, V](e))
      }
  }

  // Specifialized version of MemberAppendR with an existential type for the output stack
  implicit def MemberAppendRNoAux[T[_], L, R](implicit append: Member[T, R]): Member.Aux[T, FxAppend[L, R], FxAppend[L, append.Out]] =
  Member.MemberAppendR[T, L, R, append.Out](append)
}

trait MemberLower17 extends MemberLower18 {

  implicit def MemberAppendR[T[_], L, R, U](implicit append: Member.Aux[T, R, U]): Member.Aux[T, FxAppend[L, R], FxAppend[L, U]] = new Member[T, FxAppend[L, R]] {
    type Out = FxAppend[L, U]

    def inject[V](effect: T[V]): Union[FxAppend[L, R], V] =
      UnionAppendR(append.inject(effect))

    def accept[V](union: Union[Out, V]): Union[FxAppend[L, R], V] =
      union match {
        case UnionAppendL(u) => UnionAppendL(u)
        case UnionAppendR(u) => UnionAppendR(append.accept(u))
      }

    def project[V](union: Union[FxAppend[L, R], V]): Union[Out, V] Either T[V] =
      union match {
        case UnionAppendL(u) => Left(UnionAppendL(u))
        case UnionAppendR(u) => append.project(u).leftMap(UnionAppendR.apply)
      }

  }
}

trait MemberLower18 extends MemberLower19 {
  implicit def Member3R[L[_], M[_], R[_]]: Member.Aux[R, Fx3[L, M, R], Fx2[L, M]] = new Member[R, Fx3[L, M, R]] { outer =>
    type Out = Fx2[L, M]

    def inject[V](tv: R[V]): Union[Fx3[L, M, R], V] =
      Union3R[L, M, R, V](tv)

    def accept[V](union: Union[Out, V]): Union[Fx3[L, M, R], V] =
      union match {
        case Union2L(e) => Union3L(e)
        case Union2R(e) => Union3M(e)
      }

    def project[V](union: Union[Fx3[L, M, R], V]): Union[Out, V] Either R[V] =
      union match {
        case Union3L(e) => Left(Union2L[L, M, V](e))
        case Union3M(e) => Left(Union2R[L, M, V](e))
        case Union3R(e) => Right(e)
      }
  }
}

trait MemberLower19 {
  implicit def MemberAppendNoFx[T[_], R, U](implicit m: Member.Aux[T, R, U]): Member.Aux[T, FxAppend[R, NoFx], U] = new Member[T, FxAppend[R, NoFx]] { outer =>
    type Out = U

    def inject[V](tv: T[V]): Union[FxAppend[R, NoFx], V] =
      UnionAppendL(m.inject(tv))

    def accept[V](union: Union[Out, V]): Union[FxAppend[R, NoFx], V] =
      UnionAppendL(m.accept(union))

    def project[V](union: Union[FxAppend[R, NoFx], V]): Union[Out, V] Either T[V] =
      union match {
        case UnionAppendL(u) => m.project(u)
        case UnionAppendR(u) => sys.error("impossible - there should be no effects on the right side of FxAppend[R, NoFx]")
      }
  }
}
