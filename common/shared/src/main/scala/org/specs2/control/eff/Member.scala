package org.specs2
package control.eff

import fp._
import fp.syntax._

import scala.annotation.{implicitNotFound, switch}

@implicitNotFound("No instance found for MemberIn[${T}, ${R}].\nThe effect ${T} is not part of the stack ${R}")
trait MemberIn[T[_], R] { outer =>

  def inject[V](tv: T[V]): Union[R, V]

  final def transform[O[_]](implicit from: O ~> T): MemberIn[O, R] = new MemberIn[O, R] {
    def inject[V](ov: O[V]): Union[R, V] =
      outer.inject(from(ov))
  }
}

final case class TaggedMemberIn[T[_], R](tag: Int) extends MemberIn[T, R] {
  def inject[V](tv: T[V]): Union[R, V] = UnionTagged(tv, tag)
}

final case class AppendMemberIn[T[_], L, R, X](isRight: Boolean, member: MemberIn[T, X]) extends MemberIn[T, FxAppend[L, R]] {
  def inject[V](tv: T[V]): Union[FxAppend[L, R], V] =
    (if (isRight) Union.appendR(member.inject(tv)) else Union.appendL(member.inject(tv))).asInstanceOf[Union[FxAppend[L, R], V]]
}

object MemberIn extends MemberInLower1 {
  @implicitNotFound("No instance found for MemberIn[${T}, ${R}].\nThe effect ${T} is not part of the stack ${R}")
  type |=[T[_], R] = MemberIn[T, R]
}

trait MemberInLower1 extends MemberInLower2 {
  implicit def MemberIn1[T[_]]: MemberIn[T, Fx1[T]] =
    TaggedMemberIn(1)
}

trait MemberInLower2 extends MemberInLower3 {
  implicit def MemberIn2L[L[_], R[_]]: MemberIn[L, Fx2[L, R]] =
    TaggedMemberIn(1)

  implicit def MemberIn3L[L[_], M[_], R[_]]: MemberIn[L, Fx3[L, M, R]] =
    TaggedMemberIn(1)

  implicit def MemberInAppendL[T[_], L, R](implicit append: MemberIn[T, L]): MemberIn[T, FxAppend[L, R]] =
    AppendMemberIn(isRight = false, append)
}

trait MemberInLower3 extends MemberInLower4 {
  implicit def MemberIn2R[L[_], R[_]]: MemberIn[R, Fx2[L, R]] =
    TaggedMemberIn(2)

  implicit def MemberIn3M[L[_], M[_], R[_]]: MemberIn[M, Fx3[L, M, R]] =
    TaggedMemberIn(2)

  implicit def MemberInAppendR[T[_], L, R](implicit append: MemberIn[T, R]): MemberIn[T, FxAppend[L, R]] =
    AppendMemberIn(isRight = true, append)
}

trait MemberInLower4 extends MemberInLower5 {
  implicit def MemberIn3R[L[_], M[_], R[_]]: MemberIn[R, Fx3[L, M, R]] =
    TaggedMemberIn(3)
}

trait MemberInLower5 {
  implicit def MemberInAppendAnyR[T[_], L, R](implicit m: MemberIn[T, R]): MemberIn[T, FxAppend[L, R]] =
    AppendMemberIn(isRight = true, m)

}

@implicitNotFound("No instance found for MemberInOut[${T}, ${R}].\nThe effect ${T} is not part of the stack ${R} or cannot be extracted from ${R}")
trait MemberInOut[T[_], R] extends MemberIn[T, R] { outer =>
  def extract[V](union: Union[R, V]): Option[T[V]]

  final def transform[O[_]](implicit to: T ~> O, from: O ~> T): MemberInOut[O, R] =
    new MemberInOut[O, R] {
      def inject[V](ov: O[V]): Union[R, V] =
        outer.inject(from(ov))

      def extract[V](union: Union[R, V]): Option[O[V]] =
        outer.extract(union).map(to.apply)
    }

  final def transformUnion[A](nat: T ~> T)(union: Union[R, A]): Union[R, A] =
    extract(union).map(tx => nat(tx)).fold(union)(tx => inject(tx))

  def toMember: Member.Aux[T, R, R] = new Member[T, R] {
    type Out = R

    def inject[V](v: T[V]): Union[R, V] =
      outer.inject(v)

    def accept[V](union: Union[Out, V]): Union[R, V] =
      union

    def project[V](union: Union[R, V]): Union[Out, V] Either T[V] =
      outer.extract(union).map(Right.apply).getOrElse(Left(union))
  }
}

final case class TaggedMemberInOut[T[_], R](tag: Int) extends MemberInOut[T, R] {
  def extract[V](union: Union[R, V]): Option[T[V]] = {
    val tagged = union.tagged
    if (tagged.index == tag) Some(tagged.valueUnsafe.asInstanceOf[T[V]])
    else None
  }

  def inject[V](tv: T[V]): Union[R, V] =
    UnionTagged(tv, tag)
}

final case class AppendMemberInOut[T[_], L, R, X](isRight: Boolean, append: MemberInOut[T, X]) extends MemberInOut[T, FxAppend[L, R]] {
  def extract[V](union: Union[FxAppend[L, R], V]): Option[T[V]] =
    union match {
      case UnionAppendR(r) if isRight => append.extract(r.asInstanceOf[Union[X, V]])
      case UnionAppendL(l) if !isRight => append.extract(l.asInstanceOf[Union[X, V]])
      case _ => None
    }

  def inject[V](tv: T[V]): Union[FxAppend[L, R], V] =
    if (isRight) UnionAppendR(append.inject(tv)).asInstanceOf[Union[FxAppend[L, R], V]]
    else UnionAppendL(append.inject(tv)).asInstanceOf[Union[FxAppend[L, R], V]]
}

object MemberInOut extends MemberInOutLower1 {

  @implicitNotFound("No instance found for MemberInOut[${T}, ${R}].\nThe effect ${T} is not part of the stack ${R} or cannot be extracted from ${R}")
  type /=[T[_], R] = MemberInOut[T, R]
}

trait MemberInOutLower1 extends MemberInOutLower2 {
  implicit def MemberInOutOut1[T[_]]: MemberInOut[T, Fx1[T]] =
    TaggedMemberInOut(1)
}

trait MemberInOutLower2 extends MemberInOutLower3 {
  implicit def MemberInOut2L[L[_], R[_]]: MemberInOut[L, Fx2[L, R]] =
    TaggedMemberInOut(1)

  implicit def MemberInOut3L[L[_], M[_], R[_]]: MemberInOut[L, Fx3[L, M, R]] =
    TaggedMemberInOut(1)

  implicit def MemberInOutAppendL[T[_], L, R](implicit append: MemberInOut[T, L]): MemberInOut[T, FxAppend[L, R]] =
    AppendMemberInOut(isRight = false, append)
}

trait MemberInOutLower3 extends MemberInOutLower4 {
  implicit def MemberInOut2R[L[_], R[_]]: MemberInOut[R, Fx2[L, R]] =
    TaggedMemberInOut(2)

  implicit def MemberInOut3M[L[_], M[_], R[_]]: MemberInOut[M, Fx3[L, M, R]] =
    TaggedMemberInOut(2)

  implicit def MemberInOutAppendR[T[_], L, R](implicit append: MemberInOut[T, R]): MemberInOut[T, FxAppend[L, R]] =
    AppendMemberInOut(isRight = true, append)
}

trait MemberInOutLower4 extends MemberInOutLower5 {
  implicit def MemberInOut3R[L[_], M[_], R[_]]: MemberInOut[R, Fx3[L, M, R]] =
    TaggedMemberInOut(3)
}

trait MemberInOutLower5 {
  implicit def MemberInOutAppendAnyR[T[_], L, R](implicit m: MemberInOut[T, R]): MemberInOut[T, FxAppend[L, R]] =
    AppendMemberInOut(isRight = true, m)
}


@implicitNotFound("No instance found for Member[${T}, ${R}].\nThe effect ${T} is not part of the stack ${R}\n or it was not possible to determine the stack that would result from removing ${T} from ${R}")
trait Member[T[_], R] extends MemberInOut[T, R] {
  type Out

  def accept[V](union: Union[Out, V]): Union[R, V]

  def project[V](union: Union[R, V]): Union[Out, V] Either T[V]

  def aux: Member.Aux[T, R, Out] =
    this

  def extract[V](union: Union[R, V]): Option[T[V]] =
    project(union).toOption

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
      Union.one[T, V](tv)

    def accept[V](union: Union[Out, V]): Union[Fx1[T], V] =
      sys.error("cannot accept a nil effect as In1")

    def project[V](union: Union[Fx1[T], V]): Union[Out, V] Either T[V] =
      Right(union.tagged.valueUnsafe.asInstanceOf[T[V]])
  }
}

trait MemberLower2 extends MemberLower3 {
  implicit def Member2L[L[_], R[_]]: Member.Aux[L, Fx2[L, R], Fx1[R]] = new Member[L, Fx2[L, R]] { outer =>
    type Out = Fx1[R]

    def inject[V](tv: L[V]): Union[Fx2[L, R], V] =
      Union.twoL(tv)

    def accept[V](union: Union[Out, V]): Union[Fx2[L, R], V] =
      union.tagged.increment

    def project[V](union: Union[Fx2[L, R], V]): Union[Out, V] Either L[V] =
      if (union.tagged.index == 1) Right(union.tagged.valueUnsafe.asInstanceOf[L[V]])
      else                         Left(union.tagged.decrement)
  }
}

trait MemberLower3 extends MemberLower4 {
  implicit def Member3L[L[_], M[_], R[_]]: Member.Aux[L, Fx3[L, M, R], Fx2[M, R]] = new Member[L, Fx3[L, M, R]] { outer =>
    type Out = Fx2[M, R]

    def inject[V](tv: L[V]): Union[Fx3[L, M, R], V] =
      Union.threeL(tv)

    def accept[V](union: Union[Out, V]): Union[Fx3[L, M, R], V] =
      union.tagged.increment

    def project[V](union: Union[Fx3[L, M, R], V]): Union[Out, V] Either L[V] =
      if (union.tagged.index == 1) Right(union.tagged.valueUnsafe.asInstanceOf[L[V]])
      else                         Left(union.tagged.decrement.asInstanceOf[Union[Out, V]])
  }
}

trait MemberLower4 extends MemberLower5 {
  implicit def Member4L[T[_], L[_], M[_], R[_]]: Member.Aux[T, FxAppend[Fx1[T], Fx3[L, M, R]], Fx3[L, M, R]] = new Member[T, FxAppend[Fx1[T], Fx3[L, M, R]]] { outer =>
    type Out = Fx3[L, M, R]

    def inject[V](tv: T[V]): Union[FxAppend[Fx1[T], Out], V] =
      Union.appendL(Union.one(tv))

    def accept[V](union: Union[Out, V]): Union[FxAppend[Fx1[T], Fx3[L, M, R]], V] =
      Union.appendR(union)

    def project[V](union: Union[FxAppend[Fx1[T], Fx3[L, M, R]], V]): Union[Out, V] Either T[V] =
      union match {
        case UnionAppendL(l) => Right(l.tagged.valueUnsafe.asInstanceOf[T[V]])
        case UnionAppendR(r) => Left(r)
        case _               => sys.error("impossible")
      }
  }
}

trait MemberLower5 extends MemberLower6 {
  implicit def Member4RL[T[_], L[_], M[_], R[_]]: Member.Aux[L, FxAppend[Fx1[T], Fx3[L, M, R]], Fx3[T, M, R]] = new Member[L, FxAppend[Fx1[T], Fx3[L, M, R]]] { outer =>
    type Out = Fx3[T, M, R]

    def inject[V](l: L[V]): Union[FxAppend[Fx1[T], Fx3[L, M, R]], V] =
      Union.appendR(Union.threeL(l))

    def accept[V](union: Union[Fx3[T, M, R], V]): Union[FxAppend[Fx1[T], Fx3[L, M, R]], V] = {
      val tagged = union.tagged
      if (tagged.index == 1) Union.appendL(tagged.forget)
      else                   Union.appendR(tagged.forget)
    }

    def project[V](union: Union[FxAppend[Fx1[T], Fx3[L, M, R]], V]): Union[Fx3[T, M, R], V] Either L[V] =
      union match {
        case UnionAppendL(l) => Left(l.forget)
        case UnionAppendR(r) =>
          val tagged = r.tagged
          (tagged.index: @switch) match {
            case 2 | 3 => Left(tagged.forget)
            case 1 => Right(tagged.valueUnsafe.asInstanceOf[L[V]])
          }
        case UnionTagged(_, _) => sys.error("impossible")
      }
  }
}

trait MemberLower6 extends MemberLower7 {
  implicit def Member4RM[T[_], L[_], M[_], R[_]]: Member.Aux[M, FxAppend[Fx1[T], Fx3[L, M, R]], Fx3[T, L, R]] = new Member[M, FxAppend[Fx1[T], Fx3[L, M, R]]] { outer =>
    type Out = Fx3[T, L, R]

    def inject[V](m: M[V]): Union[FxAppend[Fx1[T], Fx3[L, M, R]], V] =
      Union.appendR(Union.threeM(m))

    def accept[V](union: Union[Fx3[T, L, R], V]): Union[FxAppend[Fx1[T], Fx3[L, M, R]], V] = {
      val tagged = union.tagged
      (tagged.index: @switch) match {
        case 1 => UnionAppendL(tagged.forget)
        case 2 => UnionAppendR(tagged.decrement)
        case 3 => UnionAppendR(tagged.forget)
      }
    }

    def project[V](union: Union[FxAppend[Fx1[T], Fx3[L, M, R]], V]): Union[Fx3[T, L, R], V] Either M[V] = union match {
      case UnionAppendL(l) => Left(l.forget)
      case UnionAppendR(r) =>
        val tagged = r.tagged
        (tagged.index: @switch) match {
          case 1 => Left(tagged.increment)
          case 2 => Right(tagged.valueUnsafe.asInstanceOf[M[V]])
          case 3 => Left(tagged.forget)
        }
      case UnionTagged(_, _) => sys.error("impossible")
    }
  }
}

trait MemberLower7 extends MemberLower8 {
  implicit def Member4RR[T[_], L[_], M[_], R[_]]: Member.Aux[R, FxAppend[Fx1[T], Fx3[L, M, R]], Fx3[T, L, M]] = new Member[R, FxAppend[Fx1[T], Fx3[L, M, R]]] { outer =>
    type Out = Fx3[T, L, M]

    def inject[V](r: R[V]): Union[FxAppend[Fx1[T], Fx3[L, M, R]], V] =
      Union.appendR(Union.threeR(r))

    def accept[V](union: Union[Fx3[T, L, M], V]): Union[FxAppend[Fx1[T], Fx3[L, M, R]], V] = {
      val tagged = union.tagged
      if (tagged.index == 1) UnionAppendL(tagged.forget)
      else                   UnionAppendR(tagged.decrement)
    }

    def project[V](union: Union[FxAppend[Fx1[T], Fx3[L, M, R]], V]): Union[Fx3[T, L, M], V] Either R[V] = union match {
      case UnionAppendL(l) => Left(l.forget)
      case UnionAppendR(r) =>
        val tagged = r.tagged
        if (tagged.index == 3) Right(tagged.valueUnsafe.asInstanceOf[R[V]])
        else                   Left(tagged.increment)
      case UnionTagged(_, _) => sys.error("impossible")
    }
  }
}

trait MemberLower8 extends MemberLower9 {
  implicit def MemberAppend1R[T[_], R]: Member.Aux[T, FxAppend[Fx1[T], R], R] = new Member[T, FxAppend[Fx1[T], R]] {
    type Out = R

    def inject[V](e: T[V]): Union[FxAppend[Fx1[T], R], V] =
      Union.appendL(Union.one(e))

    def project[V](union: Union[FxAppend[Fx1[T], R], V]): Union[Out, V] Either T[V] = union match {
      case UnionAppendR(r)   => Left(r)
      case UnionAppendL(l)   => Right(l.tagged.valueUnsafe.asInstanceOf[T[V]])
      case UnionTagged(_, _) => sys.error("impossible")
    }

    def accept[V](union: Union[Out, V]): Union[FxAppend[Fx1[T], R], V] =
      Union.appendR(union)
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
      Union.appendL(append.inject(effect))

    def accept[V](union: Union[Out, V]): Union[FxAppend[L, R], V] = union match {
      case UnionAppendL(l) => Union.appendL(append.accept(l))
      case _ => union.forget
    }

    def project[V](union: Union[FxAppend[L, R], V]): Union[Out, V] Either T[V] = union match {
      case UnionAppendL(r)   => append.project(r).left.map(Union.appendL)
      case UnionAppendR(_)   => Left(union.forget)
      case UnionTagged(_, _) => sys.error("impossible")
    }
  }
}

trait MemberLower15 extends MemberLower16 {
  implicit def Member2R[L[_], R[_]]: Member.Aux[R, Fx2[L, R], Fx1[L]] = new Member[R, Fx2[L, R]] { outer =>
    type Out = Fx1[L]

    def inject[V](tv: R[V]): Union[Fx2[L, R], V] =
      Union.twoR(tv)

    def accept[V](union: Union[Out, V]): Union[Fx2[L, R], V] =
      UnionTagged(union.asInstanceOf[UnionTagged[Fx2[L, R], V]].valueUnsafe, 1)

    def project[V](union: Union[Fx2[L, R], V]): Union[Out, V] Either R[V] = {
      val tagged = union.tagged
      if (tagged.index == 2) Right(tagged.valueUnsafe.asInstanceOf[R[V]])
      else                   Left(tagged.forget)
    }

  }
}

trait MemberLower16 extends MemberLower17 {
  implicit def Member3M[L[_], M[_], R[_]]: Member.Aux[M, Fx3[L, M, R], Fx2[L, R]] = new Member[M, Fx3[L, M, R]] { outer =>
    type Out = Fx2[L, R]

    def inject[V](tv: M[V]): Union[Fx3[L, M, R], V] =
      Union.threeM(tv)

    def accept[V](union: Union[Out, V]): Union[Fx3[L, M, R], V] =
      union match {
        case tagged@UnionTagged(value, index) =>
          if (index == 1) tagged.forget
          else            UnionTagged(value, 3)
      }

    def project[V](union: Union[Fx3[L, M, R], V]): Union[Out, V] Either M[V] = union match {
      case UnionTagged(value, index) => (index: @switch) match {
        case 1 => Left(union.forget)
        case 2 => Right(value.asInstanceOf[M[V]])
        case 3 => Left(UnionTagged(value, 2))
      }
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
        case UnionAppendL(u)   => UnionAppendL(u)
        case UnionAppendR(u)   => UnionAppendR(append.accept(u))
        case UnionTagged(_, _) => sys.error("impossible")
      }

    def project[V](union: Union[FxAppend[L, R], V]): Union[Out, V] Either T[V] =
      union match {
        case UnionAppendL(u)   => Left(UnionAppendL(u))
        case UnionAppendR(u)   => append.project(u).leftMap(UnionAppendR.apply)
        case UnionTagged(_, _) => sys.error("impossible")
      }

  }
}

trait MemberLower18 extends MemberLower19 {
  implicit def Member3R[L[_], M[_], R[_]]: Member.Aux[R, Fx3[L, M, R], Fx2[L, M]] = new Member[R, Fx3[L, M, R]] { outer =>
    type Out = Fx2[L, M]

    def inject[V](tv: R[V]): Union[Fx3[L, M, R], V] =
      Union.threeR(tv)

    def accept[V](union: Union[Out, V]): Union[Fx3[L, M, R], V] =
      union.forget

    def project[V](union: Union[Fx3[L, M, R], V]): Union[Out, V] Either R[V] = union match {
      case tagged@UnionTagged(value, index) =>
        if (index == 3) Right(value.asInstanceOf[R[V]])
        else            Left(tagged.forget)
    }
  }
}

trait MemberLower19 {
  implicit def MemberAppendNoFxR[T[_], R, U](implicit m: Member.Aux[T, R, U]): Member.Aux[T, FxAppend[R, NoFx], U] = new Member[T, FxAppend[R, NoFx]] { outer =>
    type Out = U

    def inject[V](tv: T[V]): Union[FxAppend[R, NoFx], V] =
      Union.appendL(m.inject(tv))

    def accept[V](union: Union[Out, V]): Union[FxAppend[R, NoFx], V] =
      Union.appendL(m.accept(union))

    def project[V](union: Union[FxAppend[R, NoFx], V]): Union[Out, V] Either T[V] =
      m.project(union.asInstanceOf[UnionAppendL[R, NoFx, V]].value)
  }

  implicit def MemberAppendNoFxL[T[_], R, U](implicit m: Member.Aux[T, R, U]): Member.Aux[T, FxAppend[NoFx, R], U] = new Member[T, FxAppend[NoFx, R]] { outer =>
    type Out = U

    def inject[V](tv: T[V]): Union[FxAppend[NoFx, R], V] =
      Union.appendR(m.inject(tv))

    def accept[V](union: Union[Out, V]): Union[FxAppend[NoFx, R], V] =
      Union.appendR(m.accept(union))

    def project[V](union: Union[FxAppend[NoFx, R], V]): Union[Out, V] Either T[V] =
      m.project(union.asInstanceOf[UnionAppendR[NoFx, R, V]].value)
  }

}
