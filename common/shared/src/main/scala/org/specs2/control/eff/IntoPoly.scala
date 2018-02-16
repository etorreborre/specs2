package org.specs2.control.eff

/**
 * Typeclass proving that it is possible to send a tree of effects R into another tree of effects U
 *
 * for example
 *
 *  send[Option1, Fx.fx3[Option1, Option2, Option3], Int](Option1(1)).
 *    into[Fx.fx5[Option1, Option2, Option3, Option4, Option5]]
 *
 *  should work because all the effects of the first stack are present in the second
 *
 * Note: some implicit definitions are probably missing in some cases
 */
trait IntoPoly[R, U] {
  val unionInto: UnionInto[R, U]

  def apply[A](e: Eff[R, A]): Eff[U, A] =
    unionInto.into(e)
}

object IntoPoly extends IntoPolyLower1

trait IntoPolyLower1 extends IntoPolyLower2 {

  implicit def intoNil[R]: IntoPoly[NoFx, R] =
    new IntoPoly[NoFx, R] {
      val unionInto: UnionInto[NoFx, R] = new UnionInto[NoFx, R] {
        def apply[A](union: Union[NoFx, A]): Union[R, A] =
          sys.error("impossible NoFx into R is only for pure values")
      }
    }

  implicit def intoNoFxAppendL[R]: IntoPoly[FxAppend[NoFx, R], R] =
    new IntoPoly[FxAppend[NoFx, R], R] {
      val unionInto: UnionInto[FxAppend[NoFx, R], R] = new UnionInto[FxAppend[NoFx, R], R] {
        def apply[A](union: Union[FxAppend[NoFx, R], A]): Union[R, A] =
          union match {
            case UnionAppendR(r)   => r
            case UnionAppendL(l)   => sys.error("impossible - intoNoFxAppendL for UnionAppendL")
            case UnionTagged(_, _) => sys.error("impossible - intoNoFxAppendL for UnionTagged")
          }
      }
    }

  implicit def intoNoFxAppendR[R]: IntoPoly[FxAppend[R, NoFx], R] =
    new IntoPoly[FxAppend[R, NoFx], R] {
      val unionInto: UnionInto[FxAppend[R, NoFx], R] = new UnionInto[FxAppend[R, NoFx], R] {
        def apply[A](union: Union[FxAppend[R, NoFx], A]): Union[R, A] =
          union match {
            case UnionAppendL(l)   => l
            case UnionAppendR(r)   => sys.error("impossible - intoNoFxAppendR for UnionAppendR")
            case UnionTagged(_, _) => sys.error("impossible - intoNoFxAppendR for UnionTagged")
          }
      }
    }

  implicit def intoSelf[R]: IntoPoly[R, R] =
    new IntoPoly[R, R] {
      val unionInto: UnionInto[R, R] =
        new UnionInto[R, R] {
          def apply[A](union: Union[R, A]): Union[R, A] =
            union
        }
    }

}

trait IntoPolyLower2  extends IntoPolyLower3 {

  implicit def intoAppendL2L[T1[_], T2[_], R]: IntoPoly[FxAppend[Fx1[T2], R], FxAppend[Fx2[T1, T2], R]] =
    new IntoPoly[FxAppend[Fx1[T2], R], FxAppend[Fx2[T1, T2], R]] {
      val unionInto: UnionInto[FxAppend[Fx1[T2], R], FxAppend[Fx2[T1, T2], R]]  =
        new UnionInto[FxAppend[Fx1[T2], R], FxAppend[Fx2[T1, T2], R]] {
          def apply[X](union: Union[FxAppend[Fx1[T2], R], X]) = union match {
            case UnionAppendL(l)   => UnionAppendL(l.tagged.increment)
            case UnionAppendR(r)   => UnionAppendR(r)
            case UnionTagged(_, _) => sys.error("impossible - intoAppendL2L for UnionTagged")
          }
        }
    }

  implicit def intoAppendL2R[T1[_], T2[_], R]: IntoPoly[FxAppend[Fx1[T1], R], FxAppend[Fx2[T1, T2], R]] =
    new IntoPoly[FxAppend[Fx1[T1], R], FxAppend[Fx2[T1, T2], R]] {
      val unionInto: UnionInto[FxAppend[Fx1[T1], R], FxAppend[Fx2[T1, T2], R]] =
        new UnionInto[FxAppend[Fx1[T1], R], FxAppend[Fx2[T1, T2], R]] {
          def apply[X](union: Union[FxAppend[Fx1[T1], R], X]) = union match {
            case UnionAppendL(l)   => UnionAppendL(l.forget)
            case UnionAppendR(r)   => UnionAppendR(r)
            case UnionTagged(_, _) => sys.error("impossible - intoAppendL2R for UnionTagged")
          }
        }
    }

  implicit def intoAppendL3L[T1[_], T2[_], T3[_], R]: IntoPoly[FxAppend[Fx2[T2, T3], R], FxAppend[Fx3[T1, T2, T3], R]] =
    new IntoPoly[FxAppend[Fx2[T2, T3], R], FxAppend[Fx3[T1, T2, T3], R]] {
      val unionInto: UnionInto[FxAppend[Fx2[T2, T3], R], FxAppend[Fx3[T1, T2, T3], R]] =
        new UnionInto[FxAppend[Fx2[T2, T3], R], FxAppend[Fx3[T1, T2, T3], R]] {
          def apply[X](union: Union[FxAppend[Fx2[T2, T3], R], X]) = union match {
            case UnionAppendL(l)   => UnionAppendL(l.tagged.increment)
            case UnionAppendR(r)   => UnionAppendR(r)
            case UnionTagged(_, _) => sys.error("impossible - intoAppendL3L for UnionTagged")
          }
        }
    }

  implicit def intoAppendL3M[T1[_], T2[_], T3[_], R]: IntoPoly[FxAppend[Fx2[T1, T3], R], FxAppend[Fx3[T1, T2, T3], R]] =
    new IntoPoly[FxAppend[Fx2[T1, T3], R], FxAppend[Fx3[T1, T2, T3], R]] {
      val unionInto: UnionInto[FxAppend[Fx2[T1, T3], R], FxAppend[Fx3[T1, T2, T3], R]] =
        new UnionInto[FxAppend[Fx2[T1, T3], R], FxAppend[Fx3[T1, T2, T3], R]] {
          def apply[X](union: Union[FxAppend[Fx2[T1, T3], R], X]) = union match {
            case UnionAppendL(l)   =>
              if (l.tagged.index == 1) UnionAppendL(l.tagged.forget)
              else                     UnionAppendL(l.tagged.increment)
            case UnionAppendR(r)   => UnionAppendR(r)
            case UnionTagged(_, _) => sys.error("impossible - intoAppendL3M for UnionTagged")
          }
        }
    }

  implicit def intoAppendL3R[T1[_], T2[_], T3[_], R]: IntoPoly[FxAppend[Fx2[T1, T2], R], FxAppend[Fx3[T1, T2, T3], R]] =
    new IntoPoly[FxAppend[Fx2[T1, T2], R], FxAppend[Fx3[T1, T2, T3], R]] {
      val unionInto: UnionInto[FxAppend[Fx2[T1, T2], R], FxAppend[Fx3[T1, T2, T3], R]] =
        new UnionInto[FxAppend[Fx2[T1, T2], R], FxAppend[Fx3[T1, T2, T3], R]] {
          def apply[X](union: Union[FxAppend[Fx2[T1, T2], R], X]) = union match {
            case UnionAppendL(l)   => UnionAppendL(l.forget)
            case UnionAppendR(r)   => UnionAppendR(r)
            case UnionTagged(_, _) => sys.error("impossible - intoAppendL3R for UnionTagged")
          }
        }
    }
}

trait IntoPolyLower3 extends IntoPolyLower4 {
  implicit def intoAppendL1[T[_], R]: IntoPoly[R, FxAppend[Fx1[T], R]] =
    new IntoPoly[R, FxAppend[Fx1[T], R]] {
      val unionInto: UnionInto[R, FxAppend[Fx1[T], R]] =
        new UnionInto[R, FxAppend[Fx1[T], R]] {
          def apply[X](union: Union[R, X]): Union[FxAppend[Fx1[T], R], X] =
            UnionAppendR(union)
        }
    }
}

trait IntoPolyLower4 extends IntoPolyLower5 {

  implicit def into[T[_], R, U, S](implicit
                                   t: Member.Aux[T, R, S],
                                   m: T |= U,
                                   recurse: IntoPoly[S, U]): IntoPoly[R, U] =
    new IntoPoly[R, U] {
      val unionInto: UnionInto[R, U] = new UnionInto[R, U] {
        def apply[X](union: Union[R, X]): Union[U, X] =
          t.project(union) match {
            case Right(tx) => m.inject(tx)
            case Left(us) => recurse.unionInto(us)
          }
      }
    }
}

trait IntoPolyLower5 {

  implicit def intoMember[T[_], R, U](implicit m: Member.Aux[T, R, U]): IntoPoly[U, R] = new IntoPoly[U, R] {
    val unionInto: UnionInto[U, R] = new UnionInto[U, R] {
      def apply[X](union: Union[U, X]): Union[R, X] =
        m.accept(union)
    }
  }
}
