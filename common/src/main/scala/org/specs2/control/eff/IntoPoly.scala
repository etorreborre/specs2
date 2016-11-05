package org.specs2.control.eff

import Eff._

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
  def apply[A](e: Eff[R, A]): Eff[U, A]
}

object IntoPoly extends IntoPolyLower1

trait IntoPolyLower1 extends IntoPolyLower2 {

  implicit def intoNil[R]: IntoPoly[NoFx, R] =
    new IntoPoly[NoFx, R] {
      def apply[A](e: Eff[NoFx, A]) =
        e match {
          case Pure(a, last) => pure[R, A](a).addLast(last.interpret(apply))
          case _ => sys.error("impossible NoFx into R is only for pure values")
        }
    }

  implicit def intoSelf[R]: IntoPoly[R, R] =
    new IntoPoly[R, R] { def apply[A](e: Eff[R, A]) = e }

}

trait IntoPolyLower2  extends IntoPolyLower3 {

  implicit def intoAppendL2L[T1[_], T2[_], R]: IntoPoly[FxAppend[Fx1[T2], R], FxAppend[Fx2[T1, T2], R]] =
    new IntoPoly[FxAppend[Fx1[T2], R], FxAppend[Fx2[T1, T2], R]] {
      def apply[A](e: Eff[FxAppend[Fx1[T2], R], A]): Eff[FxAppend[Fx2[T1, T2], R], A] =
        e match {
          case Pure(a, last) =>
            EffMonad[FxAppend[Fx2[T1, T2], R]].pure(a).addLast(last.interpret(apply))

          case Impure(u@UnionAppendR(r), c, l) =>
            Impure[FxAppend[Fx2[T1, T2], R], u.X, A](UnionAppendR(r), Arrs.singleton(x => effInto(c(x))), l.interpret(apply))

          case Impure(u@UnionAppendL(Union1(tx)), c, l) =>
            Impure[FxAppend[Fx2[T1, T2], R], u.X, A](UnionAppendL(Union2R(tx)), Arrs.singleton(x => effInto(c(x))), l.interpret(apply))

          case ImpureAp(unions, continuation, l) =>
            ImpureAp[FxAppend[Fx2[T1, T2], R], unions.X, A](
              unions.into(new UnionInto[FxAppend[Fx1[T2], R], FxAppend[Fx2[T1, T2], R]] {
                def apply[X](union: Union[FxAppend[Fx1[T2], R], X]) = union match {
                  case UnionAppendR(r) => UnionAppendR(r)
                  case UnionAppendL(Union1(tx)) => UnionAppendL(Union2R(tx))
                }}), Arrs.singleton(x => apply(continuation(x))), l.interpret(apply))
        }
    }

  implicit def intoAppendL2R[T1[_], T2[_], R]: IntoPoly[FxAppend[Fx1[T1], R], FxAppend[Fx2[T1, T2], R]] =
    new IntoPoly[FxAppend[Fx1[T1], R], FxAppend[Fx2[T1, T2], R]] {
      def apply[A](e: Eff[FxAppend[Fx1[T1], R], A]): Eff[FxAppend[Fx2[T1, T2], R], A] =
        e match {
          case Pure(a, last) =>
            EffMonad[FxAppend[Fx2[T1, T2], R]].pure(a).addLast(last.interpret(apply))

          case Impure(u@UnionAppendR(r), c, l) =>
            Impure[FxAppend[Fx2[T1, T2], R], u.X, A](UnionAppendR(r), Arrs.singleton(x => effInto(c(x))), l.interpret(apply))

          case Impure(u@UnionAppendL(Union1(tx)), c, l) =>
            Impure[FxAppend[Fx2[T1, T2], R], u.X, A](UnionAppendL(Union2L(tx)), Arrs.singleton(x => effInto(c(x))), l.interpret(apply))

          case ImpureAp(unions, continuation, l) =>
            ImpureAp[FxAppend[Fx2[T1, T2], R], unions.X, A](
              unions.into(new UnionInto[FxAppend[Fx1[T1], R], FxAppend[Fx2[T1, T2], R]] {
                def apply[X](union: Union[FxAppend[Fx1[T1], R], X]) = union match {
                  case UnionAppendR(r) => UnionAppendR(r)
                  case UnionAppendL(Union1(tx)) => UnionAppendL(Union2L(tx))
                }}), Arrs.singleton(x => apply(continuation(x))), l.interpret(apply))
        }
    }

  implicit def intoAppendL3L[T1[_], T2[_], T3[_], R]: IntoPoly[FxAppend[Fx2[T2, T3], R], FxAppend[Fx3[T1, T2, T3], R]] =
    new IntoPoly[FxAppend[Fx2[T2, T3], R], FxAppend[Fx3[T1, T2, T3], R]] {
      def apply[A](e: Eff[FxAppend[Fx2[T2, T3], R], A]): Eff[FxAppend[Fx3[T1, T2, T3], R], A] =
        e match {
          case Pure(a, last) =>
            EffMonad[FxAppend[Fx3[T1, T2, T3], R]].pure(a).addLast(last.interpret(apply))

          case Impure(u@UnionAppendR(r), c, l) =>
            Impure[FxAppend[Fx3[T1, T2, T3], R], u.X, A](UnionAppendR(r), Arrs.singleton(x => effInto(c(x))), l.interpret(apply))

          case Impure(u@UnionAppendL(Union2L(tx)), c, l) =>
            Impure[FxAppend[Fx3[T1, T2, T3], R], u.X, A](UnionAppendL(Union3M(tx)), Arrs.singleton(x => effInto(c(x))), l.interpret(apply))

          case Impure(u@UnionAppendL(Union2R(tx)), c, l) =>
            Impure[FxAppend[Fx3[T1, T2, T3], R], u.X, A](UnionAppendL(Union3R(tx)), Arrs.singleton(x => effInto(c(x))), l.interpret(apply))

          case ImpureAp(unions, continuation, l) =>
            ImpureAp[FxAppend[Fx3[T1, T2, T3], R], unions.X, A](
              unions.into(new UnionInto[FxAppend[Fx2[T2, T3], R], FxAppend[Fx3[T1, T2, T3], R]] {
                def apply[X](union: Union[FxAppend[Fx2[T2, T3], R], X]) = union match {
                  case UnionAppendR(r) => UnionAppendR(r)
                  case UnionAppendL(Union2L(tx)) => UnionAppendL(Union3M(tx))
                  case UnionAppendL(Union2R(tx)) => UnionAppendL(Union3R(tx))
                }}), Arrs.singleton(x => apply(continuation(x))), l.interpret(apply))
        }
    }

  implicit def intoAppendL3M[T1[_], T2[_], T3[_], R]: IntoPoly[FxAppend[Fx2[T1, T3], R], FxAppend[Fx3[T1, T2, T3], R]] =
    new IntoPoly[FxAppend[Fx2[T1, T3], R], FxAppend[Fx3[T1, T2, T3], R]] {
      def apply[A](e: Eff[FxAppend[Fx2[T1, T3], R], A]): Eff[FxAppend[Fx3[T1, T2, T3], R], A] =
        e match {
          case Pure(a, last) =>
            EffMonad[FxAppend[Fx3[T1, T2, T3], R]].pure(a).addLast(last.interpret(apply))

          case Impure(u@UnionAppendR(r), c, l) =>
            Impure[FxAppend[Fx3[T1, T2, T3], R], u.X, A](UnionAppendR(r), Arrs.singleton(x => effInto(c(x))), l.interpret(apply))

          case Impure(u@UnionAppendL(Union2L(tx)), c, l) =>
            Impure[FxAppend[Fx3[T1, T2, T3], R], u.X, A](UnionAppendL(Union3L(tx)), Arrs.singleton(x => effInto(c(x))), l.interpret(apply))

          case Impure(u@UnionAppendL(Union2R(tx)), c, l) =>
            Impure[FxAppend[Fx3[T1, T2, T3], R], u.X, A](UnionAppendL(Union3R(tx)), Arrs.singleton(x => effInto(c(x))), l.interpret(apply))

          case ImpureAp(unions, continuation, l) =>
            ImpureAp[FxAppend[Fx3[T1, T2, T3], R], unions.X, A](
              unions.into(new UnionInto[FxAppend[Fx2[T1, T3], R], FxAppend[Fx3[T1, T2, T3], R]] {
                def apply[X](union: Union[FxAppend[Fx2[T1, T3], R], X]) = union match {
                  case UnionAppendR(r) => UnionAppendR(r)
                  case UnionAppendL(Union2L(tx)) => UnionAppendL(Union3L(tx))
                  case UnionAppendL(Union2R(tx)) => UnionAppendL(Union3R(tx))
                }}), Arrs.singleton(x => apply(continuation(x))), l.interpret(apply))
        }
    }

  implicit def intoAppendL3R[T1[_], T2[_], T3[_], R]: IntoPoly[FxAppend[Fx2[T1, T2], R], FxAppend[Fx3[T1, T2, T3], R]] =
    new IntoPoly[FxAppend[Fx2[T1, T2], R], FxAppend[Fx3[T1, T2, T3], R]] {
      def apply[A](e: Eff[FxAppend[Fx2[T1, T2], R], A]): Eff[FxAppend[Fx3[T1, T2, T3], R], A] =
        e match {
          case Pure(a, last) =>
            EffMonad[FxAppend[Fx3[T1, T2, T3], R]].pure(a).addLast(last.interpret(apply))

          case Impure(u@UnionAppendR(r), c, l) =>
            Impure[FxAppend[Fx3[T1, T2, T3], R], u.X, A](UnionAppendR(r), Arrs.singleton(x => effInto(c(x))), l.interpret(apply))

          case Impure(u@UnionAppendL(Union2L(tx)), c, l) =>
            Impure[FxAppend[Fx3[T1, T2, T3], R], u.X, A](UnionAppendL(Union3L(tx)), Arrs.singleton(x => effInto(c(x))), l.interpret(apply))

          case Impure(u@UnionAppendL(Union2R(tx)), c, l) =>
            Impure[FxAppend[Fx3[T1, T2, T3], R], u.X, A](UnionAppendL(Union3M(tx)), Arrs.singleton(x => effInto(c(x))), l.interpret(apply))

          case ImpureAp(unions, continuation, l) =>
            ImpureAp[FxAppend[Fx3[T1, T2, T3], R], unions.X, A](
              unions.into(new UnionInto[FxAppend[Fx2[T1, T2], R], FxAppend[Fx3[T1, T2, T3], R]] {
                def apply[X](union: Union[FxAppend[Fx2[T1, T2], R], X]) = union match {
                  case UnionAppendR(r) => UnionAppendR(r)
                  case UnionAppendL(Union2L(tx)) => UnionAppendL(Union3L(tx))
                  case UnionAppendL(Union2R(tx)) => UnionAppendL(Union3M(tx))
                }}), Arrs.singleton(x => apply(continuation(x))), l.interpret(apply))

        }
    }
}

trait IntoPolyLower3 extends IntoPolyLower4 {
  implicit def intoAppendL1[T[_], R]: IntoPoly[R, FxAppend[Fx1[T], R]] =
    new IntoPoly[R, FxAppend[Fx1[T], R]] {
      def apply[A](e: Eff[R, A]): Eff[FxAppend[Fx1[T], R], A] =
        e match {
          case Pure(a, last) =>
            EffMonad[FxAppend[Fx1[T], R]].pure(a).addLast(last.interpret(apply))

          case Impure(u, c, l) =>
            Impure[FxAppend[Fx1[T], R], u.X, A](UnionAppendR(u), Arrs.singleton(x => effInto(c(x))), l.interpret(apply))

          case ImpureAp(unions, continuation, l) =>
            ImpureAp[FxAppend[Fx1[T], R], unions.X, A](
              unions.into(new UnionInto[R, FxAppend[Fx1[T], R]] {
                def apply[X](union: Union[R, X]) = union match {
                  case u => UnionAppendR(u)
                }}), Arrs.singleton(x => apply(continuation(x))), l.interpret(apply))
        }
    }
}

trait IntoPolyLower4 extends IntoPolyLower5 {

  implicit def into[T[_], R, U, S](implicit
                                   t: Member.Aux[T, R, S],
                                   m: T |= U,
                                   recurse: IntoPoly[S, U]): IntoPoly[R, U] =
    new IntoPoly[R, U] {
      def apply[A](e: Eff[R, A]): Eff[U, A] =
        e match {
          case Pure(a, last) =>
            EffMonad[U].pure(a).addLast(last.interpret(apply))

          case Impure(u, c, l) =>
            t.project(u) match {
              case Right(tx) => Impure[U, u.X, A](m.inject(tx), Arrs.singleton(x => effInto[R, U, A](c(x))), l.interpret(apply))
              case Left(s)   => recurse(Impure[S, s.X, s.X](s, Arrs.singleton(x => pure(x)))).flatMap((x: s.X) => effInto[R, U, A](c(x))).addLast(l.interpret(apply))
            }

          case ImpureAp(unions, continuation, l) =>
            ImpureAp[U, unions.X, A](unions.into(new UnionInto[R, U] {
              def apply[X](u: Union[R, X]): Union[U, X] =
                t.project(u) match {
                  case Right(t1)   => m.inject(t1)
                  case Left(other) =>
                    recurse(Impure[S, X, X](other, Arrs.singleton(x => pure(x)))) match {
                      case Impure(u1, _, _) => u1.asInstanceOf[Union[U, X]]
                      case _ => sys.error("impossible into case: Impure must be transformed to Impure")
                    }
                }
            }), Arrs.singleton(x => apply(continuation(x))), l.interpret(apply))
        }
    }
}

trait IntoPolyLower5 {

  implicit def intoMember[T[_], R, U](implicit m: Member.Aux[T, R, U]): IntoPoly[U, R] = new IntoPoly[U, R] {
    def apply[A](e: Eff[U, A]): Eff[R, A] =
      e match {
        case Pure(a, last) => pure[R, A](a)
        case Impure(u, c, l) =>
          Impure(m.accept(u), Arrs.singleton((x: u.X) => intoMember.apply(c(x))), l.interpret(apply))

        case ImpureAp(unions, c, l) =>
          ImpureAp(unions.into(new UnionInto[U, R] { def apply[X](u: Union[U, X]) = m.accept(u) }),
            Arrs.singleton((xs: List[Any]) => intoMember.apply(c(xs))), l.interpret(apply))
      }
  }
}
