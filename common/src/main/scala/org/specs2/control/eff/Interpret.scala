package org.specs2.control.eff

import scalaz._, Scalaz._
import Eff._

/**
 * Support methods to create interpreters (or "effect handlers") for a given effect M and a value Eff[R, A]
 * when M is a member of R.
 *
 * Those methods guarantee a stack-safe behaviour when running on a large list of effects
 * (in list.traverse(f) for example).
 *
 * There are different types of supported interpreters:
 *
 *  1. "interpret" + Recurse
 *
 *  This interpreter is used to handle effects which either return a value X from M[X] or stops with Eff[R, B]
 *  See an example of such an interpreter in Eval where we just evaluate a computation X for each Eval[X].
 *
 *  2. "interpretState" + StateRecurse
 *
 *  This interpreter is used to handle effects which either return a value X from M[X] or stops with Eff[R, B]
 *
 *  3. "interpretLoop" + Loop
 *
 *  The most generic kind of interpreter where we can even recurse in the case of Pure(a) (See ListEffect for such a use)
 *
 *  4. "intercept / interceptState / interceptLoop" methods are similar but they transform an effect to other effects in
 *  the same stack without removing it from the stack
 *
 *  5. "transform" to swap an effect T of a stack to another effect, using a Natural Transformation
 *
 *  6. "translate" to interpret one effect of a stack into other effects of the same stack using a Natural Transformation
 *     this is a specialized version of interpret + Recurse
 *
 *  7. "interpretUnsafe + SideEffect" when you have a side effecting function M[X] => X
 */
trait Interpret {

  /**
   * interpret the effect M in the R stack
   */
  def interpret[R, U, M[_], A, B](pure: A => Eff[U, B], recurse: Recurse[M, U, B])(effects: Eff[R, A])(implicit m: Member.Aux[M, R, U]): Eff[U, B] = {
    val loop = new Loop[M, R, A, Eff[U, B], Eff[U, Unit]] {
      type S = Unit
      val init = ()

      def onPure(a: A, s: Unit): (Eff[R, A], Unit) Either Eff[U, B] =
        Right(pure(a))

      def onEffect[X](mx: M[X], continuation: Arrs[R, X, A], s: Unit): (Eff[R, A], Unit) Either Eff[U, B] =
        recurse(mx).bimap(x => (continuation(x), ()), identity)

      def onLastEffect[X](mx: M[X], continuation: Arrs[R, X, Unit], s: Unit): (Eff[R, Unit], Unit) Either Eff[U, Unit] =
        recurse(mx).map(_.void).bimap(x => (continuation(x), ()), identity)

      def onApplicativeEffect[X, T[_] : Traverse](mx: T[M[X]], continuation: Arrs[R, T[X], A], s: Unit): (Eff[R, A], Unit) Either Eff[U, B] =
        recurse.applicative(mx) match {
          case Left(xs) => Left((continuation(xs), s))
          case Right(mlx) => onEffect(mlx, continuation, s)
        }

      def onLastApplicativeEffect[X, T[_] : Traverse](xs: T[M[X]], continuation: Arrs[R, T[X], Unit], s: S): (Eff[R, Unit], S) Either Eff[U, Unit] =
        recurse.applicative(xs) match {
          case Left(xs1) => Left((continuation(xs1), s))
          case Right(mlx) => onLastEffect(mlx, continuation, s)
        }

    }
    interpretLoop[R, U, M, A, B](pure, loop)(effects)
  }

  /**
   * simpler version of interpret where the pure value is just mapped to another type
   */
  def interpret1[R, U, M[_], A, B](pure: A => B)(recurse: Recurse[M, U, B])(effects: Eff[R, A])(implicit m: Member.Aux[M, R, U]): Eff[U, B] =
  interpret[R, U, M, A, B]((a: A) => EffMonad[U].pure(pure(a)), recurse)(effects)

  /**
   * Helper trait for computations
   * which might produce several M[X] in a stack of effects and which need to keep some state around
   *
   * This is typically the case for Writer or State which need to keep some state S after each evaluation
   * Evaluating the effect M[X] might use the previous S value as shown in the `apply method`
   *
   * Finally when the Eff[R, A] returns an A, this one can be combined with the last state value to produce a B
   *
   */
  trait StateRecurse[M[_], A, B] {
    type S
    val init: S
    def apply[X](x: M[X], s: S): (X, S)
    def applicative[X, T[_] : Traverse](xs: T[M[X]], s: S): (T[X], S) Either (M[T[X]], S)
    def finalize(a: A, s: S): B
  }

  /**
   * interpret the effect M in the M |: R stack, keeping track of some state
   */
  def interpretState[R, U, M[_], A, B](pure: A => Eff[U, B], recurse: StateRecurse[M, A, B])(effects: Eff[R, A])(implicit m: Member.Aux[M, R, U]): Eff[U, B] = {
    val loop = new Loop[M, R, A, Eff[U, B], Eff[U, Unit]] {
      type S = recurse.S
      val init: S = recurse.init

      def onPure(a: A, s: S): (Eff[R, A], S) Either Eff[U, B] =
        Right(EffMonad[U].pure(recurse.finalize(a, s)))

      def onEffect[X](mx: M[X], continuation: Arrs[R, X, A], s: S): (Eff[R, A], S) Either Eff[U, B] =
        Left { recurse(mx, s) match { case (a, b) => (continuation(a), b)} }

      def onLastEffect[X](mx: M[X], continuation: Arrs[R, X, Unit], s: S): (Eff[R, Unit], S) Either Eff[U, Unit] =
        Left { recurse(mx, s) match { case (a, b) => (continuation(a), b)} }

      def onApplicativeEffect[X, T[_] : Traverse](mx: T[M[X]], continuation: Arrs[R, T[X], A], s: S): (Eff[R, A], S) Either Eff[U, B] =
        recurse.applicative(mx, s) match {
          case Left((ls, s1))   => Left((continuation(ls), s1))
          case Right((mlx, s1)) => onEffect(mlx, continuation, s1)
        }

      def onLastApplicativeEffect[X, T[_] : Traverse](xs: T[M[X]], continuation: Arrs[R, T[X], Unit], s: S): (Eff[R, Unit], S) Either Eff[U, Unit] =
        recurse.applicative(xs, s) match {
          case Left((ls, s1))   => Left((continuation(ls), s1))
          case Right((mlx, s1)) => onLastEffect(mlx, continuation, s1)
        }

    }
    interpretLoop(pure, loop)(effects)
  }

  /**
   * simpler version of interpret1 where the pure value is just mapped to another type
   */
  def interpretState1[R, U, M[_], A, B](pure: A => B)(recurse: StateRecurse[M, A, B])(effects: Eff[R, A])(implicit m: Member.Aux[M, R, U]): Eff[U, B] =
  interpretState((a: A) => EffMonad[U].pure(pure(a)), recurse)(effects)

  /**
   * generalization of interpret and interpretState
   *
   * This method contains a loop which is stack-safe
   */
  def interpretLoop[R, U, M[_], A, B](pure: A => Eff[U, B], loop: Loop[M, R, A, Eff[U, B], Eff[U, Unit]])(effects: Eff[R, A])(implicit m: Member.Aux[M, R, U]): Eff[U, B] = {
    def goLast(ls: Last[R], s: loop.S): Eff[U, Unit] =
      ls match {
        case Last(None) => Eff.pure[U, Unit](())
        case Last(Some(l)) =>
          l.value match {
            case Pure(u, last) => goLast(last, s)

            case Impure(union, continuation, last) =>
              m.project(union) match {
                case Right(mx) =>
                  loop.onLastEffect(mx, continuation, s) match {
                    case Left((x, s1)) => goLast(Last.eff(x.addLast(last)), s1)
                    case Right(b)      => goLast(last, s)
                  }
                case Left(u) =>
                  Impure[U, union.X, Unit](u, Arrs.singleton(x => goLast(Last.eff(continuation(x).addLast(last)), s)))
              }

            case ap @ ImpureAp(unions, continuation, last) =>
              val collected = unions.project

              if (collected.effects.isEmpty)
                collected.othersEff(Arrs.singleton(x => goLast(Last.eff(continuation(x).addLast(last)), s)))
              else
                loop.onLastApplicativeEffect(collected.effects, collected.continuation(continuation, m), s) match {
                  case Left((x, s1)) => goLast(Last.eff(x.addLast(last)), s1)
                  case Right(b)      => goLast(last, s)
                }
          }
      }

    def go(eff: Eff[R, A], s: loop.S): Eff[U, B] = {
      eff match {
        case Pure(a, last) =>
          loop.onPure(a, s) match {
            case Left((a1, s1)) => go(a1.addLast(last), s1)
            case Right(b)       => b.addLast(goLast(last, s))
          }

        case Impure(union, continuation, last) =>
          m.project(union) match {
            case Right(v) =>
              loop.onEffect(v, continuation, s) match {
                case Left((x, s1)) => go(x.addLast(last), s1)
                case Right(b)      => b.addLast(goLast(last, s))
              }

            case Left(u) =>
              Impure[U, union.X, B](u, Arrs.singleton(x => go(continuation(x), s)), Last.eff(goLast(last, s)))
          }

        case ap @ ImpureAp(unions, continuation, last) =>
          val collected = unions.project

          if (collected.effects.isEmpty)
            collected.othersEff(Arrs.singleton(x => go(continuation(x).addLast(last), s)))
          else
            loop.onApplicativeEffect(collected.effects, collected.continuation(continuation, m), s) match {
              case Left((x, s1)) => go(x.addLast(last), s1)
              case Right(b)      => b.addLast(goLast(last, s))
            }
      }
    }

    go(effects, loop.init)
  }

  def interpretLoop1[R, U, M[_], A, B](pure: A => B)(loop: Loop[M, R, A, Eff[U, B], Eff[U, Unit]])(effects: Eff[R, A])(implicit m: Member.Aux[M, R, U]): Eff[U, B] =
    interpretLoop[R, U, M, A, B]((a: A) => EffMonad[U].pure(pure(a)), loop)(effects)

  /**
   * generalization of interpret
   *
   * This method contains a loop which is stack-safe
   */
  def interpretStatelessLoop[R, U, M[_], A, B](pure: A => Eff[U, B], loop: StatelessLoop[M, R, A, Eff[U, B], Eff[U, Unit]])(effects: Eff[R, A])(implicit m: Member.Aux[M, R, U]): Eff[U, B] =
  interpretLoop[R, U, M, A, B](pure, new Loop[M, R, A, Eff[U, B], Eff[U, Unit]] {
    type S = Unit
    val init: S = ()
    def onPure(a: A, s: S) = loop.onPure(a).leftMap((_, init))
    def onEffect[X](x: M[X], continuation: Arrs[R, X, A], s: S) = loop.onEffect(x, continuation).leftMap((_, init))
    def onLastEffect[X](x: M[X], continuation: Arrs[R, X, Unit], s: S) = loop.onLastEffect(x, continuation).leftMap((_, init))
    def onApplicativeEffect[X, T[_] : Traverse](xs: T[M[X]], continuation: Arrs[R, T[X], A], s: S) = loop.onApplicativeEffect(xs, continuation).leftMap((_, init))
    def onLastApplicativeEffect[X, T[_] : Traverse](xs: T[M[X]], continuation: Arrs[R, T[X], Unit], s: S) = loop.onLastApplicativeEffect(xs, continuation).leftMap((_, init))
  })(effects)(m)

  def interpretStatelessLoop1[R, U, M[_], A, B](pure: A => B)(loop: StatelessLoop[M, R, A, Eff[U, B], Eff[U, Unit]])(effects: Eff[R, A])(implicit m: Member.Aux[M, R, U]): Eff[U, B] =
    interpretStatelessLoop[R, U, M, A, B]((a: A) => EffMonad[U].pure(pure(a)), loop)(effects)

  /**
   * INTERPRET IN THE SAME STACK
   */
  def intercept[R, M[_], A, B](pure: A => Eff[R, B], recurse: Recurse[M, R, B])(effects: Eff[R, A])(implicit m: M /= R): Eff[R, B] = {
    val loop = new Loop[M, R, A, Eff[R, B], Eff[R, Unit]] {
      type S = Unit
      val init = ()

      def onPure(a: A, s: Unit): (Eff[R, A], Unit) Either Eff[R, B] =
        Right(pure(a))

      def onEffect[X](mx: M[X], continuation: Arrs[R, X, A], s: S): (Eff[R, A], Unit) Either Eff[R, B] =
        recurse(mx).bimap(x => (continuation(x), ()), identity)

      def onLastEffect[X](mx: M[X], continuation: Arrs[R, X, Unit], s: S): (Eff[R, Unit], S) Either Eff[R, Unit] =
        recurse(mx).map(_.void).bimap(x => (continuation(x), ()), identity)

      def onApplicativeEffect[X, T[_] : Traverse](xs: T[M[X]], continuation: Arrs[R, T[X], A], s: S): (Eff[R, A], S) Either Eff[R, B] =
        recurse.applicative(xs) match {
          case Left(ls)   => Left((continuation(ls), s))
          case Right(mlx) => onEffect(mlx, continuation, s)
        }

      def onLastApplicativeEffect[X, T[_] : Traverse](xs: T[M[X]], continuation: Arrs[R, T[X], Unit], s: S): (Eff[R, Unit], S) Either Eff[R, Unit] =
        recurse.applicative(xs) match {
          case Left(ls)   => Left((continuation(ls), s))
          case Right(mlx) => onLastEffect(mlx, continuation, s)
        }

    }
    interceptLoop[R, M, A, B](pure, loop)(effects)
  }

  /**
   * simpler version of intercept where the pure value is just mapped to another type
   */
  def intercept1[R, M[_], A, B](pure: A => B)(recurse: Recurse[M, R, B])(effects: Eff[R, A])(implicit m: M /= R): Eff[R, B] =
  intercept[R, M, A, B]((a: A) => EffMonad[R].pure(pure(a)), recurse)(effects)

  /**
   * intercept an effect and interpret it in the same stack.
   * This method is stack-safe
   */
  def interceptLoop[R, M[_], A, B](pure: A => Eff[R, B], loop: Loop[M, R, A, Eff[R, B], Eff[R, Unit]])(effects: Eff[R, A])(implicit m: M /= R): Eff[R, B] = {
    def goLast(ls: Last[R], s: loop.S): Eff[R, Unit] =
      ls match {
        case Last(None) => Eff.pure[R, Unit](())
        case Last(Some(l)) =>
          l.value match {
            case Pure(u, last) => goLast(last, s)

            case Impure(union, continuation, last) =>
              m.extract(union) match {
                case Some(mx) =>
                  loop.onLastEffect(mx, continuation, s) match {
                    case Left((x, s1)) => goLast(Last.eff(x.addLast(last)), s1)
                    case Right(b)      => goLast(last, s)
                  }
                case None =>
                  Impure[R, union.X, Unit](union, Arrs.singleton(x => goLast(Last.eff(continuation(x).addLast(last)), s)))
              }

            case ap @ ImpureAp(unions, continuation, last) =>
              val collected = unions.extract

              if (collected.effects.isEmpty)
                collected.othersEff(Arrs.singleton(x => goLast(Last.eff(continuation(x).addLast(last)), s)))
              else
                loop.onLastApplicativeEffect(collected.effects, collected.continuation(continuation), s) match {
                  case Left((x, s1)) => goLast(Last.eff(x.addLast(last)), s1)
                  case Right(b)      => goLast(last, s)
                }
          }
      }

    def go(eff: Eff[R, A], s: loop.S): Eff[R, B] = {
      eff match {
        case Pure(a, last) =>
          loop.onPure(a, s) match {
            case Left((a1, s1)) => go(a1, s1).addLast(goLast(last, s1))
            case Right(b) => b.addLast(goLast(last, s))
          }

        case Impure(union, continuation, last) =>
          m.extract(union) match {
            case Some(v) =>
              loop.onEffect(v, continuation, s) match {
                case Left((x, s1)) => go(x.addLast(last), s1)
                case Right(b)      => b.addLast(goLast(last, s))
              }

            case None =>
              Impure[R, union.X, B](union, Arrs.singleton(x => go(continuation(x).addLast(last), s)))
          }

        case ImpureAp(unions, continuation, last) =>
          val collect = unions.extract

          if (collect.effects.isEmpty)
            collect.othersEff(Arrs.singleton(x => go(continuation(x).addLast(last), s)))
          else
            loop.onApplicativeEffect(collect.effects, collect.continuation(continuation), s) match {
              case Left((x, s1)) => go(x.addLast(last), s1)
              case Right(b)      => b.addLast(goLast(last, s))
            }
      }
    }

    go(effects, loop.init)
  }

  def interceptLoop1[R, M[_], A, B](pure: A => B)(loop: Loop[M, R, A, Eff[R, B], Eff[R, Unit]])(effects: Eff[R, A])(implicit m: M /= R): Eff[R, B] =
    interceptLoop[R, M, A, B]((a: A) => EffMonad[R].pure(pure(a)), loop)(effects)

  def interceptStatelessLoop[R, M[_], A, B](pure: A => Eff[R, B], loop: StatelessLoop[M, R, A, Eff[R, B], Eff[R, Unit]])(effects: Eff[R, A])(implicit m: M /= R): Eff[R, B] =
    interceptLoop[R, M, A, B](pure, new Loop[M, R, A, Eff[R, B], Eff[R, Unit]] {
      type S = Unit
      val init: S = ()
      def onPure(a: A, s: S) = loop.onPure(a).leftMap((_, ()))
      def onEffect[X](x: M[X], continuation: Arrs[R, X, A], s: S) = loop.onEffect(x, continuation).leftMap((_, ()))
      def onLastEffect[X](x: M[X], continuation: Arrs[R, X, Unit], s: S) = loop.onLastEffect(x, continuation).leftMap((_, ()))
      def onApplicativeEffect[X, T[_] : Traverse](xs: T[M[X]], continuation: Arrs[R, T[X], A], s: S) = loop.onApplicativeEffect(xs, continuation).leftMap((_, ()))
      def onLastApplicativeEffect[X, T[_] : Traverse](xs: T[M[X]], continuation: Arrs[R, T[X], Unit], s: S) = loop.onLastApplicativeEffect(xs, continuation).leftMap((_, ()))
    })(effects)(m)

  def interceptStatelessLoop1[R, M[_], A, B](pure: A => B)(loop: StatelessLoop[M, R, A, Eff[R, B], Eff[R, Unit]])(effects: Eff[R, A])(implicit m: M /= R): Eff[R, B] =
    interceptStatelessLoop[R, M, A, B]((a: A) => EffMonad[R].pure(pure(a)), loop)(effects)

  /**
   * transform an effect into another one
   * using a natural transformation, leaving the rest of the stack untouched
   */
  def transform[SR, BR, U, TS[_], TB[_], A](r: Eff[SR, A], nat: TS ~> TB)
                                           (implicit sr: Member.Aux[TS, SR, U], br: Member.Aux[TB, BR, U]): Eff[BR, A] = {

    def goLast(ls: Last[SR]): Eff[BR, Unit] =
      ls match {
        case Last(None) => Eff.pure[BR, Unit](())
        case Last(Some(l)) =>
          l.value match {
            case Pure(u, last) => goLast(last)

            case Impure(union, continuation, last) =>
              sr.project(union) match {
                case Right(small) =>
                  Impure(br.inject(nat(small)), Arrs.singleton((x: union.X) => goLast(Last.eff(continuation(x).addLast(last)))))

                case Left(u1) =>
                  Impure(br.accept(u1), Arrs.singleton((x: union.X) => goLast(Last.eff(continuation(x).addLast(last)))))

              }

            case ap @ ImpureAp(unions, continuation, last) =>
              goLast(Last.eff(ap.toMonadic))
          }
      }

    def go(eff: Eff[SR, A]): Eff[BR, A] = {
      eff match {
        case Pure(a, last) => Pure[BR, A](a).addLast(goLast(last))

        case Impure(u, c, last) =>
          sr.project(u) match {
            case Right(small) =>
              Impure(br.inject(nat(small)), Arrs.singleton((x: u.X) => go(c(x).addLast(last))))

            case Left(u1) =>
              Impure(br.accept(u1), Arrs.singleton((x: u.X) => go(c(x).addLast(last))))
          }

        case ap @ ImpureAp(unions, continuation, last) =>
          ImpureAp(unions.transformInto(nat), Arrs.singleton(x => transform(continuation(x).addLast(last), nat)))
      }
    }

    go(r)
  }

  /**
   * Translate one effect of the stack into some of the other effects in the stack
   */
  def translate[R, U, T[_], A](effects: Eff[R, A])
                              (tr: Translate[T, U])
                              (implicit m: Member.Aux[T, R, U]): Eff[U, A] = {
    def goLast(ls: Last[R]): Eff[U, Unit] =
      ls match {
        case Last(None) => Eff.pure[U, Unit](())
        case Last(Some(l)) =>
          l.value match {
            case Pure(u, last) => goLast(last)

            case Impure(union, continuation, last) =>
              m.project(union) match {
                case Right(kv) =>
                  val effectsU: Eff[U, union.X] = tr(kv)
                  effectsU.flatMap(r => goLast(Last.eff(continuation(r).addLast(last))))

                case Left(u1) =>
                  Impure(u1, Arrs.singleton((x: union.X) => goLast(Last.eff(continuation(x).addLast(last)))))
              }

            case ap @ ImpureAp(unions, continuation, last) =>
              val collected = unions.project

              if (collected.effects.isEmpty)
                collected.othersEff(Arrs.singleton(x => goLast(Last.eff(continuation(x).addLast(last)))))
              else {
                val translated: Eff[U, List[Any]] = EffApplicative.traverse(collected.effects)(tr.apply)
                translated.flatMap(ls => goLast(Last.eff(collected.continuation(continuation, m).apply(ls).addLast(last))))
              }
          }
      }

    def go(eff: Eff[R, A]): Eff[U, A] = {
      eff match {
        case Pure(a, last) => Pure(a).addLast(goLast(last))

        case Impure(union, continuation, last) =>
          m.project(union) match {
            case Right(kv) =>
              val effectsU: Eff[U, union.X] = tr(kv)
              effectsU.flatMap(r => go(continuation(r).addLast(last)))

            case Left(u1) =>
              Impure(u1, Arrs.singleton((x: union.X) => go(continuation(x).addLast(last))))
          }

        case ap @ ImpureAp(unions, continuation, last) =>
          val collected = unions.project

          if (collected.effects.isEmpty)
            collected.othersEff(Arrs.singleton(x => go(continuation(x).addLast(last))))
          else {
            val translated: Eff[U, List[Any]] = EffApplicative.traverse(collected.effects)(tr.apply)
            translated.flatMap(ls => translate(collected.continuation(continuation, m).apply(ls).addLast(last))(tr))
          }
      }
    }

    go(effects)
  }

  /**
   * Translate one effect of the stack into some of the other effects in the stack
   * Using a natural transformation
   */
  def translateNat[R, U, T[_], A](effects: Eff[R, A])
                                 (nat: T ~> Eff[U, ?])
                                 (implicit m: Member.Aux[T, R, U]): Eff[U, A] =
  translate(effects)(new Translate[T, U] {
    def apply[X](tx: T[X]): Eff[U, X] = nat(tx)
  })

  /**
   * Translate one effect of the stack into other effects in a larger stack
   */
  def translateInto[R, T[_], U, A](eff: Eff[R, A])(translate: Translate[T, U])(implicit m: MemberInOut[T, R], into: IntoPoly[R, U]): Eff[U, A] =  {
    def goLast(ls: Last[R]): Eff[U, Unit] =
      ls match {
        case Last(None) => Eff.pure[U, Unit](())
        case Last(Some(l)) =>
          l.value match {
            case Pure(u, last) => goLast(last)

            case i @ Impure(union, continuation, last) =>
              m.extract(union) match {
                case Some(tx) => translate(tx).flatMap(x => goLast(Last.eff(continuation(x).addLast(last))))
                case None     => into(i).addLast(goLast(last))
              }

            case ap @ ImpureAp(unions, continuation, last) =>
              val translated: Eff[U, List[Any]] = Eff.traverseA(unions.extract.effects)(tx => translate(tx).addLast(goLast(last)))
              translated.flatMap(ts => goLast(Last.eff(continuation(ts).addLast(last))))
          }
      }

    eff match {
      case Pure(a, last) => into(eff).addLast(goLast(last))

      case Impure(u, c, last) =>
        m.extract(u) match {
          case Some(tx) => translate(tx).flatMap(x => translateInto(c(x).addLast(last))(translate))
          case None     => into(eff).addLast(goLast(last))
        }

      case ImpureAp(unions, c, last) =>
        val translated: Eff[U, List[Any]] = Eff.traverseA(unions.extract.effects)(tx => translate(tx).addLast(goLast(last)))
        translated.flatMap(ts => translateInto(c(ts).addLast(last))(translate))
    }
  }

  def augment[R, T[_], O[_], A](eff: Eff[R, A])(w: Augment[T, O])(implicit m: MemberInOut[T, R]): Eff[Fx.prepend[O, R], A] =  {
    type U = Fx.prepend[O, R]
    implicit val mw = MemberIn.MemberInAppendAnyL

    translateInto(eff)(new Translate[T, U] {
      def apply[X](tx: T[X]): Eff[U, X] = send[O, U, Unit](w(tx)) >> send[T, U, X](tx)
    })
  }

  /**
   * Intercept the values for one effect and transform them into
   * other values for the same effect
   */
  def interceptNat[R, T[_], A](effects: Eff[R, A])
                              (nat: T ~> T)
                              (implicit m: MemberInOut[T, R]): Eff[R, A] = {

    def goLast(ls: Last[R]): Eff[R, Unit] =
      ls match {
        case Last(None) => Eff.pure[R, Unit](())
        case Last(Some(l)) =>
          l.value match {
            case Pure(u, last) => goLast(last)

            case Impure(union, continuation, last) =>
              m.extract(union) match {
                case None     => Impure(union, Arrs.singleton((x: union.X) => goLast(Last.eff(continuation(x).addLast(last)))))
                case Some(tx) => Impure(m.inject(nat(tx)), Arrs.singleton((x: union.X) => goLast(Last.eff(continuation(x).addLast(last)))))
              }

            case ap @ ImpureAp(unions, continuation, last) =>
              ImpureAp(unions.transform(nat), Arrs.singleton(x => goLast(Last.eff(continuation(x).addLast(last)))))
          }
      }

    effects match {
      case Pure(a, last) => Pure(a).addLast(goLast(last))

      case Impure(union, continuation, last) =>
        m.extract(union) match {
          case None     => Impure(union, Arrs.singleton((x: union.X) => interceptNat(continuation(x).addLast(last))(nat)))
          case Some(tx) => Impure(m.inject(nat(tx)), Arrs.singleton((x: union.X) => interceptNat(continuation(x).addLast(last))(nat)))
        }

      case ImpureAp(unions, continuation, last) =>
        ImpureAp(unions.transform(nat), Arrs.singleton(x => interceptNat(continuation(x).addLast(last))(nat)))
    }
  }

  type of[F[_], G[_]] = {type l[A] = F[G[A]]}

  /**
   * Intercept the values for one effect,
   * emitting new values for the same effect inside a monad which is interleaved in
   */
  def interceptNatM[R, T[_], F[_], A](effects: Eff[R, A], nat: T ~> (T `of` F)#l)
                                     (implicit m: MemberInOut[T, R], FT: Traverse[F], FM: Monad[F]): Eff[R, F[A]] = {
    effects match {
      case Pure(a, last) =>
        pure[R, F[A]](FM.point(a)).addLast(last)

      case Impure(u, c, last) =>
        m.extract(u) match {
          case Some(tx) =>
            val union = m.inject(nat(tx))

            Impure(union, Arrs.singleton({ ex: F[u.X] =>
              Eff.flatTraverseA(ex)(x => interceptNatM[R, T, F, A](c(x), nat))
            }), last)

          case None => Impure(u, c.mapLast(r => interceptNatM[R, T, F, A](r, nat)), last)
        }

      case ImpureAp(unions, continuation, last) =>
        def materialize(u: Union[R, Any]): Union[R, Any] =
          m.extract(u) match {
            case Some(tx) => m.inject(nat(tx).asInstanceOf[T[Any]])
            case None => u
          }

        val materializedUnions =
          Unions(materialize(unions.first), unions.rest.map(materialize))

        val collected = unions.extract(m)
        val continuation1 = Arrs.singleton[R, List[Any], F[A]]({ ls: List[Any] =>
          val xors =
            ls.zipWithIndex.collect { case (a, i) =>
              if (collected.indices.contains(i)) a.asInstanceOf[F[Any]]
              else FM.pure(a)
            }.sequence

          Eff.flatTraverseA(xors)(x => interceptNatM[R, T, F, A](continuation(x), nat))

        })

        ImpureAp(materializedUnions, continuation1, last)
    }
  }

  /** interpret an effect by running side-effects */
  def interpretUnsafe[R, U, T[_], A](effects: Eff[R, A])
                                    (sideEffect: SideEffect[T])
                                    (implicit m: Member.Aux[T, R, U]): Eff[U, A] = {
    val recurse = new Recurse[T, m.Out, A] {
      def apply[X](tx: T[X]): X Either Eff[m.Out, A] =
        Left(sideEffect(tx))

      def applicative[X, Tr[_]: Traverse](ms: Tr[T[X]]): Tr[X] Either T[Tr[X]] =
        Left(ms.map(sideEffect.apply))
    }
    interpret1((a: A) => a)(recurse)(effects)(m)
  }
}

object Interpret extends Interpret

/**
 * Helper trait for computations
 * which might produce several M[X] in a stack of effects.
 *
 * Either we can produce an X to pass to a continuation or we're done
 *
 * For the applicative case we expect to be able to traverse a list
 * of effects and return an effect of a list of results OR
 * completely consume the effect and return a pure list of values
 */
trait Recurse[M[_], R, A] {
  def apply[X](m: M[X]): X Either Eff[R, A]
  def applicative[X, T[_]: Traverse](ms: T[M[X]]): T[X] Either M[T[X]]
}

/**
 * Generalisation of Recurse and StateRecurse
 *
 * The loop defines some state with an initial value which is maintained at
 * each step of the interpretation.
 *
 * A is the type of Eff values to interpret, and B is the result of the
 * interpretation (generally an other Eff value)
 *
 * C is the type of result for "last" actions.
 *
 * - the interpretation of a Pure value either returns the final result or possibly
 *   one more Eff value to interpret
 *
 * - onEffect interprets one effect and possibly uses the continuation to produce the next
 *   value to interpret. If no X can be used to run the continuation we might just
 *   output one final B value
 *
 *  - onLastEffect interprets the last effect of an Eff value. The only difference with onEffect
 *    is the fact that last actions return Unit values (and not A values)
 *
 *  - onApplicativeEff interprets a list of effects and possibly uses the continuation to
 *    get to the next value to interpret. If no interpretation can be done, a B value might be returned
 *
 *  - onLastApplicativeEffect does the same thing for last actions
 *
 */
trait Loop[M[_], R, A, B, C] {
  type S
  val init: S

  def onPure(a: A, s: S): (Eff[R, A], S) Either B

  def onEffect[X](x: M[X], continuation: Arrs[R, X, A], s: S): (Eff[R, A], S) Either B
  def onLastEffect[X](x: M[X], continuation: Arrs[R, X, Unit], s: S): (Eff[R, Unit], S) Either C

  def onApplicativeEffect[X, T[_] : Traverse](xs: T[M[X]], continuation: Arrs[R, T[X], A], s: S): (Eff[R, A], S) Either B
  def onLastApplicativeEffect[X, T[_] : Traverse](xs: T[M[X]], continuation: Arrs[R, T[X], Unit], s: S): (Eff[R, Unit], S) Either C
}

/**
 * Generalisation of Recurse
 */
trait StatelessLoop[M[_], R, A, B, C] {
  def onPure(a: A): Eff[R, A] Either B

  def onEffect[X](x: M[X], continuation: Arrs[R, X, A]): Eff[R, A] Either B
  def onLastEffect[X](x: M[X], continuation: Arrs[R, X, Unit]): Eff[R, Unit] Either C

  def onApplicativeEffect[X, T[_] : Traverse](xs: T[M[X]], continuation: Arrs[R, T[X], A]): Eff[R, A] Either B
  def onLastApplicativeEffect[X, T[_] : Traverse](xs: T[M[X]], continuation: Arrs[R, T[X], Unit]): Eff[R, Unit] Either C
}

/**
 * trait for translating one effect into other ones in the same stack
 */
trait Translate[T[_], U] {
  def apply[X](kv: T[X]): Eff[U, X]
}

trait SideEffect[T[_]] {
  def apply[X](tx: T[X]): X
  def applicative[X, Tr[_] : Traverse](ms: Tr[T[X]]): Tr[X]
}

trait Augment[T[_], O[_]] {
  def apply[X](tx: T[X]): O[Unit]
}

trait Write[T[_], O] {
  def apply[X](tx: T[X]): O
}

