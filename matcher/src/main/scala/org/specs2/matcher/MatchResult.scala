package org.specs2
package matcher

import scalaz.{ Functor, Scalaz }, Scalaz._
import execute._
import MatchResultLogicalCombinators._
import ResultLogicalCombinators._

/**
 * Result of a Match.
 * 
 * A MatchResult contains several information about a match on an expectable:
 * 
 * - the expectable value, to allow the chaining of matches
 * - a pair of messages ok message / ko message to allow the easy creation of the negation
 *   of a match
 * 
 * A MatchResult can be transformed to a simple Result object to be the body of an Example.
 * 
 * There are different kinds of MatchResults, some of them being only created to support
 * English-like combination of Matchers:
 * 
 * `1 must be equalTo(1) and not be equalTo(2)`
 * 
 * In an Expectation like the one above, there is a left to right evaluation:
 * 
 *  1. be is a NeutralMatcher, returning a NeutralMatch doing nothing yet, just storing
 *     the expectable
 *  
 *  2. equalTo(1) is a real Matcher which is applied to the NeutralMatch MatchResult
 *     thanks to an implicit definition in the BeHaveAnyMatchers trait. This yields a 
 *     MatchSuccess result
 *  
 *  3. not creates a NotMatcher and can be and-ed with the previous MatchSuccess to
 *     yield a AndMatch(MatchSuccess, NotMatch), with NotMatch being the result of
 *     applying the NotMatcher to the expectable. This AndMatch is evaluated to create a 
 *     AndNotMatch(MatchSuccess, MatchSkip)
 *     
 *     Basically this is like forming an evaluation
 *     structure which will be resolved when the next 'real' matcher will arrive
 *     
 *  4. the AndNotMatch get nows it be method called with the equalTo Matcher.
 *     This results in equalTo being applied to the AndNotMatch, effectively doing:
 *     MatchSuccess and MatchSkip.apply(equalTo(2).not), which is
 *     MatchSuccess and expectable.applyMatcher(equalTo(2).not) which is MatchSuccess
 * 
 * @see org.specs2.matcher.BeHaveMatchersSpec for examples
 */
trait MatchResult[+T] extends ResultLike {
  /** the value being matched */
  val expectable: Expectable[T]

  /** 
   * apply a Matcher to the expectable contained in that MatchResult. Depending on the exact type of the MatchResult,
   * that logic may vary.
   *
   * Note: this method is marked as "private" to give a clue to the user when semi-column inference fails to apply.
   *
   * For example in that case:
   *
   *    "string" must not beNull
   *    1 must_== 1
   *
   * There will be a compilation error as if the apply method was used between the first and second line:
   *    ("string" must not beNull).apply(1)
   *
   * For a more detailed explanation, see: http://bit.ly/12STc95
   *
   */
  private[specs2]
  def apply(m: Matcher[T]): MatchResult[T]

  /** alias for the apply method, to be used outside specs2 */
  def applyMatcher(m: Matcher[T]): MatchResult[T] = apply(m)

  /** @return the negation of this result */
  def negate: MatchResult[T]

  /** apply the matcher */
  def be(m: Matcher[T]) = {
    if (m == null) apply(new BeNull)
    else apply(m)
  }
  def be[S >: T <: AnyRef](s: S) = {
    apply(new BeTheSameAs(s))
  }
  /** apply the matcher */
  def have(m: Matcher[T]) = apply(m)
  def toResult: Result = evaluate.toResult

  def isSuccess = toResult.isSuccess
  def message = toResult.message
  def orThrow = this
  def orSkip = this

  /** @return the MatchResult with no messages */
  def mute: MatchResult[T] = this
  /** update the failure message of this match result */
  def updateMessage(f: String => String) = this
  /** filter the trace of this result (if there is one) */
  def filterTrace(f: List[StackTraceElement] => List[StackTraceElement]) = this
  /** set a new failure message on this match result */
  def setMessage(message: String) = updateMessage((s: String) => message)

  /** the value being matched */
  protected[specs2] def evaluate[S >: T]: MatchResult[S] = this
}

/**
 * The signature of this class constructor is unusual, with a useless implicit parameter.
 *
 * This is actually here to avoid overloading conflicts with the apply method in the companion object
 */
case class MatchSuccess[T] private[specs2](ok: () => String, ko: () => String, expectable: Expectable[T])(implicit p: Int = 0) extends MatchResult[T] {
  lazy val okMessage = ok()
  lazy val koMessage = ko()
  override def toResult = Success(okMessage)

  def negate: MatchResult[T] = MatchFailure(koMessage, okMessage, expectable)
  def apply(matcher: Matcher[T]): MatchResult[T] = expectable.applyMatcher(matcher)
  override def mute = MatchSuccess("", "", expectable)
  override def updateMessage(f: String => String) = copy(ok = () => f(okMessage), ko = () => f(koMessage))
}
object MatchSuccess {
  def apply[T](ok: =>String, ko: =>String, expectable: Expectable[T]) =
    new MatchSuccess(() => ok, () => ko, expectable)
}
case class MatchFailure[T] private[specs2](ok: () => String, ko: () => String, expectable: Expectable[T],
                                           trace: List[StackTraceElement] = Nil, details: Details = NoDetails) extends MatchResult[T] {
  def okMessage = ok()
  def koMessage = ko()

  /** an exception having the same stacktrace */
  lazy val exception = new Exception(koMessage)

  override def toResult = Failure(koMessage, okMessage, trace, details)

  def negate: MatchResult[T] = MatchSuccess(koMessage, okMessage, expectable)
  def apply(matcher: Matcher[T]): MatchResult[T] = expectable.applyMatcher(matcher)

  override def mute                               = copy(ok = () => "", ko = () => "", details = NoDetails)
  override def updateMessage(f: String => String) = copy(ok = () => f(okMessage), ko = () => f(koMessage))
  override def orThrow: MatchFailure[T]           = throw new FailureException(toResult)
  override def orSkip: MatchFailure[T]            = throw new SkipException(toResult)
}

object MatchFailure {
  def create[T](ok: =>String, ko: =>String, expectable: Expectable[T], trace: List[StackTraceElement], details: Details): MatchFailure[T] =
    new MatchFailure(() => ok, () => ko, expectable, trace, details)

  def create[T](ok: =>String, ko: =>String, expectable: Expectable[T], details: Details): MatchFailure[T] =
    create(ok, ko, expectable, Nil, details)

  def apply[T](ok: =>String, ko: =>String, expectable: Expectable[T]): MatchFailure[T] =
    create(ok, ko, expectable, Nil, NoDetails)
}

case class MatchSkip[T] private[specs2](override val message: String, expectable: Expectable[T]) extends MatchResult[T] {
  def negate: MatchResult[T] = this
  def apply(matcher: Matcher[T]): MatchResult[T] = expectable.applyMatcher(matcher)
  override def toResult = Skipped(message)
  override def mute = MatchSkip("", expectable)
  override def updateMessage(f: String => String) = MatchSkip(f(message), expectable)
  override def orThrow: MatchSkip[T] = throw new SkipException(toResult)
}
case class MatchPending[T] private[specs2](override val message: String, expectable: Expectable[T]) extends MatchResult[T] {
  def negate: MatchResult[T] = this
  def apply(matcher: Matcher[T]): MatchResult[T] = expectable.applyMatcher(matcher)
  override def toResult = Pending(message)
  override def mute = MatchPending("", expectable)
  override def updateMessage(f: String => String) = MatchPending(message, expectable)
  override def orThrow: MatchPending[T] = throw new PendingException(toResult)
}
case class NotMatch[T] private[specs2](m: MatchResult[T]) extends MatchResult[T] {
  val expectable = m.expectable
  override def evaluate[S >: T] = m
  def negate: MatchResult[T] = NeutralMatch(m)
  def apply(matcher: Matcher[T]): MatchResult[T] = m(matcher.not)
}
case class NeutralMatch[T] private[specs2](m: MatchResult[T]) extends MatchResult[T] {
  val expectable = m.expectable
  override def evaluate[S >: T] = m
  def negate: MatchResult[T] = NotMatch(m)
  def apply(matcher: Matcher[T]): MatchResult[T] = m(matcher)
}
class AndMatch[T] private[specs2](first: MatchResult[T], second: =>MatchResult[T]) extends MatchResult[T] {
  val expectable = m1.expectable
  lazy val m1 = first
  lazy val m2 = second
  override def evaluate[S >: T] = {
    m1 match {
      case f: MatchFailure[_] => m1
      case _ =>
        (m1, m2) match {
          case (_, NeutralMatch(_))                      => new AndMatch(m1, MatchSkip("", expectable))
          case (NeutralMatch(_), _)                      => new AndMatch(m2, MatchSkip("", expectable))
          case (NotMatch(_), NotMatch(_))                => new AndNotMatch(m1.evaluate, m2.evaluate)
          case (_, NotMatch(_))                          => new AndNotMatch(m1, MatchSkip("", expectable))
          case (NotMatch(_), _)                          => new AndMatch(m1.evaluate, m2).evaluate
          case (s: MatchSuccess[_], f: MatchFailure[_])  => m2
          case (s: MatchSuccess[_], _)                   => m1
          case (_, s: MatchSuccess[_])                   => m2
          case (_, _)                                    => m1
        }
    }
  }
  def negate: MatchResult[T] = new OrMatch(m1.not, m2.not).evaluate
  def apply(matcher: Matcher[T]): MatchResult[T] = m1 and m2(matcher)
  override def toResult = m1.toResult and m2.toResult
}
class AndNotMatch[T] private[specs2](first: MatchResult[T], second: =>MatchResult[T]) extends MatchResult[T] {
  val expectable = m1.expectable
  lazy val m1 = first
  lazy val m2 = second

  override def evaluate[S >: T] = m1 and m2.not
  def negate: MatchResult[T] = new OrMatch(m1.not, m2).evaluate
  def apply(matcher: Matcher[T]): MatchResult[T] = m1 and m2(matcher.not)
}
class OrMatch[T] private[specs2](first: MatchResult[T], second: =>MatchResult[T]) extends MatchResult[T] {
  val expectable = m1.expectable
  lazy val m1 = first
  lazy val m2 = second
  override def evaluate[S >: T] = {
    m1 match {
      // see MatchResultLogicalCombinatorsSpec for a case where OrMatches are nested together. see #233
      case om: OrMatch[_] if om.m1.isSuccess => new OrMatch(om.m1, MatchSkip("", expectable))
      case MatchSuccess(_, _, _)             => new OrMatch(m1, MatchSkip("", expectable))
      case _ => {
        (m1, m2) match {
          case (_, NeutralMatch(_))                       => new OrMatch(m1, MatchSkip("", expectable))
          case (NeutralMatch(_), _)                       => new OrMatch(m2, MatchSkip("", expectable))
          case (NotMatch(_), NotMatch(_))                 => new OrNotMatch(m1.evaluate, m2)
          case (_, NotMatch(_))                           => new OrNotMatch(m1, m2)
          case (NotMatch(_), _)                           => new OrMatch(m1.evaluate, m2).evaluate
          case (_, s: MatchSuccess[_])                    => m2
          case (f1: MatchFailure[_], f2: MatchFailure[_]) => MatchFailure.create(f1.okMessage+"; "+f2.okMessage, f1.koMessage+"; "+f2.koMessage, f1.expectable, f1.trace, f1.details)
          case (_, _) => m1
        }
      }
    }
  }
  def negate: MatchResult[T] = new AndMatch(m1.not, m2.not).evaluate
  def apply(matcher: Matcher[T]): MatchResult[T] = m1 or m2(matcher)
  override def toResult = m1.toResult or m2.toResult
}
class OrNotMatch[T] private[specs2](first: MatchResult[T], second: =>MatchResult[T]) extends MatchResult[T] {
  lazy val m1 = first
  lazy val m2 = second
  val expectable = m1.expectable
  override def evaluate[S >: T] = m1 or m2.not
  def negate: MatchResult[T] = new AndMatch(m1.not, m2).evaluate
  def apply(matcher: Matcher[T]): MatchResult[T] = m1 or evaluate(matcher.not)
}

/**
 * Utility functions for MatchResult.
 *
 * A MatchResult is a Functor where the fmap function acts on the embedded Expectable value (which itself is a Functor)
 */
object MatchResult {

  implicit val MatchResultFunctor: Functor[MatchResult] = new Functor[MatchResult] {
    def map[A, B](m: MatchResult[A])(f: A => B) = m match {
      case success: MatchSuccess[_] => success.map(f)
      case failure: MatchFailure[_] => failure.map(f)
      case skip: MatchSkip[_]       => skip.map(f)
      case pending: MatchPending[_] => pending.map(f)
      case neg: NotMatch[_]         => neg.map(f)
      case neutral: NeutralMatch[_] => neutral.map(f)
      case and: AndMatch[_]         => and.map(f)
      case andnot: AndNotMatch[_]   => andnot.map(f)
      case or: OrMatch[_]           => or.map(f)
      case ornot: OrNotMatch[_]     => ornot.map(f)
    }
  }
  implicit val MatchSuccessFunctor: Functor[MatchSuccess] = new Functor[MatchSuccess] {
    def map[A, B](m: MatchSuccess[A])(f: A => B) =
      MatchSuccess(m.okMessage, m.koMessage, m.expectable.map(f))
  }
  implicit val MatchFailureFunctor: Functor[MatchFailure] = new Functor[MatchFailure] {
    def map[A, B](m: MatchFailure[A])(f: A => B) = MatchFailure(m.okMessage, m.koMessage, m.expectable.map(f))
  }
  implicit val MatchSkipFunctor: Functor[MatchSkip] = new Functor[MatchSkip] {
    def map[A, B](m: MatchSkip[A])(f: A => B) = new MatchSkip(m.message, m.expectable.map(f))
  }
  implicit val MatchPendingFunctor: Functor[MatchPending] = new Functor[MatchPending] {
    def map[A, B](m: MatchPending[A])(f: A => B) = new MatchPending(m.message, m.expectable.map(f))
  }
  implicit val NotMatchFunctor: Functor[NotMatch] = new Functor[NotMatch] {
    def map[A, B](n: NotMatch[A])(f: A => B) = new NotMatch(n.m.map(f))
  }
  implicit val NeutralMatchFunctor: Functor[NeutralMatch] = new Functor[NeutralMatch] {
    def map[A, B](n: NeutralMatch[A])(f: A => B) = new NeutralMatch(n.m.map(f))
  }
  implicit val AndMatchFunctor: Functor[AndMatch] = new Functor[AndMatch] {
    def map[A, B](m: AndMatch[A])(f: A => B) = new AndMatch(m.m1.map(f), m.m2.map(f))
  }
  implicit val AndNotMatchFunctor: Functor[AndNotMatch] = new Functor[AndNotMatch] {
    def map[A, B](m: AndNotMatch[A])(f: A => B) = new AndNotMatch(m.m1.map(f), m.m2.map(f))
  }
  implicit val OrMatchFunctor: Functor[OrMatch] = new Functor[OrMatch] {
    def map[A, B](m: OrMatch[A])(f: A => B) = new OrMatch(m.m1.map(f), m.m2.map(f))
  }
  implicit val OrNotMatchFunctor: Functor[OrNotMatch] = new Functor[OrNotMatch] {
    def map[A, B](m: OrNotMatch[A])(f: A => B) = new OrNotMatch(m.m1.map(f), m.m2.map(f))
  }

  implicit def matchResultAsResult[M[_] <: MatchResult[_], T]: AsResult[M[T]] = new AsResult[M[T]] {
    def asResult(t: =>M[T]): Result = AsResult(t.toResult)
  }

  /** implicit typeclass instance to create examples from a sequence of MatchResults */
  implicit def matchResultSeqAsResult[T]: AsResult[Seq[MatchResult[T]]] = new AsResult[Seq[MatchResult[T]]] {
    def asResult(t: =>Seq[MatchResult[T]]): Result = t.foldLeft(StandardResults.success: Result)(_ and _.toResult)
  }

  /** sequence a list of MatchResults into a MatchResult of a list */
  def sequence[T](seq: Seq[MatchResult[T]]): MatchResult[Seq[T]] =
    Matcher.result(AsResult(seq), MustExpectations.createExpectable(seq.map(_.expectable.value)))
}

