package org.specs2
package specification

import org.specs2.execute.Result._
import org.specs2.execute.{ResultExecution, Result, AsResult}
import org.specs2.matcher.StoredExpectations
import scalaz.Monad


/**
 * generic trait for Before, After, Around
 */
trait Context extends Scope {
  def apply[T : AsResult](a: =>T): Result
}

object Context {
  def compose(c1: Context, c2: Context): Context = new Context {
    def apply[T : AsResult](a: =>T): Result = c1(c2(a))
  }
}

/**
 * The Before trait can be inherited by classes representing a context
 * where an action must be executing before the main executable action
 *
 * @see Example to understand why the type T must : AsResult
 */
trait Before extends Context { outer =>

  /** override this method to provide the before behavior */
  def before: Any
  /**
   * execute an action returning a Result
   * and finally the before action.
   *
   * The action will be aborted if the before block fails:
   *
   * - with an exception
   * - with a non-Success result
   * - with a non-Success match result
   */
  override def apply[T : AsResult](a: =>T): Result =
    ResultExecution.execute(before)((any: Any) => AsResult(a))

  /** compose the actions of 2 Before traits */
  def compose(b: Before): Before = new Before {
    def before = { b.before; outer.before }
  }

  /** sequence the actions of 2 Before traits */
  def andThen(b: Before): Before = new Before {
    def before = { outer.before; b.before }
  }

}

/**
 * The After trait can be inherited by classes representing a context
 * where an action must be executing after the main executable action
 *
 * @see Example to understand why the type T must : AsResult
 */
trait After extends Context { outer =>

  /** override this method to provide the after behavior */
  def after: Any
  /**
   * execute an action returning a Result
   * and finally the after action
   */
  def apply[T : AsResult](a: =>T): Result = {
    try { AsResult(a) }
    finally { after  }
  }

  /** compose the actions of 2 After traits */
  def compose(a: After): After = new After {
    def after = { a.after; outer.after }
  }

  /** sequence the actions of 2 After traits */
  def andThen(a: After): After = new After {
    def after = { outer.after; a.after }
  }

}

trait BeforeAfter extends Before with After { outer =>
  override def apply[T : AsResult](a: =>T): Result = {
    lazy val result = super[Before].apply(a)
    super[After].apply(result)
  }

  /** compose the actions of 2 BeforeAfter traits */
  def compose(b: BeforeAfter): BeforeAfter = new BeforeAfter {
    def before = { b.before; outer.before }
    def after = { b.after; outer.after}
  }

  /** sequence the actions of 2 BeforeAfter traits */
  def andThen(b: BeforeAfter): BeforeAfter = new BeforeAfter {
    def before = { outer.before; b.before }
    def after = { outer.after; b.after}
  }
}

/**
 * The Around trait can be inherited by classes which will
 * execute some code inside the around method provided by the context.
 *
 * This can be used for example to execute some code inside a webapp session
 *
 * @see Example to understand why the type T must : AsResult
 */
trait Around extends Context { outer =>

  def around[T : AsResult](t: =>T): Result
  def apply[T : AsResult](a: =>T) = around(a)

  /** compose the actions of 2 Around traits */
  def compose(a: Around): Around = new Around {
    def around[T : AsResult](t: =>T): Result = {
      a.around(outer.around(t))
    }
  }

  /** sequence the actions of 2 Around traits */
  def andThen(a: Around): Around = new Around {
    def around[T : AsResult](t: =>T): Result = {
      outer.around(a.around(t))
    }
  }
}

/**
 * A Fixture can be implicitly passed to a set of examples taking a function as an input.
 *
 * It can effectively act as an Outside and an Around context
 */
trait Fixture[T] {
  def apply[R : AsResult](f: T => R): Result
}

object Fixture {
  implicit def fixtureHasMonad: Monad[Fixture] = new Monad[Fixture] {
    def point[A](a: =>A) = new Fixture[A] {
      def apply[R : AsResult](f: A => R): Result = AsResult(f(a))
    }
    def bind[A, B](fixture: Fixture[A])(fa: A => Fixture[B]): Fixture[B] = new Fixture[B] {
      def apply[R : AsResult](fb: B => R): Result = fixture((a: A) => fa(a)(fb))
    }
  }
}

/**
 * Marker trait that is used to create a new scope with variables and implicitly converted to a Success in a mutable
 * Specification
 */
trait Scope extends matcher.Scope

/**
 * This class is used to evaluate a Context as a sequence of results by side-effects.
 *
 * @see the AllExpectations trait for its use
 */
class ResultsContext(results: =>Seq[Result]) extends StoredResultsContext {
  def storedResults = results
}

/**
 * This trait can be used when it is not desirable to use the AllExpectations trait, that is, when the specification
 * examples must be executed concurrently and not isolated.
 *
 * @see the UserGuide on how to use this trait
 */
trait StoredExpectationsContext extends StoredExpectations with StoredResultsContext

/**
 * This trait is a context which will use the results provided by the class inheriting that trait.
 * It evaluates the result of an example, which is supposed to create side-effects
 * and returns the 'storedResults' as the summary of all results
 */
trait StoredResultsContext extends Context { this: { def storedResults: Seq[Result]} =>
  def apply[T : AsResult](r: =>T): Result = {
    // evaluate r, triggering side effects
    val asResult = AsResult(r)
    val results = storedResults
    // if the execution returns an Error or a Failure that was created for a thrown
    // exception, like a JUnit assertion error or a NotImplementedError
    // then add the result as a new issue
    if (asResult.isError || asResult.isThrownFailure) issues(results :+ asResult, "\n")
    else                                              issues(results, "\n")
  }
}
