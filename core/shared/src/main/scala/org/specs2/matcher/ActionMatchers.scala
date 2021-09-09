package org.specs2
package matcher

import control.*
import execute.*
import text.Regexes.*
import concurrent.*

/** Matchers for Action values
  */
trait ActionMatchers extends ValueChecks:

  private val ee: ExecutionEnv =
    ExecutionEnv.fromGlobalExecutionContext

  def beOk[T]: Matcher[Action[T]] =
    Matcher { (action: Action[T]) =>
      AsResult(action.runAction(ee).fold(t => Error(t), _ => Success()))
    }

  def beOk[T, R: AsResult](f: T => R): Matcher[Action[T]] =
    Matcher { (action: Action[T]) =>
      AsResult(action.runAction(ee).fold(t => Error(t), t => AsResult(f(t))))
    }

  def beOk[T](check: ValueCheck[T]): Matcher[Action[T]] =
    Matcher { (action: Action[T]) =>
      AsResult(action.runAction(ee).fold(t => Error(t), check.check))
    }

  def beOkWithValue[T](t: T): Matcher[Action[T]] =
    beOk(new BeEqualTo(t))

  def beKo[T]: Matcher[Action[T]] =
    Matcher { (action: Action[T]) =>
      action
        .runAction(ee)
        .fold(
          e => Success(),
          ok => Failure("a failure was expected")
        )
    }

  def beKo[T](message: String): Matcher[Action[T]] =
    Matcher { (action: Action[T]) =>
      action
        .runAction(ee)
        .fold(
          throwable =>
            if throwable.getMessage `matchesSafely` message then Success()
            else Failure(s"the action failed with message ${throwable.getMessage}. Expected: $message"),
          ok => Failure(s"a failure with message $message was expected")
        )
    }

object ActionMatchers extends ActionMatchers
