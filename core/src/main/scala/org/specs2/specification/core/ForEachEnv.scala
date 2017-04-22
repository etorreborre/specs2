package org.specs2.specification.core

import org.specs2.execute.{AsResult, Result}
import org.specs2.specification.ForEach

/**
 * Specialized ForEach trait to use the Env in examples
 */
trait ForEachEnv extends ForEach[Env] {
  private lazy val foreachEnv = Env()
  protected def foreach[R : AsResult](f: Env => R): Result = {
    AsResult(f(foreachEnv))
  }
}

