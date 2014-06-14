package org.specs2.specification.core

import org.specs2.execute.{AsResult, Result}
import org.specs2.specification.ForEach

trait ForEachEnv extends ForEach[Env] {
  protected def foreach[R : AsResult](f: Env => R): Result = {
    val env = Env()
    try AsResult(f(env))
    finally env.shutdown
  }
}

