package org.specs2.specification.core

import org.specs2.execute.{AsResult, Result}
import org.specs2.specification.FixtureExample

trait EnvFixture extends FixtureExample[Env] {
  protected def fixture[R : AsResult](f: Env => R): Result = {
    val env = Env()
    try AsResult(f(env))
    finally env.shutdown
  }
}

