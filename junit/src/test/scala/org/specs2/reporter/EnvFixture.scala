package org.specs2
package reporter

import execute.{Result, AsResult}
import specification.FixtureExample
import specification.core.Env

trait EnvFixture extends FixtureExample[Env] {
  protected def fixture[R : AsResult](f: Env => R): Result = {
    val env = Env()
    try AsResult(f(env))
    finally env.shutdown
  }
}
