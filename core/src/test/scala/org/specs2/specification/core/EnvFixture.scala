package org.specs2
package specification
package core

import execute._

trait EnvFixture extends FixtureExample[Env] {
  protected def fixture[R : AsResult](f: Env => R): Result = {
    val env = Env()
    try AsResult(f(env))
    finally env.shutdown
  }
}

