package org.specs2
package mutable

import org.specs2.specification.core.{Env, OwnExecutionEnv}

class MutableSpec(val env: Env) extends Specification with OwnExecutionEnv {

  "s2 strings must create examples".p
  s2"""${ this.is.examplesList must haveSize(1) }"""

}
