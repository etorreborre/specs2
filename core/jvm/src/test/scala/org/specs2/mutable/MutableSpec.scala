package org.specs2
package mutable

import org.specs2.specification.core.{Env, OwnExecutionEnv}

class MutableSpec(val env: Env) extends Specification with OwnExecutionEnv {

  "s2 strings must create examples".p
  s2"""${ this.is.examplesList must haveSize(2) }"""

  "an execution can be used in a block even if an exception is thrown" >> pendingUntilFixed("it's ok") {
    throw new Exception("boom")
    (1 ==== 1).pendingUntilFixed("to force the conversion of the block to an Execution")
  }

}
