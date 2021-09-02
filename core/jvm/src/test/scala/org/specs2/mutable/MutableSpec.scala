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

class NestedSpec extends Specification {
  "This is a test for #981" >> {
    // https://github.com/etorreborre/specs2/pull/981#issuecomment-911400717
    // this used to throw a ClassCastException possibly to a scala bug
    "Nested" >> {
      1 ==== 1
    }
  }
}
