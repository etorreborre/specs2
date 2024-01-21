package org.specs2
package mutable

import org.specs2.specification.core.{Env, OwnExecutionEnv}
import org.specs2.execute.{AsResult, Result}
import org.specs2.specification.AroundEach

class MutableSpec extends Specification with OwnExecutionEnv:

  "s2 strings must create examples".p
  s2"""${this.is.examplesList must haveSize(2)}"""

  "an execution can be used in a block even if an exception is thrown" >> pendingUntilFixed("it's ok") {
    throw new Exception("boom")
    (1 === 1).pendingUntilFixed("to force the conversion of the block to an Execution")
  }

// https://github.com/etorreborre/specs2/pull/981#issuecomment-911400717
// this used to throw a ClassCastException possibly due to a scala bug with implicits
class NestedSpec extends Specification {
  "This is a test for #981" >> {
    "Nested" >> {
      1 === 1
    }
  }
}

// #984 this again looks like a weird behaviour with implicits
// Prior to the fix which shuffled implicits around the var
// was being captured by the >> { ... } closure and never evaluated again
class ImplicitClosureMutableSpec extends Specification with AroundEach:
  private var value: Int = 0

  override def around[R: AsResult](r: =>R): Result =
    value = 1
    AsResult(r)

  "Around should use the same value" >> {
    value === 1
  }

class ImplicitClosureSpec extends org.specs2.Specification with AroundEach:
  def is = s2"""

  Around should use the same value $checkValue
  """

  private var value: Int = 0

  override def around[R: AsResult](r: =>R): Result =
    value = 1
    AsResult(r)

  def checkValue =
    value === 1
