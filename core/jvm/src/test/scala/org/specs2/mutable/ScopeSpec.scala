package org.specs2.mutable

import org.specs2.specification.core.Env

class ScopeSpec(env: Env) extends org.specs2.Specification:
  def is = s2"""

    A mutable specification can use the Scope trait to initialize values for each example $useScopeTrait
  """

  def useScopeTrait =
    // make sure that the arguments show all examples
    val arguments = org.specs2.main.Arguments("")
    val result = org.specs2.runner.TextRunner.run(new ScopeSpecification)(env.setArguments(arguments)).output
    (result must contain("+ This is an example which succeeds")) and
      (result must contain("x This is an example which fails"))

class ScopeSpecification extends org.specs2.mutable.Specification {
  "This is an example which succeeds" in new SpecScope:
    value === 1

  "This is an example which fails" in new SpecScope:
    value === 2
}

trait SpecScope extends org.specs2.execute.Scope:
  val value: Int = 1
