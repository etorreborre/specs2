package org.specs2
package specification.mutable

import main.CommandLine
import specification.core.Env
import specification.core.mutable.SpecificationStructure

trait CommandLineArguments extends SpecificationStructure {
  def is(commandLine: CommandLine): Any

  override def structure = (env: Env) => {
    // trigger the creation of the spec structure through side-effects
    is(env.arguments.commandLine)
    // use what the is method returns
    decorate(is, env)
  }
}

trait Environment extends SpecificationStructure {
  def is(env: Env): Any

  override def structure = (env: Env) => {
    // trigger the creation of the spec structure through side-effects
    is(env)
    // use what the is method returns
    decorate(is, env)
  }
}
