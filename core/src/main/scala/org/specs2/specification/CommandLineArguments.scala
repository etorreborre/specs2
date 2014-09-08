package org.specs2
package specification

import main.CommandLine
import core.{Env, SpecStructure, SpecificationStructure}

/**
 * The CommandLineArgument trait can be mixed-in an Acceptance Specification to
 * access the command line arguments when defining the specification body
 * with the `def is(commandLine: CommandLine)` method
 */
trait CommandLineArguments extends SpecificationStructure {
  def is: SpecStructure = SpecStructure.empty(getClass)
  def is(commandLine: CommandLine): SpecStructure
  override def structure = (env: Env) => decorate(is(env.arguments.commandLine), env)
}

/**
 * The Environment trait can be mixed-in an Acceptance Specification to
 * access the Env object when defining the specification body
 * with the `def is(env: Env)` method
 */
trait Environment extends SpecificationStructure {
  def is: SpecStructure = SpecStructure.empty(getClass)
  def is(env: Env): SpecStructure
  override def structure = (env: Env) => decorate(is(env), env)
}

