package org.specs2
package specification

import main.CommandLine
import core.{Env, SpecStructure, SpecificationStructure}

trait CommandLineArguments extends SpecificationStructure {
  def is: SpecStructure = SpecStructure.empty(getClass)
  def is(commandLine: CommandLine): SpecStructure
  override def structure = (env: Env) => decorate(is(env.arguments.commandLine), env)
}

trait Environment extends SpecificationStructure {
  def is: SpecStructure = SpecStructure.empty(getClass)
  def is(env: Env): SpecStructure
  override def structure = (env: Env) => decorate(is(env), env)
}

