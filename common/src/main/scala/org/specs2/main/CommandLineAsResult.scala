package org.specs2
package main

import execute._

/**
 * Typeclass for results depending on the command line
 */
trait CommandLineAsResult[R] {
  def asResult(commandLine: CommandLine, r: =>R): Result
}

object CommandLineAsResult {
  /** where a CommandLineAsResult is expected, if no instance can be found, just use AsResult without using the command line parameter */
  implicit def commandLineAsResultAsResult[R : AsResult]: CommandLineAsResult[R] = new CommandLineAsResult[R] {
    def asResult(commandLine: CommandLine, r: =>R): Result =
      AsResult(r)
  }

  def apply[R : CommandLineAsResult](r: =>R): CommandLine => Result =
    (commandLine: CommandLine) => implicitly[CommandLineAsResult[R]].asResult(commandLine, r)
}