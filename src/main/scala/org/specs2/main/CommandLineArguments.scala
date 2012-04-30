package org.specs2
package main

/**
 * This trait can be mixed in with a Specification to hold command line arguments when the specification is created.
 * This allows to create fragments based on the arguments passed by the user on the command line
 */
trait CommandLineArguments extends DelayedInit {
  private var commandLineArguments = Arguments()
  private[specs2] var body: () => Unit = () => ()

  /**
   * set the command line arguments and trigger the body
   */
  def set(args: Arguments) {
    commandLineArguments = args
    body()
  }

  def delayedInit(x: =>Unit) {
    body = () => x
  }

  lazy val arguments = commandLineArguments
}
