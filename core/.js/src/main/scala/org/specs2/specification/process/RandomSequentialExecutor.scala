package org.specs2
package specification
package process

import control.producer.*, Producer.*
import core.*
import main.Arguments


/**
 * Delegate to the SteppedExecutor for ScalaJS since the
 * RandomSequentialExecutor requires scala.concurrent for now
 */
case class RandomSequentialExecutor(env: Env) extends Executor:
  /**
   * find sequences of concurrent examples in between steps
   * and scramble them
   */
  def execute(specArguments: Arguments): AsyncTransducer[Fragment, Fragment] =
    SteppedExecutor(env).execute(specArguments)
