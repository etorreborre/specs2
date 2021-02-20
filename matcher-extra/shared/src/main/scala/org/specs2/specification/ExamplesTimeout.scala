package org.specs2
package specification

import core.*
import execute.*
import concurrent.ExecutionEnv

import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

/**
 * This trait can be used to add a global time out to each example
 */
trait ExamplesTimeout extends SpecificationStructure:

  override def flatMap(f: Fragment): Fragments =
    f.updateExecutionWithEnv { (execution, env) =>
      val timeout = env.arguments.commandLine.intOr("timeout", 1000 * 60).millis
      execution.setTimeout(timeout)
    }
