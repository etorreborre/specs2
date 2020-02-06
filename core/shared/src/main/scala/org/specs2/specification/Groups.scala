package org.specs2
package specification

import execute._
import control.ImplicitParameters._
import org.specs2.concurrent.ExecutionEnv
import org.specs2.control.Use
import specification.core._
import specification.create._

import scala.concurrent.Future

trait GroupsLike { this: S2StringContextCreation =>
  def createExamplesGroup(i: Int): ExamplesGroup
}

case class ExamplesGroup(private var name: String = "") {
  def groupName = name

  def nameIs(n: String) = { name = s"'$n'"; this }

  val e1:  ExecutionVar = new ExecutionVar()
  val e2:  ExecutionVar = new ExecutionVar()
  val e3:  ExecutionVar = new ExecutionVar()
  val e4:  ExecutionVar = new ExecutionVar()
  val e5:  ExecutionVar = new ExecutionVar()
  val e6:  ExecutionVar = new ExecutionVar()
  val e7:  ExecutionVar = new ExecutionVar()
  val e8:  ExecutionVar = new ExecutionVar()
  val e9:  ExecutionVar = new ExecutionVar()
  val e10: ExecutionVar = new ExecutionVar()
  val e11: ExecutionVar = new ExecutionVar()
  val e12: ExecutionVar = new ExecutionVar()
  val e13: ExecutionVar = new ExecutionVar()
  val e14: ExecutionVar = new ExecutionVar()
  val e15: ExecutionVar = new ExecutionVar()
  val e16: ExecutionVar = new ExecutionVar()
  val e17: ExecutionVar = new ExecutionVar()
  val e18: ExecutionVar = new ExecutionVar()
  val e19: ExecutionVar = new ExecutionVar()
  val e20: ExecutionVar = new ExecutionVar()
  val e21: ExecutionVar = new ExecutionVar()
  val e22: ExecutionVar = new ExecutionVar()

  protected lazy val numberedExamples = Seq(e1,e2,e3,e4,e5,e6,e7,e8,e9,e10,e11,e12,e13,e14,e15,e16,e17,e18,e19,e20,e21,e22)

  def createExample(i: Int) = numberedExamples(i)
}

class ExecutionVar(var execution: () => Execution = () => Execution.result(new execute.Pending(s" - PENDING"))) {
  def :=[T : AsResult](t: =>T) = {
    execution = () => Execution.result(t)
    this
  }

  def :=[R : AsResult](f: Env => R) = {
    execution = () => Execution.withEnv(f)
    this
  }

  def :=(other: ExecutionVar) = {
    execution = () => other.execution()
    this
  }
}
object ExecutionVar {
  def result[R : AsResult](r: =>R) =
    new ExecutionVar := r

  def futureResult[R](f: =>Future[R])(implicit asResult: AsResult[R], p1: ImplicitParam1) = {
    Use(p1)
    new ExecutionVar(() => Execution.withEnvAsync(_ => f))
  }

  def withEnv[R : AsResult](f: Env => R) =
    new ExecutionVar(() => Execution.withEnv(f))

  def withExecutionEnv[R : AsResult](f: ExecutionEnv => R) =
    new ExecutionVar(() => Execution.withExecutionEnv(f))

}
