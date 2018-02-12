package org.specs2
package specification
package core

import java.util.concurrent.ExecutorService

import concurrent.ExecutionEnv
import main._

import scala.concurrent._, duration._
import scala.reflect.ClassTag

class DependencyInjectionSpec(environment: Env) extends Specification { def is = s2"""

 Different parts of the environment can be injected in the specification

  immutable case
    the Env               $env
    the ExecutionEnv      $executionEnv
    the ExecutionContext  $executionContext
    the ExecutorService   $executorService
    the Arguments         $arguments
    the CommandLine       $commandLine

  mutable case
    the Env               $env_
    the ExecutionEnv      $executionEnv_
    the ExecutionContext  $executionContext_
    the ExecutorService   $executorService_
    the Arguments         $arguments_
    the CommandLine       $commandLine_

"""
  import SpecificationStructure._

  def env              = createSpecOk[Spec1]
  def executionEnv     = createSpecOk[Spec2]
  def executionContext = createSpecOk[Spec3]
  def executorService  = createSpecOk[Spec4]
  def arguments        = createSpecOk[Spec5]
  def commandLine      = createSpecOk[Spec6]

  def env_              = createSpecOk[SpecM1]
  def executionEnv_     = createSpecOk[SpecM2]
  def executionContext_ = createSpecOk[SpecM3]
  def executorService_  = createSpecOk[SpecM4]
  def arguments_        = createSpecOk[SpecM5]
  def commandLine_      = createSpecOk[SpecM6]

  def createSpecOk[C : ClassTag] =
    create(implicitly[ClassTag[C]].runtimeClass.getName, env = Some(environment)) must not(throwAn[Exception])

  case class Spec1(env: Env) extends Specification { def is = "test" ! ok }
  case class Spec2()(implicit ee: ExecutionEnv) extends Specification { def is = "test" ! { Await.result(Future(ok)(ee.executionContext), 1 second) } }
  case class Spec3()(implicit ec: ExecutionContext) extends Specification { def is = "test" ! Await.result(Future(ok), 1 second) }
  case class Spec4()(implicit es: ExecutorService) extends Specification { def is = "test" ! Await.result(Future(ok)(ExecutionContext.fromExecutor(es)), 1 second) }
  case class Spec5(arguments : Arguments) extends Specification { def is = "test" ! ok }
  case class Spec6(cl: CommandLine) extends Specification { def is = "test" ! ok }

  case class SpecM1(env1: Env)                     extends org.specs2.mutable.Specification { "test" >> ok }
  case class SpecM2()(implicit ee: ExecutionEnv)     extends org.specs2.mutable.Specification { "test" >> Await.result(Future(ok)(ee.executionContext), 1 second) }
  case class SpecM3()(implicit ec: ExecutionContext) extends org.specs2.mutable.Specification { "test" >> Await.result(Future(ok), 1 second) }
  case class SpecM4()(implicit es: ExecutorService)  extends org.specs2.mutable.Specification { "test" >> Await.result(Future(ok)(ExecutionContext.fromExecutor(es)), 1 second) }
  case class SpecM5(args1: Arguments)              extends org.specs2.mutable.Specification { "test" >> ok }
  case class SpecM6(cl: CommandLine)               extends org.specs2.mutable.Specification { "test" >> ok }

}
