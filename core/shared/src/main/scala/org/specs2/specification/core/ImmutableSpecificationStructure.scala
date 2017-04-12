package org.specs2
package specification
package core

import control._, Actions._
import reflect.Classes

/**
 * Structure of an immutable specification.
 *
 * It may depend on the current environment.
 *
 * If the examples need to be executed in their own instance of the specification
 * they will be "isolated"
 */
trait ImmutableSpecificationStructure extends SpecificationStructure {

  override def structure = (env: Env) => {
    val specStructure = super.structure(env)
    val arguments = env.arguments <| specStructure.arguments
    if (!arguments.isolated || env.executionParameters.withoutIsolation)
      specStructure
    else
      specStructure.map(isolateExamples(env))
  }

  /**
   * Isolate the execution of each fragment if it is executable and isolable
   */
  private def isolateExamples(env: Env): Fragments => Fragments = (fragments: Fragments) =>
    Fragments(fragments.contents.zipWithIndex.mapEval {
      case (f @ Fragment(d, e, l), i) =>
        if (e.isExecutable && f.execution.isolable)
          isolate(f, i, env)
        else
          ok(f)
    })

  /**
   * Isolate the execution of a Fragment so that it is executed in a brand new Specification instance
   */
  private def isolate(fragment: Fragment, position: Int, env: Env): Action[Fragment] = {
    protect {
      lazy val instance = runOperation(Classes.createInstanceFromClass[ImmutableSpecificationStructure](
        getClass.asInstanceOf[Class[ImmutableSpecificationStructure]],
        env.defaultInstances), env.systemLogger)

      val execution: Action[Execution] =
        instance match {
          case Left(e) =>
            ok(Execution.result(e.fold(t => org.specs2.execute.Error(t), m => org.specs2.execute.Error(m))))

          case Right(newSpec) =>
            newSpec.fragments(env.setWithoutIsolation).fragments.map { fs =>
              val previousStepExecutions = fs.take(position).collect {
                case f if Fragment.isStep(f) && f.execution.isolable => f.execution
              }
              val isolated = fs(position).execution
              isolated.afterSuccessfulSequential(previousStepExecutions)
            }
        }

      execution.map(fragment.setExecution)
    }.flatten
  }
}
