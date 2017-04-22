package org.specs2
package specification
package core

import control._
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
  private def isolateExamples(env: Env): Fragments => Fragments = (fs: Fragments) => {
    val isolated = fs.fragments.zipWithIndex.map { case (f @ Fragment(d, e, l), i) =>
      if (e.isExecutable && f.execution.isolable)
        isolate(fs, f, i, env)
      else
        f
    }
    Fragments(isolated:_*)
  }

  /**
   * Isolate the execution of a Fragment so that it is executed in a brand new Specification instance
   */
  private def isolate(fs: Fragments, fragment: Fragment, position: Int, env: Env): Fragment = {
    val instance = runOperation(Classes.createInstanceFromClass[ImmutableSpecificationStructure](
      getClass.asInstanceOf[Class[ImmutableSpecificationStructure]],
      getClass.getClassLoader,
      env.defaultInstances), env.systemLogger)

    fragment.setExecution {
      instance match {
        case Left(e) =>
          Execution.result(e.fold(t => org.specs2.execute.Error(t), m => org.specs2.execute.Error(m)))

        case Right(newSpec) =>
          val newFragments = newSpec.fragments(env.setWithoutIsolation)
          val previousStepExecutions = newFragments.fragments.take(position).toList.collect {
            case f if Fragment.isStep(f) && f.execution.isolable => f.execution
          }
          val isolated = newFragments.fragments(position).execution
          isolated.afterSuccessfulSequential(previousStepExecutions)(env)
      }
    }
  }
}
