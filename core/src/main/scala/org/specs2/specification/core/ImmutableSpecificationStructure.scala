package org.specs2
package specification
package core

import control.Status
import reflect.Classes
import execute._
import ResultLogicalCombinators._

trait ImmutableSpecificationStructure extends SpecificationStructure {
  override def structure = (env: Env) => {
    val specStructure = super.structure(env)
    val arguments = env.arguments <| specStructure.arguments

    if (!arguments.isolated || env.executionEnv.withoutIsolation) specStructure
    else                                                          specStructure.map(isolateExamples(env))
  }

  private def isolateExamples(env: Env): Fragments => Fragments = (fs: Fragments) => {
    val isolated = fs.fragments.zipWithIndex.map { case (f @ Fragment(d, e, l), i) =>
      if (e.isRunnable && f.execution.isolable) isolate(fs, f, i, env)
      else f
    }
    Fragments(isolated:_*)
  }

  private def isolate(fs: Fragments, f: Fragment, position: Int, env: Env): Fragment = {
    val isolated =
      Execution.result {
        val instance = Classes.createInstance[ImmutableSpecificationStructure](getClass.asInstanceOf[Class[ImmutableSpecificationStructure]], getClass.getClassLoader).execute(env.systemLogger).unsafePerformIO
        instance.toDisjunction.fold(
          e => org.specs2.execute.Error(Status.asException(e)),
          { newSpec =>

           val newFragments = newSpec.fragments(env.setWithoutIsolation)
           val previousStepsExecution =
             newFragments.fragments.take(position)
               .filter(f => Fragment.isStep(f) && f.execution.isolable)
               .foldLeft(Success(): Result) { _ and _.execution.execute(env).result }

           val isolatedExecution = newFragments.fragments(position).execution

           if (isolatedExecution.isRunnable) previousStepsExecution and isolatedExecution.execute(env).result
           else                              previousStepsExecution
          })
      }
    f.setExecution(isolated)
  }
}
