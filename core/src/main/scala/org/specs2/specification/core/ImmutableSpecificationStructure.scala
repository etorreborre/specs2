package org.specs2
package specification
package core

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
        val newSpec = getClass.newInstance
        val duplicatedFragment = newSpec.fragments(env.setWithoutIsolation).fragments(position)
        val isolatedExecution = duplicatedFragment.execution
        if (isolatedExecution.isRunnable) isolatedExecution.execute(env).result
        else                              org.specs2.execute.Success()
      }
    f.setExecution(isolated)
  }
}
