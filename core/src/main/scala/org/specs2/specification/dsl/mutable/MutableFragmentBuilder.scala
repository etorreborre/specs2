package org.specs2
package specification
package dsl
package mutable

import main.Arguments
import control._
import execute.{Pending, Skipped}
import reflect.Classes
import specification.create.FragmentsFactory
import specification.core._

/**
 * Creation of fragments in a mutable specification
 *
 * This essentially works by keep a mutable ListBuffer of Fragments
 *
 * Arguments and title are also added with mutable variables
 *
 * Most of the complexity in that trait comes from the "isolated" mode of execution where we
 * want to be able to recreate some blocks and not others when running an example
 * in its own instance of the Specification
 *
 */
trait MutableFragmentBuilder extends FragmentBuilder
  with FragmentsFactory
  with MutableArgumentsBuilder
  with MutableHeaderBuilder { outer =>

  private[specs2] var specFragments = Fragments.empty

  def specificationFragments = {
    val fs = replayFragments

    // add 2 line breaks just after the specification title
    fs.prepend(List.fill(2)(fragmentFactory.break))
  }

  def is =
    SpecStructure.create(headerVar, argumentsVar, specificationFragments)

  private def replayFragments: Fragments = {
    targetPath.map(effects.replay).getOrElse(effects.record)
    effects.clear
    specFragments
  }

  /** when a target path is specified we might limit the creation of fragments to only the fragments on the desired path */
  private[specs2] var targetPath: Option[EffectPath] = None

  private val effects = EffectBlocks()

  def addFragmentBlock(f: =>Fragment): Fragment = {
    effects.nestBlock(f)
    fragmentFactory.end
  }

  def addFragmentsBlock(fs: =>Fragments): Fragments = {
    effects.nestBlock(fs)
    Fragments.empty
  }

  def addFragment(fragment: Fragment): Fragment = {
    effects.addBlock {
      specFragments = specFragments.append(isolate(fragment, effects.effectPath))
    }

    fragment
  }

  def addFragments(fragments: Fragments): Fragments = {
    specFragments = specFragments.append(fragments)
    fragments
  }

  private def isolate(fragment: Fragment, effectPath: EffectPath) =
    if (mustBeIsolated(fragment))
      fragment.setExecution(duplicateExecution(effectPath))
    else
      fragment

  private def mustBeIsolated(fragment: Fragment) = {
    fragment.isExecutable                     &&
    fragment.execution.isolable               &&
    argumentsVar.isolated                     &&
    targetPath.isEmpty
  }

  private def duplicateExecution(effectPath: EffectPath): Execution = Execution.withEnvFlatten { env =>
    val instance = runOperation(Classes.createInstanceFromClass[MutableFragmentBuilder](getClass.asInstanceOf[Class[MutableFragmentBuilder]],
      getClass.getClassLoader, env.defaultInstances), env.systemLogger)

      instance match {
        case Left(e) =>
          Execution.result(e.fold(t => org.specs2.execute.Error(t), m => org.specs2.execute.Error(m)))

        case Right(newSpec) =>
          newSpec.targetPath = Some(effectPath)
          val pathFragments = newSpec.replayFragments
          val previousSteps = pathFragments.filter(f => Fragment.isStep(f) && f.execution.isolable)

          val previousStepExecutions = previousSteps.collect {
            case f if Fragment.isStep(f) && f.execution.isolable => f.execution
          }.runList.runOption(env.executionEnv).getOrElse(Nil)

          val isolatedExecution: Execution = pathFragments.fragments.map(_.lastOption.map(_.execution).
            getOrElse(Execution.executed(Pending("isolation mode failure - could not find an isolated fragment to execute")))).
            runOption(env.executionEnv).
            getOrElse(Execution.executed(Skipped(s"isolation mode failure - could not produce an isolated execution for effect path $effectPath")))

          isolatedExecution.afterSuccessfulSequential(previousStepExecutions)
      }

  }

}

trait FragmentBuilder {
  def addFragment(f: Fragment): Fragment
  def addFragments(fs: Fragments): Fragments
  def addFragmentBlock(block: =>Fragment): Fragment
  def addFragmentsBlock(block: =>Fragments): Fragments
}

trait MutableHeaderBuilder {
  private[specs2] var headerVar = new SpecHeader(specClass = getClass)
  def setTitle(t: String) = {
    headerVar = headerVar.copy(title = Some(t))
    headerVar
  }
}

trait MutableArgumentsBuilder {
  private[specs2] var argumentsVar = Arguments()
  def updateArguments(a: Arguments): Arguments = { argumentsVar = argumentsVar <| a; a }
  def setArguments(a: Arguments): Arguments = { argumentsVar = a; a }
}
