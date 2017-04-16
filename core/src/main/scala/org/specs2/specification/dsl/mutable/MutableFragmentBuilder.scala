package org.specs2
package specification
package dsl
package mutable

import main.Arguments
import control._
import producer._
import execute.{Pending, Result, ResultLogicalCombinators, Success}
import ResultLogicalCombinators._
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

  private[specs2] val specFragments = new scala.collection.mutable.ListBuffer[Fragment]

  def specificationFragments = (env: Env) => {
    val content = {
      List.fill(2)(fragmentFactory.break) ++ // add 2 line breaks just after the specification title
      replayFragments(env).toList
    }
    Fragments(producers.emit[ActionStack, Fragment](content))
  }

  def specificationStructure = (env: Env) =>
    SpecStructure.create(headerVar, argumentsVar, specificationFragments(env))

  private def replayFragments(environment: Env) = {
    env = environment
    targetPath.map(effects.replay).getOrElse(effects.record)
    effects.clear
    specFragments
  }

  /** when a target path is specified we might limit the creation of fragments to only the fragments on the desired path */
  private[specs2] var targetPath: Option[EffectPath] = None

  private val effects = EffectBlocks()

  private[specs2] var env: Env = Env()

  def addFragmentBlock(f: =>Fragment) = {
    effects.nestBlock(f)
    fragmentFactory.end
  }

  def addFragmentsBlock(fs: =>Fragments) = {
    effects.nestBlock(fs)
    Fragments()
  }

  def addFragment(fragment: Fragment): Fragment = {
    effects.addBlock {
      if (targetPath.isDefined) specFragments.append(fragment)
      else                      specFragments.append(isolate(fragment, effects.effectPath))
    }

    fragment
  }

  def addFragments(fragments: Fragments): Fragments = {
    fragments.fragments.foreach(addFragment)
    fragments
  }

  private def isolate(fragment: Fragment, effectPath: EffectPath) =
    if (targetPath.isEmpty && mustBeIsolated(fragment))
      fragment.setExecution(duplicateExecution(effectPath))
    else
      fragment

  private def mustBeIsolated(fragment: Fragment) = {
    fragment.isExecutable                     &&
    fragment.execution.isolable               &&
    argumentsVar.isolated                     &&
    !env.executionParameters.withoutIsolation
  }

  private def duplicateExecution(effectPath: EffectPath) = {
    Execution.withEnvSync { env: Env =>

      def instance =
        runOperation(Classes.createInstanceFromClass[MutableFragmentBuilder](getClass.asInstanceOf[Class[MutableFragmentBuilder]],
          getClass.getClassLoader, env.defaultInstances),
          env.systemLogger)

      instance.fold(
        e => e.fold(t => org.specs2.execute.Error(t), m => org.specs2.execute.Error(m)),
        newSpec => {
          newSpec.targetPath = Some(effectPath)
          val pathFragments = newSpec.replayFragments(env)
          val previousSteps = pathFragments.filter(f => Fragment.isStep(f) && f.execution.isolable)
          val isolatedExecution = pathFragments.lastOption.map(_.execution).
            getOrElse(Execution.executed(Pending("isolation mode failure - could not find an isolated fragment to execute")))

          if (previousSteps.nonEmpty) {
            val previousStepsExecution = previousSteps.foldLeft(Success(): Result)(_ and _.execution.execute(outer.env).result)
            previousStepsExecution and isolatedExecution.execute(outer.env).result
          } else isolatedExecution.execute(outer.env).result
        }
      )
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
