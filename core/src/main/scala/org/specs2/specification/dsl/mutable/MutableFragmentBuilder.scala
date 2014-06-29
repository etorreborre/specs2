package org.specs2
package specification
package dsl
package mutable

import main.Arguments
import org.specs2.control.Status
import org.specs2.reflect.Classes
import scalaz.stream._
import specification.create.FragmentsFactory
import specification.core._

trait MutableFragmentBuilder extends FragmentBuilder
  with FragmentsFactory
  with MutableArgumentsBuilder
  with MutableHeaderBuilder {

  private[specs2] val specFragments = new scala.collection.mutable.ListBuffer[Fragment]

  def specificationFragments = (env: Env) => {
    val content = {
      fragmentFactory.break +: // add a line break just after the specification title
      replayFragments(env).toSeq
    }
    Fragments(Process.emitAll(content).toSource)
  }

  def specificationStructure = (env: Env) =>
    SpecStructure(header, arguments, specificationFragments(env))

  private def replayFragments(environment: Env) = {
    env = environment
    effects.replay
    specFragments
  }

  /** when a target path is specified we might limit the creation of fragments to only the fragments on the desired path */
  private[specs2] var targetPath: Option[EffectPath] = None

  private val effects = new EffectBlocks

  private[specs2] var env: Env = Env()

  def addFragmentBlock(f: =>Fragment) = {
    effects.nestBlock(effects.addBlock(f))
    fragmentFactory.end
  }

  def addFragmentsBlock(fs: =>Fragments) = {
    effects.nestBlock(effects.addBlock(fs))
    Fragments()
  }

  def addFragment(fragment: Fragment): Fragment = {
    effects.addBlock {
      if (effects.isAt(targetPath)) {
        effects.stopEffects
        specFragments.append(fragment)
      }
      else
        specFragments.append(isolate(fragment, effects.effectPath))
    }
    fragment
  }

  private def isolate(fragment: Fragment, effectPath: EffectPath) =
    if (!targetPath.isDefined && mustBeIsolated(fragment))
      fragment.setExecution(duplicateExecution(effectPath))
    else
      fragment

  private def mustBeIsolated(fragment: Fragment) = {
    fragment.isRunnable               &&
    fragment.execution.isolable       &&
    arguments.isolated                &&
    !env.executionEnv.withoutIsolation
  }

  private def duplicateExecution(effectPath: EffectPath) = {
    Execution.withEnv { env: Env =>
      val instance = Classes.createInstance[MutableFragmentBuilder](getClass.asInstanceOf[Class[MutableFragmentBuilder]], getClass.getClassLoader).execute(env.systemLogger).unsafePerformIO
      instance.toDisjunction.fold(
      e => org.specs2.execute.Error(Status.asException(e)),
      { newSpec =>
        newSpec.targetPath = Some(effectPath)

        val lastCreatedFragment = newSpec.replayFragments(env).last
        val isolatedExecution = lastCreatedFragment.execution

        if (isolatedExecution.isRunnable) isolatedExecution.execute(env).result
        else                              org.specs2.execute.Success()
      })
    }
  }

}

trait FragmentBuilder {
  def addFragment(f: Fragment): Fragment
  def addFragmentBlock(block: =>Fragment): Fragment
  def addFragmentsBlock(block: =>Fragments): Fragments
}

trait MutableHeaderBuilder {
  private[specs2] var header = new SpecHeader(specClass = getClass)
  def setTitle(t: String) = {
    header = header.copy(title = Some(t))
    header
  }
}

trait MutableArgumentsBuilder {
  private[specs2] var arguments = Arguments()
  def setArguments(a: Arguments): Arguments = { arguments = a; a }
}