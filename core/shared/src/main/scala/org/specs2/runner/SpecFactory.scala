package org.specs2
package runner

import control.*
import specification.core.*

trait SpecFactory:

  def createSpecification(className: String): Operation[SpecificationStructure]
  def createLinkedSpecs(specStructure: SpecStructure): Operation[Seq[SpecStructure]]

object SpecFactory:

  def create(env: Env): SpecFactory =
    DefaultSpecFactory(env, Thread.currentThread.getContextClassLoader)

case class DefaultSpecFactory(env: Env, classLoader: ClassLoader) extends SpecFactory:

  def createSpecification(className: String): Operation[SpecificationStructure] =
    SpecificationStructure.create(className, classLoader, Some(env))

  def createLinkedSpecs(specStructure: SpecStructure): Operation[Seq[SpecStructure]] =
    SpecStructure.linkedSpecifications(specStructure, env, classLoader)
