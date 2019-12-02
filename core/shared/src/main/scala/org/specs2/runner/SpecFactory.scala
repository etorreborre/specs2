package org.specs2
package runner

import control._
import specification.core._

trait SpecFactory {

  def createSpecification(className: String): Operation[SpecificationStructure]
  def createLinkedSpecs(specStructure: SpecStructure): Operation[Seq[SpecStructure]]

}

object SpecFactory {

  def default: SpecFactory =
    DefaultSpecFactory(Env(), Thread.currentThread.getContextClassLoader)
    
}

case class DefaultSpecFactory(env: Env, classLoader: ClassLoader) extends SpecFactory {

  def createSpecification(className: String): Operation[SpecificationStructure] =
    SpecificationStructure.create(className, classLoader, Some(env))

  def createLinkedSpecs(specStructure: SpecStructure): Operation[Seq[SpecStructure]] =
    SpecStructure.linkedSpecifications(specStructure, env, classLoader)

}
