package org.specs2
package specification
package core

import data.TopologicalSort
import control._
import reflect.Classes
import Classes._
import scalaz.std.anyVal._
import scalaz.syntax.traverse._
import scalaz.std.list._

trait SpecificationStructure {
  def is: SpecStructure
  def structure = (env: Env) => is.withPreviousResults(env).map(fs => map(fs))
  def fragments = (env: Env) => structure(env).fragments
  def map(fs: =>Fragments): Fragments = fs
}

object SpecificationStructure {

  def create(className: String, classLoader: ClassLoader = Thread.currentThread.getContextClassLoader): Action[SpecificationStructure] = {
    // try to create the specification from a class name, without displaying possible errors
    createInstance[SpecificationStructure](className, classLoader)
      // try to create the specification from an object class name
      .orElse(createInstance[SpecificationStructure](className+"$", classLoader))
  }

  /** sort the specifications in topological order where specification i doesn't depend on specification j if i < j */
  def topologicalSort(env: Env) = { specifications: Seq[SpecificationStructure] =>
    TopologicalSort.sort(specifications,
      (s1: SpecificationStructure, s2: SpecificationStructure) => SpecStructure.dependsOn(s1.structure(env), s2.structure(env)))
  }

  /** @return all the linked specifications */
  def linkedSpecifications(spec: SpecificationStructure, env: Env, classLoader: ClassLoader): Action[Seq[SpecificationStructure]] = {
    linkedSpecificationsClassnames(spec, env).map(name => create(name, classLoader)).sequenceU
  }

  /** @return the class names of all the linked specifications */
  def linkedSpecificationsClassnames(spec: SpecificationStructure, env: Env): List[String] = {
    spec.structure(env).fragments.fragments.collect(Fragment.specificationLink).map(_.header.specClass.getName).toList
  }
}

