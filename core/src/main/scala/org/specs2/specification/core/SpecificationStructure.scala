package org.specs2
package specification
package core

import data.TopologicalSort
import control._
import org.specs2.main.{CommandLine, Arguments}
import reflect.Classes
import Classes._
import scalaz.std.anyVal._
import scalaz.syntax.traverse._
import scalaz.std.list._

trait ContextualSpecificationStructure {
  def structure: Env => SpecStructure
  def fragments = (env: Env) => structure(env).fragments
}

trait SpecificationStructure extends ContextualSpecificationStructure {
  def is: SpecStructure
  def structure = (env: Env) => decorate(is, env)
  def decorate(is: SpecStructure, env: Env) = is.map(fs => map(map(fs, env)))

  /** modify the fragments */
  def map(fs: =>Fragments): Fragments = fs
  /** modify the fragments, using the current environment */
  def map(fs: =>Fragments, env: Env): Fragments = fs
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
    val byName = (ss: List[SpecificationStructure]) => ss.groupBy(_.structure(env).specClassName).mapValues(_.head)

    def getLinked(s: SpecificationStructure, visited: Map[String, SpecificationStructure]): Map[String, SpecificationStructure] =
      linkedSpecificationsClassnames(s, env).map(name => create(name, classLoader)).sequenceU.map(byName).runOption.getOrElse(Map())
        .filterNot { case (n, _) => visited.keys.toSeq.contains(n) }

    Actions.safe {
      def getAll(seed: Seq[SpecificationStructure], visited: Map[String, SpecificationStructure]): Seq[SpecificationStructure] = {
        if (seed.isEmpty) visited.values.toSeq
        else {
          val toVisit: Map[String, SpecificationStructure] = Map(seed.flatMap { s => getLinked(s, visited) }:_*)
          getAll(toVisit.values.toSeq, visited ++ toVisit)
        }
      }
      val name = spec.structure(env).specClassName
      val linked = getLinked(spec, Map(name -> spec))
      getAll(linked.values.toSeq, Map(linked.toSeq :+ (name -> spec):_*))
    }

  }

  /** @return the class names of all the linked specifications */
  def linkedSpecificationsClassnames(spec: SpecificationStructure, env: Env): List[String] = {
    spec.structure(env).fragments.fragments.collect(Fragment.specificationLink).map(_.header.specClass.getName).toList
  }
}

