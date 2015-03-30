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
  def decorate(is: SpecStructure, env: Env) = map(is.map(fs => map(map(fs, env))))

  /** modify the specification structure */
  def map(structure: SpecStructure): SpecStructure = structure
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

  /**
   * sort the specifications in topological order where specification i doesn't depend on specification j if i > j
   *
   * == dependents first!
   */
  def topologicalSort(env: Env) = (specifications: Seq[SpecificationStructure]) =>
    TopologicalSort.sort(specifications, (s1: SpecificationStructure, s2: SpecificationStructure) =>
      SpecStructure.dependsOn(s2.structure(env), s1.structure(env)))

  /**
   * sort the specifications in topological order where specification i doesn't depend on specification j if i < j
   *
   *  == dependents last!
   */
  def reverseTopologicalSort(env: Env) = (specifications: Seq[SpecificationStructure]) =>
    TopologicalSort.sort(specifications, (s1: SpecificationStructure, s2: SpecificationStructure) =>
        SpecStructure.dependsOn(s1.structure(env), s2.structure(env)))

  /** @return all the referenced specifications */
  def referencedSpecifications(spec: SpecificationStructure, env: Env, classLoader: ClassLoader): Action[Seq[SpecificationStructure]] =
    specificationsRefs(spec, env, classLoader)(referencedSpecificationsClassnames)

  /** @return all the linked specifications */
  def linkedSpecifications(spec: SpecificationStructure, env: Env, classLoader: ClassLoader): Action[Seq[SpecificationStructure]] =
    specificationsRefs(spec, env, classLoader)(linkedSpecificationsClassnames)

  /** @return all the see specifications */
  def seeSpecifications(spec: SpecificationStructure, env: Env, classLoader: ClassLoader): Action[Seq[SpecificationStructure]] =
    specificationsRefs(spec, env, classLoader)(seeSpecificationsClassnames)

  /** @return all the referenced specifications */
  def specificationsRefs(spec: SpecificationStructure,
                         env: Env,
                         classLoader: ClassLoader)(refClassNames: (SpecificationStructure, Env) => List[String]): Action[Seq[SpecificationStructure]] = {

    val byName = (ss: List[SpecificationStructure]) => ss.foldLeft(Vector[(String, SpecificationStructure)]()) { (res, cur) =>
      val name = cur.structure(env).specClassName
      if (res.map(_._1).contains(name)) res
      (name, cur) +: res
    }

    def getRefs(s: SpecificationStructure, visited: Vector[(String, SpecificationStructure)]): Vector[(String, SpecificationStructure)] =
      refClassNames(s, env).map(name => create(name, classLoader)).sequenceU.map(byName).runOption.getOrElse(Vector())
        .filterNot { case (n, _) => visited.map(_._1).contains(n) }

    Actions.safe {
      def getAll(seed: Vector[SpecificationStructure], visited: Vector[(String, SpecificationStructure)]): Vector[SpecificationStructure] = {
        if (seed.isEmpty) visited.map(_._2)
        else {
          val toVisit: Vector[(String, SpecificationStructure)] = seed.flatMap(s => getRefs(s, visited))
          getAll(toVisit.map(_._2), visited ++ toVisit)
        }
      }
      val name = spec.structure(env).specClassName
      val linked = getRefs(spec, Vector((name, spec)))
      getAll(linked.map(_._2), linked :+ ((name, spec)))
    }
  }

  /** @return the class names of all the referenced specifications */
  def referencedSpecificationsClassnames(spec: SpecificationStructure, env: Env): List[String] = {
    spec.structure(env).fragments.fragments.collect(Fragment.specificationRef).map(_.header.specClass.getName).toList
  }

  /** @return the class names of all the linked specifications */
  def linkedSpecificationsClassnames(spec: SpecificationStructure, env: Env): List[String] = {
    spec.structure(env).fragments.fragments.collect(Fragment.linkReference).map(_.header.specClass.getName).toList
  }

  /** @return the class names of all the see specifications */
  def seeSpecificationsClassnames(spec: SpecificationStructure, env: Env): List[String] = {
    spec.structure(env).fragments.fragments.collect(Fragment.seeReference).map(_.header.specClass.getName).toList
  }
}

